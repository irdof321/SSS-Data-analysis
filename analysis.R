####################################################################
#     Libraries
####################################################################
library(lubridate)
library(dplyr)
library(tidyr)

library(ggplot2)
library(forcats)
library(scales)
library(readr)
library(ggrepel)


####################################################################
#      CONSTANTS PARAMETERS
####################################################################
#data_file <- "sample_survey_results.csv"
#data_file <- "simulated_sample_survey_results_100.csv" #"With some categories missing"
data_file <- "simulated_sample_survey_results_300.csv"
REMOVE_NOT_SUB = FALSE # remove rows without a submitted date




####################################################################
#   Preliminaries
####################################################################
raw_data <- read.csv(data_file, header = TRUE, sep=";")
clean_data <- raw_data[ , 0, drop = FALSE]

## Convert data into usable variables

### DATES

clean_data$submitdate = as.Date(raw_data$submitdate)
clean_data$datestamp = as.Date(raw_data$datestamp)
clean_data$startdate = as.Date(raw_data$startdate)

### NUMERICS

clean_data$dmbirth = strtoi(raw_data$dmbirth)
clean_data$tryear = strtoi(raw_data$tryear)

ref_year <- 2026
clean_data$age <- ifelse(is.na(clean_data$dmbirth), NA_integer_, ref_year - clean_data$dmbirth)

### LOGICALS

clean_data$sssknow <- as.logical(raw_data$sssknow)

### FACTORS

#### Common factors definition
origin_levels <- c(
  "Swiss",
  "Europe",
  "North America",
  "South and Central America",
  "Middle East",
  "Africa",
  "Asia"
)
continent_levels <- origin_levels[-1]  # everyything but "Swiss"


origin_from_swiss <- function(dmswiss, dmnatio) {
  to_int <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    suppressWarnings(as.integer(x))
  }
  
  swiss_code <- to_int(dmswiss)   # 1=Swiss, 2=Not Swiss, NA possible
  cont_code  <- to_int(dmnatio)   # 1..6, NA possible
  
  out <- rep(NA_character_, length(swiss_code))
  
  # Swiss
  out[!is.na(swiss_code) & swiss_code == 1] <- "Swiss"
  
  # Not Swiss AND continent code valid
  idx <- !is.na(swiss_code) & swiss_code == 2
  idx_valid_cont <- idx & !is.na(cont_code) & cont_code >= 1 & cont_code <= length(continent_levels)
  
  out[idx_valid_cont] <- continent_levels[cont_code[idx_valid_cont]]
  
  factor(out, levels = origin_levels)
}

#### gender
gender_levels <- c("Man", "Woman", "Other", "Prefer not to say")

clean_data$dmgender <- factor(
  gender_levels[as.integer(raw_data$dmgender)],
  levels = gender_levels
)

#### origin

clean_data$origin <- origin_from_swiss(raw_data$dmswiss, raw_data$dmnatio)

#### residency
residency_levels <- c(
                      "Not in Switzerland",
                      "AG",
                      "AR",
                      "AI",
                      "BL",
                      "BS",
                      "BE",
                      "FR",
                      "GE",
                      "GL",
                      "GR",
                      "JU",
                      "LU",
                      "NE",
                      "NW",
                      "OW",
                      "SH",
                      "SZ",
                      "SO",
                      "SG",
                      "TG",
                      "TI",
                      "UR",
                      "VS",
                      "VD",
                      "ZG",
                      "ZH"
                      )
clean_data$dmres <- factor(residency_levels[as.integer(raw_data$dmres)], levels = residency_levels)

#### work location
residency_levels <- c(residency_levels, "I do not work")
clean_data$dmwork <- factor(residency_levels[as.integer(raw_data$dmwork)], levels = residency_levels)

#### sss involvement
involvement_level <- c(
                        "Not a member",
                        "Passive",
                        "Occasional",
                        "Active",
                        "Volunteer"
                      )
clean_data$sssmember <- factor(involvement_level[as.integer(raw_data$sssmember)],levels = involvement_level)


#### sss time
time_sss_level <- c(
                    "Less than one year",
                    "Less than five years",
                    "Less than ten years",
                    "Ten years or more"
                  )

clean_data$ssstime <- factor(time_sss_level[as.integer(raw_data$ssstime)], levels = time_sss_level)

#### Education

education_level <- c(
                      "Bachelor of applied science",
                      "University bachelor",
                      "Master of applied science",
                      "University master",
                      "PhD",
                      "Other"
                    )

clean_data$trlvl <- factor(education_level[as.integer(raw_data$trlvl)], levels = education_level)
clean_data$study_location <- origin_from_swiss(raw_data$trcontswiss,raw_data$trreg)

#### Training field

training_field_study <- c(
  "Theology",
  "Law",
  "Science of economics",
  "Health, sport",
  "Psychology",
  "Sociology",
  "Other social sciences",
  "Language, literature",
  "History, civilizations study",
  "Art, music, design",
  "Mathematics",
  "Informatics / Computer science",
  "Statistics",
  "Data science",
  "Applied statistics",
  "Natural science, environmental science",
  "Technical science, engineering",
  "Education",
  "Other"
)


cols <- paste0("trarea.", 1:19, ".")

to_int01 <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- suppressWarnings(as.integer(x))
  x[is.na(x) | !(x %in% c(0L, 1L))] <- 0L
  x
}

# Clean -> 0/1 bitmap (n x 19)
training_fields_bitmap <- as.matrix(data.frame(lapply(raw_data[, cols, drop = FALSE], to_int01)))
storage.mode(training_fields_bitmap) <- "integer"

# Free-text "Other" column (adjust name if needed)
other_col <- "trarea.other"   # <-- change to your real column name
other_text <- trimws(as.character(raw_data[[other_col]]))
other_text[is.na(other_text) | other_text == ""] <- NA_character_

# List-column (vector per row)
clean_data$training_fields_list <- lapply(seq_len(nrow(training_fields_bitmap)), function(i) {
  r <- training_fields_bitmap[i, ]
  sel <- training_field_study[r == 1L]
  
  # if "Other" selected (bit 19), append the free text (if available)
  if (r[19] == 1L && !is.na(other_text[i])) {
    sel <- c(sel, other_text[i])
  }
  
  sel
})

#### Continuous education

continuous_education_levels <- c(
                                  "No",
                                  "MAS, DAS, CAS",
                                  "Certified online training (Coursera, Edx, etc.)",
                                  "Postgraduate in Business/Finance (MBA, EMBA, etc.)",
                                  "Post-Doc",
                                  "Further training with an employer"
                                )

# Columns trcont.1. ... trcont.6.
cont_cols <- paste0("trcont.", 1:6, ".")

# Clean -> 0/1 matrix (n x 6)
cont_bitmap <- as.matrix(data.frame(lapply(raw_data[, cont_cols, drop = FALSE], to_int01)))
storage.mode(cont_bitmap) <- "integer"

# If row is empty (all zeros), assume "No" => force trcont.1. = 1
empty_row <- rowSums(cont_bitmap) == 0L
cont_bitmap[empty_row, 1] <- 1L

# Optional: derive the selected label (NA if multiple selected)
clean_data$continuous_education <- apply(cont_bitmap, 1, function(r) {
  idx <- which(r == 1L)
  if (length(idx) == 1) continuous_education_levels[idx] else NA_character_
})

# Derived boolean: trcont2. = TRUE if any option 2..6 is selected, else FALSE
clean_data$trcont2 <- rowSums(cont_bitmap[, 2:6, drop = FALSE]) > 0L

#### Employed
clean_data$employed <- as.logical(raw_data$plemployed)

#### Employ status

employment_status_level <- c(
                              "Employed",
                              "Self-employed",
                              "Student",
                              "Unemployed",
                              "Retired"
                            )

clean_data$job_status <- ifelse(
  raw_data$plemployed == 1,
  "Employed",
  employment_status_level[ raw_data$plstatus - 1 ]  # 2->1, 3->2, ..., 5->4
)

#### job title
# 1) plrole to string
raw_data$plrole <- as.character(raw_data$plrole)

# 2) Job role creation, but if student previously keep it
clean_data$job_role <- ifelse(
  clean_data$job_status == "Student",
  "Student",
  raw_data$plrole
)

# Cleaaning
clean_data$job_role <- trimws(clean_data$job_role)
clean_data$job_role[clean_data$job_role == ""] <- NA

#### Sector

sector_job_level <- c(
  "Banking / Finance / Insurance",
  "Luxury goods",
  "IT/ Telecommunicatins industry",
  "Consumer goods",
  "Audit/ Consulting/ Professional service",
  "Automotive",
  "Aviation/ Aerospace/ Defense",
  "Chemicals/ Ingredients",
  "Electrical / Electronics / Semiconductors",
  "Government / Public administration",
  "Machinery and Equipment / Automation",
  "Materials",
  "Pharmaceuticals",
  "Real estate",
  "Transportation/ Rail",
  "Watchmaking",
  "Biotechnology/ Bioengineering",
  "Construction/ Civil engineering",
  "Engineering consulting",
  "Hospital/ Healthcare",
  "Logistics/ Suplly chain industry",
  "Media / Advertising / Communication",
  "Medical technologies and devices",
  "Nonprofit organization / Social",
  "Oil and gas / Energy",
  "Primary or Secondary Education",
  "Architecture / Urban planning",
  "Higher education / Research / Academia",
  "Renewables / Environment",
  "Other",
  "None"
)

clean_data$plsector <- factor(sector_job_level[as.integer(raw_data$plsector)], levels = sector_job_level)

#### Employment xp

clean_data$plyexp <- as.integer(raw_data$plyexp)

#### Employment rate

clean_data$plrate <- as.integer(raw_data$plrate)

#### Seniority level

seniority_level_levels <- c(
                            "Intern / Entry level position",
                            "No managerial function",
                            "Lower management",
                            "Middle management",
                            "Top management",
                            "Never worked"
                            )
clean_data$plsenior <- factor(seniority_level_levels[as.integer(raw_data$plsenior)], levels = seniority_level_levels)


#### skills

skills_levels <- c(
                    "Statistical programming (R, SAS, Python (Statistics and ML libraries), SPSS, Stata, etc.)",
                    "Other programming (C, C++, Java, Python, etc.)",
                    "Data visualization (Power BI, Tableau, Looker Studio, etc.)",
                    "Scientific writing and/or research",
                    "Project management",
                    "Time management"
                  )

skill_cols <- paste0("plskill.", 1:6, ".")

# matrice 0/1 
skill_mat <- as.data.frame(lapply(raw_data[skill_cols], function(x) as.integer(as.character(x))))

# 
clean_data$skills <- lapply(seq_len(nrow(skill_mat)), function(i) {
  idx <- which(skill_mat[i, ] == 1)
  skills_levels[idx]
})

clean_data$skills_str <- vapply(clean_data$skills, function(x) {
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}, character(1))


#### skills importance

theme_levels <- c(
  "Data cleaning and preparation",
  "Descriptive analysis",
  "Inferential analysis",
  "Modeling / Machine learning",
  "Development or automation of statistical tools",
  "Supervision or validation of statistical work carried out by others"
)

importance_levels <- c(
  "Not at all important", "Slightly important", "Moderately important",
  "Important", "Very important"
)

involvement_levels <- c(
  "No use", "Direct practice", "Supervision", "Direct practice and supervision"
)

# colonnes à prendre
ustime_cols <- grep("^ustime\\.[1-6]\\.\\.[1-2]\\.$", names(raw_data), value = TRUE)

# pour chaque ligne, construit une petite table tidy 6x2
ustime_list <- lapply(seq_len(nrow(raw_data)), function(i) {
  tmp <- raw_data[i, ustime_cols, drop = FALSE]
  
  long <- tidyr::pivot_longer(
    tmp,
    cols = everything(),
    names_to = c("theme_id", "scale_id"),
    names_pattern = "^ustime\\.(\\d+)\\.\\.(\\d+)\\.$",
    values_to = "value"
  ) %>%
    mutate(
      theme_id = as.integer(theme_id),
      scale_id = as.integer(scale_id)
    ) %>%
    pivot_wider(
      names_from = scale_id,
      values_from = value,
      names_prefix = "scale_"
    ) %>%
    transmute(
      theme_id,
      theme = theme_levels[theme_id],
      importance_code = scale_1,
      involvement_code = scale_2,
      importance = ifelse(is.na(importance_code), NA_character_, importance_levels[importance_code + 1]),
      involvement = ifelse(is.na(involvement_code), NA_character_, involvement_levels[involvement_code + 1])
    )
  
  long
})

clean_data$ustime <- ustime_list


#### work salary

x <- as.character(raw_data$issalary)
x <- gsub("'", "", x)          # remove tausend separator 1'234
x <- gsub(" ", "", x)          # remove spaces
x <- gsub(",", ".", x)         # comma to point
clean_data$salary <- suppressWarnings(as.numeric(x))/clean_data$plrate*100 # report salary to an 100%

#### work satisfactrion

work_satisfaction_levels <- c(
                              "Very satisfied",
                              "Quite satisfied",
                              "Neutral",
                              "Not quite satisfied",
                              "Not at all satisfied"
                              )

clean_data$worksatisfction <- factor(work_satisfaction_levels[as.integer(raw_data$issatisf)], levels = work_satisfaction_levels)




satisf_levels <- c(
  "Very satisfied",
  "Somewhat satisfied",
  "Neutral",
  "Not so satisfied",
  "Not at all satisfied"
)

# Mets ici les 12 textes (dans l’ordre 1..12)
satisf_items <- c(
  "Interesting and meaningful work",
  "Opportunity to exercise job-related expertise and judgment",
  "Work that makes a positive contribution",
  "Pay",
  "Benefits (e.g., leave, health, insurance, retirement benefits)",
  "Learning and development opportunities (e.g., training, continuing ...)",
  "Opportunity for advancement",
  "Work-life balance",
  "Work flexibility (e.g., telework, alternative work schedules, core hours)",
  "Relationships with coworkers and supervisors",
  "Recognition and appreciation",
  "Manageability of job stress"
)

issatisf_cols <- grep("^issatisf2\\.[0-9]+\\.$", names(raw_data), value = TRUE)

# Convertit tout en numérique (en gardant NA)
issatisf_mat <- as.data.frame(lapply(raw_data[issatisf_cols], function(x) {
  suppressWarnings(as.integer(as.character(x)))
}))

clean_data$issatisf2 <- lapply(seq_len(nrow(issatisf_mat)), function(i) {
  codes <- as.integer(issatisf_mat[i, ])
  data.frame(
    item_id = seq_along(codes),
    item = if (length(satisf_items) >= length(codes)) satisf_items[seq_along(codes)] else NA_character_,
    code = codes,
    label = ifelse(is.na(codes), NA_character_, satisf_levels[codes]),
    stringsAsFactors = FALSE
  )
})

####################################################################
#   Derived variables
####################################################################




####################################################################
#   Study variables
####################################################################



####################################################################
#   Results analysis
####################################################################
# 0) dossier de sortie
out_dir <- "descriptives_plots"
if (!dir.exists(out_dir)) dir.create(out_dir)


readr::write_csv(clean_data, "my_df.csv")



#5.4.0 Basic datas

############################### ADD down here

# ---- Style (colors) ----
my_fill <- "#2C7FB8"   # blue
my_border <- NA        # set to "white" if you want a thin border

# Helper: save a bar plot of counts (single variable)
# NOTE: title is now "presentation title" ONLY (no variable name inside)
save_barplot_counts <- function(df, xvar, title, filename,
                                xlab = NULL, rotate_x = TRUE) {
  p <- ggplot(df, aes(x = .data[[xvar]])) +
    geom_bar(fill = my_fill, color = my_border) +
    labs(
      title = title,
      x = xlab,
      y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
  
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  print(p)
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = 12, height = 6, dpi = 300
  )
}

# Helper: numeric binning plot (counts)
# Helper: numeric binning plot (counts) - robust labels (no scientific notation)
save_binned_counts <- function(df, numvar, breaks, title, filename, xlab = NULL,
                               label_mode = c("default", "year", "k")) {
  label_mode <- match.arg(label_mode)
  
  # Custom labels built from numeric breaks (NOT from cut() default strings)
  if (label_mode == "year") {
    # 1950–1959
    bin_labels <- paste0(breaks[-length(breaks)], "\u2013", breaks[-1] - 1)
  } else if (label_mode == "k") {
    # 50k–59k (for CHF)
    lo <- breaks[-length(breaks)] / 1000
    hi <- (breaks[-1] - 1) / 1000
    bin_labels <- paste0(lo, "k\u2013", floor(hi), "k")
  } else {
    # default numeric labels without sci notation
    bin_labels <- format(breaks[-length(breaks)], scientific = FALSE, trim = TRUE)
  }
  
  tmp <- df %>%
    filter(!is.na(.data[[numvar]])) %>%
    mutate(
      bin = cut(
        .data[[numvar]],
        breaks = breaks,
        right = FALSE,
        include.lowest = TRUE,
        labels = bin_labels
      )
    )
  
  p <- ggplot(tmp, aes(x = bin)) +
    geom_bar(fill = my_fill, color = my_border) +
    labs(
      title = title,
      x = xlab,
      y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(p)
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = 12, height = 6, dpi = 300
  )
}


# Helper: Top N categories + Other
save_topn_barplot <- function(df, xvar, n_top, title, filename, xlab = NULL) {
  tmp <- df %>%
    filter(!is.na(.data[[xvar]]), .data[[xvar]] != "") %>%
    count(.data[[xvar]], name = "n") %>%
    arrange(desc(n)) %>%
    mutate(
      rank = row_number(),
      group = ifelse(rank <= n_top, as.character(.data[[xvar]]), "Other")
    ) %>%
    count(group, wt = n, name = "n") %>%
    mutate(group = forcats::fct_reorder(group, n))
  
  p <- ggplot(tmp, aes(x = group, y = n)) +
    geom_col(fill = my_fill, color = my_border) +
    coord_flip() +
    labs(title = title, x = xlab, y = "N respondents") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
  
  print(p)
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = 12, height = 7, dpi = 300
  )
}

# Helper: donut plot with legend on the side (stable palette + fixed legend key size)
save_donut_counts <- function(df, xvar, title, filename,
                              palette = NULL,
                              wrap_width = NULL,
                              drop_na = TRUE,
                              legend_show_pct = TRUE) {
  
  dd <- df %>%
    mutate(cat = as.character(.data[[xvar]])) %>%
    { if (drop_na) dplyr::filter(., !is.na(cat), cat != "") else . } %>%
    count(cat, name = "n") %>%
    mutate(pct = n / sum(n))
  
  if (nrow(dd) == 0) return(invisible(NULL))
  
  # display labels (optionally wrapped) BUT keep fill mapped to raw cat
  cat_disp <- dd$cat
  if (!is.null(wrap_width)) cat_disp <- stringr::str_wrap(cat_disp, width = wrap_width)
  
  if (legend_show_pct) {
    dd$cat_lab <- paste0(cat_disp, " — ", dd$n, " (", scales::percent(dd$pct, accuracy = 1), ")")
  } else {
    dd$cat_lab <- cat_disp
  }
  
  # IMPORTANT: breaks must be raw categories in the same order as dd
  breaks_cat <- dd$cat
  labels_map <- setNames(dd$cat_lab, dd$cat)
  
  p <- ggplot(dd, aes(x = 2, y = n, fill = cat)) +
    geom_col(width = 0.9, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.6) +
    labs(title = title, fill = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.key = element_rect(color = NA),
      legend.key.height = unit(5, "mm"),
      legend.key.width  = unit(5, "mm"),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    guides(fill = guide_legend(
      keyheight = unit(5, "mm"),
      keywidth  = unit(5, "mm"),
      override.aes = list(alpha = 1)
    ))
  
  if (!is.null(palette)) {
    p <- p + scale_fill_manual(
      values = palette,
      breaks = breaks_cat,
      labels = labels_map[breaks_cat],
      drop = FALSE
    )
  } else {
    p <- p + scale_fill_discrete(
      breaks = breaks_cat,
      labels = labels_map[breaks_cat],
      drop = FALSE
    )
  }
  
  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
}

# Histogram helper
save_hist_counts <- function(df, numvar, title, filename,
                             binwidth = NULL, bins = 30, xlab = NULL) {
  
  tmp <- df %>% filter(!is.na(.data[[numvar]]))
  
  if (nrow(tmp) == 0) return(invisible(NULL))
  
  p <- ggplot(tmp, aes(x = .data[[numvar]])) +
    { if (!is.null(binwidth)) geom_histogram(binwidth = binwidth, fill = my_fill, color = my_border)
      else geom_histogram(bins = bins, fill = my_fill, color = my_border) } +
    labs(title = title, x = xlab, y = "N respondents") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
}




# =========================
# 1) Birth year (10-year bins)
# =========================
# =========================
# Age (10-year bins)
# =========================
if (any(!is.na(clean_data$age))) {
  min_a <- floor(min(clean_data$age, na.rm = TRUE) / 10) * 10
  max_a <- ceiling(max(clean_data$age, na.rm = TRUE) / 10) * 10 + 10
  age_breaks <- seq(min_a, max_a, by = 10)
  
  save_binned_counts(
    clean_data,
    numvar = "age",
    breaks = age_breaks,
    title = "Age (10-year bins)",
    filename = "basic_age_10y.png",
    xlab = "Age bin",
    label_mode = "default"
  )
}
# =========================
# 2) Training completion year (5-year bins)
# =========================
#Training completion year (tryear): histogram (or keep your 5y bins if you prefer)
save_hist_counts(clean_data, "tryear",
                 "Training completion year",
                 "basic_trainingyear_hist.png",
                 binwidth = 1, xlab = "Year")

# 3) SSS awareness
save_barplot_counts(clean_data, "sssknow", "SSS awareness", "basic_sss_awareness.png",
                    xlab = NULL, rotate_x = FALSE)

save_donut_counts(
  clean_data, "sssknow",
  title = "SSS awareness",
  filename = "donut_sss_awareness.png",
  palette = c("FALSE" = "#D55E00", "TRUE" = "#009E73"),
  drop_na = TRUE
)




# 5) Origin
save_barplot_counts(clean_data, "origin", "Origin", "basic_origin.png",
                    xlab = NULL)

# 6) Residency (canton)
save_barplot_counts(clean_data, "dmres", "Residency (canton)", "basic_residency.png",
                    xlab = NULL)

# 7) Work location
save_barplot_counts(clean_data, "dmwork", "Work location", "basic_work_location.png",
                    xlab = NULL)

# 8) SSS involvement
save_barplot_counts(clean_data, "sssmember", "SSS involvement", "basic_sss_involvement.png",
                    xlab = NULL)

# 8b) SSS time
save_barplot_counts(clean_data, "ssstime", "SSS membership duration",
                    "basic_sss_time.png", xlab = NULL)

save_donut_counts(
  clean_data, "ssstime",
  title = "SSS membership duration",
  filename = "donut_sss_time.png",
  drop_na = TRUE
)

# SSS time × involvement (heatmap de counts)
df_sss <- clean_data %>%
  filter(!is.na(ssstime), !is.na(sssmember)) %>%
  count(ssstime, sssmember, name = "n")

#OPTIONAL
p_sss_cross <- ggplot(df_sss, aes(x = sssmember, y = ssstime, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3.5, color = "white") +
  scale_fill_gradient(low = "#1a2a3a", high = my_fill) +
  labs(title = "SSS time × involvement", x = NULL, y = NULL, fill = "N") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p_sss_cross)
ggsave(file.path(out_dir, "sss_time_by_involvement.png"),
       p_sss_cross, width = 10, height = 5, dpi = 300)

# 9) Education level
save_barplot_counts(clean_data, "trlvl", "Education level", "basic_education_level.png",
                    xlab = NULL)

# 10) Study location
save_barplot_counts(clean_data, "study_location", "Study location", "basic_study_location.png",
                    xlab = NULL)

# 11) Training fields (multiple answers allowed)
df_tf <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "") %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

p_tf <- df_tf %>%
  count(training_field, name = "n") %>%
  ggplot(aes(x = training_field, y = n)) +
  geom_col(fill = my_fill, color = my_border) +
  labs(
    title = "Training fields (multiple answers allowed)",
    x = NULL,
    y = "N respondents (selections)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_tf)
ggsave(
  filename = file.path(out_dir, "basic_training_fields.png"),
  plot = p_tf,
  width = 14, height = 7, dpi = 300
)

# 12) Continuous education
clean_data$continuous_education <- fct_relabel(
  clean_data$continuous_education,
  ~ stringr::str_wrap(.x, width = 28)
)
save_barplot_counts(clean_data, "continuous_education", "Continuous education",
                    "basic_continuous_education.png", xlab = NULL)

# 13) Continuous education beyond 'No'
save_barplot_counts(clean_data, "trcont2", "Continuous education beyond 'No'",
                    "basic_continuous_education_yesno.png", xlab = NULL, rotate_x = FALSE)

save_donut_counts(
  clean_data, "continuous_education",
  title = "Continuous education",
  filename = "donut_continuous_education.png",
  wrap_width = 26,
  drop_na = TRUE
)


# 14) Employment status (employed)
save_barplot_counts(clean_data, "employed", "Employment status",
                    "basic_employment_status.png", xlab = NULL, rotate_x = FALSE)

save_donut_counts(
  clean_data, "employed",
  title = "Employment status",
  filename = "donut_employment_status.png",
  palette = c("FALSE" = "#D55E00", "TRUE" = "#009E73"),
  drop_na = TRUE
)


# 15) Job status
save_barplot_counts(clean_data, "job_status", "Job status",
                    "basic_job_status.png", xlab = NULL)

save_donut_counts(
  clean_data, "job_status",
  title = "Job status",
  filename = "donut_job_status.png",
  wrap_width = 18,
  drop_na = TRUE
)


# 16) Job role (Top 20 + Other)
save_topn_barplot(clean_data, "job_role", n_top = 20,
                  title = "Job role (Top 20 + Other)",
                  filename = "basic_job_role_top20.png",
                  xlab = NULL)

# 17) Sector (drop NA + wrap long labels + bigger export)
p_sector <- clean_data %>%
  filter(!is.na(plsector)) %>%                              # remove NA category
  mutate(plsector_wrap = stringr::str_wrap(as.character(plsector), width = 22)) %>%
  ggplot(aes(x = plsector_wrap)) +
  geom_bar(fill = my_fill, color = my_border) +
  labs(title = "Sector", x = NULL, y = "N respondents") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_sector)
ggsave(
  filename = file.path(out_dir, "basic_sector.png"),
  plot = p_sector,
  width = 18, height = 9, dpi = 300
)


# 18) # Years of experience (plyexp): often integer years
save_hist_counts(clean_data, "plyexp",
                 "Years of professional experience in current field",
                 "basic_years_experience_hist.png",
                 binwidth = 1, xlab = "Years")

# 19)  Work percent (plrate): usually 0–100, step 10
save_hist_counts(clean_data, "plrate",
                 "Employment rate (workload %)",
                 "basic_employment_rate_hist.png",
                 binwidth = 10, xlab = "Workload (%)")

# 20) Seniority level
save_barplot_counts(clean_data, "plsenior", "Seniority level",
                    "basic_seniority.png", xlab = NULL)

# 21) Salary (10k bins)
if (any(!is.na(clean_data$salary))) {
  min_s <- floor(min(clean_data$salary, na.rm = TRUE) / 10000) * 10000
  max_s <- ceiling(max(clean_data$salary, na.rm = TRUE) / 10000) * 10000 + 10000
  salary_breaks <- seq(min_s, max_s, by = 10000)
  
  save_binned_counts(
    clean_data,
    numvar = "salary",
    breaks = salary_breaks,
    title = "Salary (normalized to 100% workload, 10k bins)",
    filename = "basic_salary_10k.png",
    xlab = "Salary bin (CHF)",
    label_mode = "k"
  )
}

# 22) Work satisfaction
save_barplot_counts(clean_data, "worksatisfction", "Work satisfaction",
                    "basic_work_satisfaction.png", xlab = NULL)

# 23) Work satisfaction details
# ── Satisfaction détaillée (issatisf2) ──

# 1) Unnest
df_satisf <- clean_data %>%
  tidyr::unnest(issatisf2) %>%
  filter(!is.na(code), !is.na(item)) %>%
  mutate(
    label = factor(label, levels = satisf_levels),
    item  = stringr::str_wrap(item, width = 38)
  )

# 2) Counts + % par item
df_satisf_pct <- df_satisf %>%
  count(item, label, name = "n") %>%
  group_by(item) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# 3) Diverging : on sépare positif / négatif
# Positif = Very/Somewhat satisfied  |  Négatif = Not so / Not at all satisfied
# Neutral reste au centre

positive_labels <- c("Very satisfied", "Somewhat satisfied")
negative_labels <- c("Not at all satisfied", "Not so satisfied")

df_div <- df_satisf_pct %>%
  mutate(
    side = case_when(
      label %in% positive_labels ~ "positive",
      label %in% negative_labels ~ "negative",
      TRUE ~ "neutral"
    ),
    pct_directed = case_when(
      side == "negative" ~ -pct,
      side == "neutral"  ~  pct / 2,  # neutral partagé des deux côtés
      TRUE               ~  pct
    )
  )

# 4) Palette Likert
likert_palette <- c(
  "Very satisfied"      = "#1a7a4a",
  "Somewhat satisfied"  = "#6dbe8d",
  "Neutral"             = "#b0b8c1",
  "Not so satisfied"    = "#e08060",
  "Not at all satisfied"= "#c0392b"
)

# 5) Ordre des items par % de satisfied (du plus au moins satisfait)
item_order <- df_satisf_pct %>%
  filter(label %in% positive_labels) %>%
  group_by(item) %>%
  summarise(pos_pct = sum(pct)) %>%
  arrange(pos_pct) %>%
  pull(item)

df_div <- df_div %>%
  mutate(item = factor(item, levels = item_order))

# 6) Plot
p_satisf2 <- ggplot(df_div, aes(x = pct_directed, y = item, fill = label)) +
  geom_col(position = "stack", width = 0.7) +
  geom_vline(xintercept = 0, color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = likert_palette,
    breaks = satisf_levels,   # ordre dans la légende
    drop   = FALSE
  ) +
  scale_x_continuous(
    labels = function(x) paste0(abs(round(x * 100)), "%"),
    limits = c(-1, 1)
  ) +
  labs(
    title = "Job satisfaction — detailed items",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

print(p_satisf2)
ggsave(
  file.path(out_dir, "satisf2_diverging.png"),
  p_satisf2, width = 13, height = 7, dpi = 300
)

# ── Compétences (skills) ──

# 1) Unnest
df_skills <- clean_data %>%
  tidyr::unnest_longer(skills, values_to = "skill") %>%
  filter(!is.na(skill), skill != "") %>%
  mutate(skill = factor(skill, levels = skills_levels))

# 2) Bar chart fréquences (labels wrappés car ils sont longs)
p_skills <- df_skills %>%
  count(skill, name = "n") %>%
  mutate(skill = forcats::fct_reorder(stringr::str_wrap(as.character(skill), 28), n)) %>%
  ggplot(aes(x = n, y = skill)) +
  geom_col(fill = my_fill, color = my_border) +
  labs(
    title = "Work-related skills (multiple answers allowed)",
    x = "N respondents", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_skills)
ggsave(file.path(out_dir, "basic_skills.png"), p_skills, width = 11, height = 6, dpi = 300)

# 3) Co-occurrence — quelles compétences vont ensemble
skill_wide <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest_longer(skills, values_to = "skill") %>%
  filter(!is.na(skill)) %>%
  mutate(val = 1,
         skill_short = case_when(
           grepl("Statistical", skill)  ~ "Stat. prog.",
           grepl("Other prog", skill)   ~ "Other prog.",
           grepl("visualization", skill)~ "DataViz",
           grepl("writing", skill)      ~ "Sci. writing",
           grepl("Project", skill)      ~ "Project mgmt",
           grepl("Time", skill)         ~ "Time mgmt"
         )) %>%
  select(id, skill_short, val) %>%
  tidyr::pivot_wider(names_from = skill_short, values_from = val, values_fill = 0)

skill_mat <- as.matrix(skill_wide[, -1])
cooc <- t(skill_mat) %*% skill_mat  # co-occurrence matrix
diag(cooc) <- NA  # masquer la diagonale

cooc_df <- as.data.frame(as.table(cooc)) %>%
  rename(skill1 = Var1, skill2 = Var2, n = Freq) %>%
  filter(!is.na(n))

p_cooc <- ggplot(cooc_df, aes(x = skill1, y = skill2, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3.5, color = "white") +
  scale_fill_gradient(low = "#1a2a3a", high = my_fill, na.value = "grey20") +
  labs(title = "Skills co-occurrence", x = NULL, y = NULL, fill = "N") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid = element_blank()
  )

print(p_cooc)
ggsave(file.path(out_dir, "skills_cooccurrence.png"), p_cooc, width = 9, height = 7, dpi = 300)

# ── Setup commun ──
df_us <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code), !is.na(involvement_code)) %>%
  mutate(
    theme = factor(theme, levels = theme_levels),
    importance = factor(importance, levels = importance_levels),
    involvement = factor(involvement, levels = involvement_levels)
  )

# ── Graphique 1 : Importance — stacked bar 100% par thème ──
# Chaque barre = un thème, divisée par les 5 niveaux d'importance
# Triée du plus important au moins important

df_imp <- df_us %>%
  count(theme, importance, name = "n") %>%
  group_by(theme) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(theme = forcats::fct_reorder(
    theme,
    ifelse(importance %in% c("Important", "Very important"), pct, 0),
    sum
  ))

p_imp <- ggplot(df_imp, aes(x = pct, y = theme, fill = importance)) +
  geom_col(width = 0.7) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "Not at all important" = "#c0392b",
    "Slightly important"   = "#e09060",
    "Moderately important" = "#b0b8c1",
    "Important"            = "#6dbe8d",
    "Very important"       = "#1a7a4a"
  )) +
  labs(title = "Importance of statistical activities",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

print(p_imp)
ggsave(file.path(out_dir, "ustime_importance.png"), p_imp, width = 12, height = 6, dpi = 300)

# ── Graphique 2 : Implication — même structure ──

df_inv <- df_us %>%
  count(theme, involvement, name = "n") %>%
  group_by(theme) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(theme = forcats::fct_reorder(
    theme,
    ifelse(involvement == "No use", pct, 0),
    sum,
    .desc = TRUE   # trier : ceux qui utilisent le plus en haut
  ))

p_inv <- ggplot(df_inv, aes(x = pct, y = theme, fill = involvement)) +
  geom_col(width = 0.7) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "No use"                          = "#e8eaed",
    "Direct practice"                 = "#2C7FB8",
    "Supervision"                     = "#6dbe8d",
    "Direct practice and supervision" = "#1a4a7a"
  )) +
  labs(title = "Involvement in statistical activities",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

print(p_inv)
ggsave(file.path(out_dir, "ustime_involvement.png"), p_inv, width = 12, height = 6, dpi = 300)


# ── Graphique 3 : Vue combinée — importance moyenne vs % actifs ──
# Un point par thème — le plus utile pour le rapport

df_bubble <- df_us %>%
  group_by(theme) %>%
  summarise(
    imp_mean   = mean(importance_code, na.rm = TRUE),   # 0-4
    active_pct = mean(involvement != "No use", na.rm = TRUE)
  )

p_bubble <- ggplot(df_bubble,
                   aes(x = imp_mean, y = active_pct, label = stringr::str_wrap(as.character(theme), 25))) +
  geom_point(size = 5, color = my_fill, alpha = 0.8) +
  ggrepel::geom_label_repel(size = 3.2, fill = "white", color = "grey30",
                            box.padding = 0.6, max.overlaps = 10) +
  scale_x_continuous(
    limits = c(0, 4),
    breaks = 0:4,
    labels = c("Not at all", "Slightly", "Moderately", "Important", "Very important")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Statistical activities — importance vs active use",
    x = "Mean importance score",
    y = "% respondents actively involved"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

print(p_bubble)
ggsave(file.path(out_dir, "ustime_bubble.png"), p_bubble, width = 11, height = 7, dpi = 300)
############################## ADD above



# 5.4.1
## ADD here the plots and data for the section 5.4.1 of the protocol


# 1) Unnest the list-column: one row per (respondent x training field)
df_fields <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "") %>%
  filter(!is.na(dmgender), dmgender != "")

# Optional: enforce the field order if you have 'training_field_study' defined
# (this keeps fields ordered as in your protocol list)
df_fields <- df_fields %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

# 2) Compute percentages within each field (so bars sum to 100% per field)
df_plot <- df_fields %>%
  count(training_field, dmgender, name = "n") %>%
  group_by(training_field) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# 3) Plot: grouped bars (4 bars per field) with manual colors
p_gender_by_field <- ggplot(df_plot, aes(x = training_field, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(
    values = c(
      "Man" = "blue",
      "Woman" = "red",
      "Other" = "yellow",
      "Prefer not to say" = "green"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Gender distribution by training field",
    x = "Training field",
    y = "Share within field",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_gender_by_field)

# 4) Save (keeps your export logic)
ggsave(
  filename = file.path(out_dir, "gender_by_training_field.png"),
  plot = p_gender_by_field,
  width = 14, height = 7, dpi = 300
)

# ── Lieu d'études × Genre ──

df_loc_gender <- clean_data %>%
  filter(!is.na(study_location), !is.na(dmgender)) %>%
  count(study_location, dmgender, name = "n") %>%
  group_by(study_location) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_loc_gender <- ggplot(df_loc_gender,
                       aes(x = study_location, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge2(width = 0.85, preserve = "single"),
           width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(
    values = c("Man"             = "#2C7FB8",
               "Woman"           = "#e05252",
               "Other"           = "#e6a817",
               "Prefer not to say" = "#6dbe8d"),
    drop = FALSE
  ) +
  labs(
    title = "Gender distribution by study location",
    x = NULL, y = "Share within location", fill = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

print(p_loc_gender)
ggsave(file.path(out_dir, "gender_by_study_location.png"),
       p_loc_gender, width = 12, height = 6, dpi = 300)

# ── Niveau de diplôme × Genre ──

df_edu_gender <- clean_data %>%
  filter(!is.na(trlvl), !is.na(dmgender)) %>%
  count(trlvl, dmgender, name = "n") %>%
  group_by(trlvl) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_edu_gender <- ggplot(df_edu_gender,
                       aes(x = trlvl, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge2(width = 0.85, preserve = "single"),
           width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(
    values = c("Man"               = "#2C7FB8",
               "Woman"             = "#e05252",
               "Other"             = "#e6a817",
               "Prefer not to say" = "#6dbe8d"),
    drop = FALSE
  ) +
  labs(
    title = "Gender distribution by education level",
    x = NULL, y = "Share within level", fill = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

print(p_edu_gender)
ggsave(file.path(out_dir, "gender_by_education_level.png"),
       p_edu_gender, width = 12, height = 6, dpi = 300)

# ── Domaine de formation × Lieu d'études ──

df_field_loc <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "",
         !is.na(study_location)) %>%
  mutate(training_field = factor(training_field, levels = training_field_study)) %>%
  count(training_field, study_location, name = "n") %>%
  group_by(training_field) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_field_loc <- ggplot(df_field_loc,
                      aes(x = training_field, y = pct, fill = study_location)) +
  geom_col(position = position_stack(), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  labs(
    title = "Study location by training field",
    x = NULL, y = "Share within field", fill = "Study location"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_field_loc)
ggsave(file.path(out_dir, "study_location_by_training_field.png"),
       p_field_loc, width = 14, height = 7, dpi = 300)

# ── Séniorité × Niveau de diplôme — 3 versions ──

df_sen_edu <- clean_data %>%
  filter(!is.na(plsenior), !is.na(trlvl)) %>%
  count(trlvl, plsenior, name = "n") %>%
  group_by(trlvl) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Option 1 — Grouped bar
p_sen_edu_1 <- ggplot(df_sen_edu, aes(x = plsenior, y = pct, fill = trlvl)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Blues", drop = FALSE) +
  labs(title = "Seniority level by education level — grouped bar",
       x = NULL, y = "Share within education level", fill = "Education") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_sen_edu_1)
ggsave(file.path(out_dir, "seniority_by_education_v1_grouped.png"),
       p_sen_edu_1, width = 13, height = 7, dpi = 300)

# Option 2 — Stacked bar 100%
p_sen_edu_2 <- ggplot(df_sen_edu, aes(x = trlvl, y = pct, fill = plsenior)) +
  geom_col(width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, drop = FALSE) +
  labs(title = "Seniority level by education level — stacked bar",
       x = NULL, y = "Share within education level", fill = "Seniority") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_sen_edu_2)
ggsave(file.path(out_dir, "seniority_by_education_v2_stacked.png"),
       p_sen_edu_2, width = 13, height = 7, dpi = 300)

# Option 3 — Facet par diplôme
p_sen_edu_3 <- ggplot(df_sen_edu, aes(x = plsenior, y = pct)) +
  geom_col(fill = my_fill, width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ trlvl, nrow = 2) +
  labs(title = "Seniority level by education level — facet",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

print(p_sen_edu_3)
ggsave(file.path(out_dir, "seniority_by_education_v3_facet.png"),
       p_sen_edu_3, width = 14, height = 8, dpi = 300)


# ══════════════════════════════════════════════════════
# Activity-based job profiles
# TODO: profils à valider sur données réelles
# Approche actuelle : règles manuelles sur scores moyens
# À remplacer potentiellement par k-means
# ══════════════════════════════════════════════════════

# 1) Score moyen par thème et par répondant
df_scores <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code)) %>%
  select(id, theme_id, importance_code) %>%
  tidyr::pivot_wider(names_from = theme_id, values_from = importance_code,
                     names_prefix = "theme_") %>%
  # theme_1 = Data cleaning
  # theme_2 = Descriptive analysis
  # theme_3 = Inferential analysis
  # theme_4 = Modeling / ML
  # theme_5 = Development / automation
  # theme_6 = Supervision
  rename(
    cleaning    = theme_1,
    descriptive = theme_2,
    inferential = theme_3,
    modeling    = theme_4,
    automation  = theme_5,
    supervision = theme_6
  )

# 2) Assignation des profils
# TODO: règles à affiner sur données réelles
df_scores <- df_scores %>%
  mutate(
    job_profile = case_when(
      supervision >= 3 & supervision == pmax(cleaning, descriptive,
                                             inferential, modeling,
                                             automation, supervision)
      ~ "Manager / Supervisor",
      (modeling >= 3 | automation >= 3) & pmax(modeling, automation) >= pmax(cleaning, descriptive, inferential)
      ~ "Data Scientist / Engineer",
      (inferential >= 3) & inferential >= pmax(cleaning, descriptive, modeling)
      ~ "Statistician",
      (descriptive >= 3 | cleaning >= 3) & pmax(descriptive, cleaning) >= pmax(inferential, modeling)
      ~ "Data Analyst",
      TRUE                                ~ "Generalist"
    ),
    job_profile = factor(job_profile, levels = c(
      "Data Analyst",
      "Statistician",
      "Data Scientist / Engineer",
      "Manager / Supervisor",
      "Generalist"
    ))
  )

# Joindre au clean_data
clean_data <- clean_data %>%
  mutate(id = row_number()) %>%
  left_join(df_scores %>% select(id, job_profile), by = "id") %>%
  select(-id)

# 3) Distribution des profils — vérification
save_barplot_counts(clean_data, "job_profile",
                    "[TBD] Activity-based job profiles (rules to be validated)",
                    "jobprofile_distribution.png", xlab = NULL)

save_donut_counts(clean_data, "job_profile",
                  title = "[TBD] Activity-based job profiles",
                  filename = "donut_jobprofile.png",
                  drop_na = TRUE)

# 4) Job role × job profile
# TODO: titre et interprétation à revoir selon profils finaux
df_role_profile <- clean_data %>%
  filter(!is.na(job_role), !is.na(job_profile)) %>%
  count(job_profile, job_role, name = "n") %>%
  group_by(job_profile) %>%
  slice_max(n, n = 8) %>%   # top 8 titres par profil
  mutate(job_role = forcats::fct_reorder(job_role, n)) %>%
  ungroup()

p_role_profile <- ggplot(df_role_profile, aes(x = n, y = job_role)) +
  geom_col(fill = my_fill, color = my_border) +
  facet_wrap(~ job_profile, scales = "free_y", ncol = 2) +
  labs(
    title = "[TBD] Top job roles by activity-based profile",
    subtitle = "Rules-based profiles — to be validated on real data",
    x = "N respondents", y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey50", face = "italic"),
    strip.text    = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_role_profile)
ggsave(file.path(out_dir, "jobrole_by_profile_TBD.png"),
       p_role_profile, width = 14, height = 10, dpi = 300)


# ══════════════════════════════════════════════════════
# Domaines de formation × Genre × Lieu d'études
# Décomposé en 2 graphiques pour rester lisible
# ══════════════════════════════════════════════════════

df_field_gender_loc <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "",
         !is.na(dmgender), !is.na(study_location)) %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

# ── Graphique 1 : un fichier par lieu d'études ──
for (loc in levels(df_field_gender_loc$study_location)) {
  
  df_g1 <- df_field_gender_loc %>%
    filter(study_location == loc) %>%
    count(training_field, dmgender, name = "n") %>%
    group_by(training_field) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
  
  if (nrow(df_g1) == 0) next  # skip si pas de données
  
  p_g1 <- ggplot(df_g1, aes(x = training_field, y = pct, fill = dmgender)) +
    geom_col(width = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(
      values = c("Man"               = "#2C7FB8",
                 "Woman"             = "#e05252",
                 "Other"             = "#e6a817",
                 "Prefer not to say" = "#6dbe8d"),
      drop = FALSE
    ) +
    labs(
      title = paste0("Gender by training field — ", loc),
      x = NULL, y = "Share within field", fill = "Gender"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  safe_loc <- gsub("[^A-Za-z0-9]+", "_", loc)
  ggsave(file.path(out_dir, paste0("field_gender_loc_", safe_loc, ".png")),
         p_g1, width = 14, height = 7, dpi = 300)
}

# ── Graphique 2 : un fichier par genre ──
for (gen in levels(df_field_gender_loc$dmgender)) {
  
  df_g2 <- df_field_gender_loc %>%
    filter(dmgender == gen) %>%
    count(training_field, study_location, name = "n") %>%
    group_by(training_field) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
  
  if (nrow(df_g2) == 0) next
  
  p_g2 <- ggplot(df_g2, aes(x = training_field, y = pct, fill = study_location)) +
    geom_col(width = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set2", drop = FALSE) +
    labs(
      title = paste0("Study location by training field — ", gen),
      x = NULL, y = "Share within field", fill = "Study location"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  safe_gen <- gsub("[^A-Za-z0-9]+", "_", gen)
  ggsave(file.path(out_dir, paste0("field_location_gender_", safe_gen, ".png")),
         p_g2, width = 14, height = 7, dpi = 300)
}

# ── Importance activités statistiques — overall + par secteur ──
# ── Importance activités statistiques — overall + par secteur ──

# Joindre plsector AVANT d'unnester
df_us <- clean_data %>%
  mutate(id = row_number()) %>%
  select(id, ustime, plsector) %>%   # on garde plsector dès le début
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code)) %>%
  mutate(
    theme      = factor(theme, levels = theme_levels),
    importance = factor(importance, levels = importance_levels)
  )

importance_palette <- c(
  "Not at all important" = "#c0392b",
  "Slightly important"   = "#e09060",
  "Moderately important" = "#b0b8c1",
  "Important"            = "#6dbe8d",
  "Very important"       = "#1a7a4a"
)

plot_importance <- function(data, title) {
  data %>%
    count(theme, importance, name = "n") %>%
    group_by(theme) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    mutate(theme = forcats::fct_reorder(
      theme,
      ifelse(importance %in% c("Important", "Very important"), pct, 0),
      sum
    )) %>%
    ggplot(aes(x = pct, y = theme, fill = importance)) +
    geom_col(width = 0.7) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = importance_palette, drop = FALSE) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# 1) Overall
p_imp_overall <- plot_importance(df_us, "Importance of statistical activities — overall")
print(p_imp_overall)
ggsave(file.path(out_dir, "ustime_importance_overall.png"),
       p_imp_overall, width = 12, height = 6, dpi = 300)

# 2) Par secteur
for (sec in levels(df_us$plsector)) {
  
  df_sec <- df_us %>% filter(plsector == sec)
  
  n_resp <- n_distinct(df_sec$id)
  if (n_resp < 5) next
  
  p_sec <- plot_importance(
    df_sec,
    paste0("Importance of statistical activities — ", sec, " (n=", n_resp, ")")
  )
  
  safe_sec <- gsub("[^A-Za-z0-9]+", "_", sec)
  ggsave(file.path(out_dir, paste0("ustime_importance_sector_", safe_sec, ".png")),
         p_sec, width = 12, height = 6, dpi = 300)
}


# ── Séniorité et expérience par secteur ──
df_sector <- clean_data %>%
  mutate(plsector = as.character(plsector),
         plsenior = as.character(plsenior)) %>%
  filter(!is.na(plsector), plsector != "None", plsector != "NA",
         !is.na(plsenior), plsenior != "Never worked",
         !is.na(plyexp)) %>%
  mutate(plsector = factor(plsector),
         plsenior = factor(plsenior, levels = seniority_level_levels)) %>%
  droplevels()

# ── Graphique 1 : Expérience par secteur — boxplot ──
# Trié par médiane d'expérience

sector_order <- df_sector %>%
  group_by(plsector) %>%
  summarise(med = median(plyexp, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(plsector)

p_exp_sector <- df_sector %>%
  mutate(plsector = factor(plsector, levels = sector_order)) %>%
  ggplot(aes(x = plyexp, y = plsector)) +
  geom_boxplot(fill = my_fill, color = "grey30",
               alpha = 0.7, outlier.size = 1.5) +
  labs(
    title = "Years of experience by sector",
    x = "Years of experience", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_exp_sector)
ggsave(file.path(out_dir, "experience_by_sector.png"),
       p_exp_sector, width = 12, height = 10, dpi = 300)

# ── Graphique 2 : Séniorité par secteur — stacked bar 100% ──
# Trié par % top management + middle management

seniority_palette <- c(
  "Intern / Entry level position" = "#c0392b",
  "No managerial function"        = "#e09060",
  "Lower management"              = "#b0b8c1",
  "Middle management"             = "#6dbe8d",
  "Top management"                = "#1a7a4a",
  "Never worked"                  = "#cccccc"
)

sector_order_sen <- df_sector %>%
  count(plsector, plsenior, name = "n") %>%
  group_by(plsector) %>%
  mutate(pct = n / sum(n)) %>%
  filter(plsenior %in% c("Top management", "Middle management")) %>%
  summarise(senior_pct = sum(pct)) %>%
  arrange(senior_pct) %>%
  pull(plsector)

p_sen_sector <- df_sector %>%
  count(plsector, plsenior, name = "n") %>%
  group_by(plsector) %>%
  mutate(pct = n / sum(n),
         plsector = factor(plsector, levels = sector_order_sen)) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = plsector, fill = plsenior)) +
  geom_col(width = 0.8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(values = seniority_palette, drop = FALSE) +
  labs(
    title = "Seniority level by sector",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2))

print(p_sen_sector)
ggsave(file.path(out_dir, "seniority_by_sector.png"),
       p_sen_sector, width = 13, height = 10, dpi = 300)



# ══════════════════════════════════════════════════════
# Formation continue — overall + par secteur / séniorité / expérience
# ══════════════════════════════════════════════════════

# Palette fixe — même couleur pour chaque catégorie dans tous les graphiques
cont_edu_palette <- c(
  "No"                                                  = "#cccccc",
  "MAS, DAS, CAS"                                       = "#2C7FB8",
  "Certified online training (Coursera, Edx, etc.)"     = "#6dbe8d",
  "Postgraduate in Business/Finance (MBA, EMBA, etc.)"  = "#e6a817",
  "Post-Doc"                                            = "#e05252",
  "Further training with an employer"                   = "#1a4a7a"
)

# ── 1) Overall ──
save_donut_counts(
  clean_data, "continuous_education",
  title    = "Continuous education — overall",
  filename = "cont_edu_overall.png",
  palette  = cont_edu_palette,
  drop_na  = TRUE
)

# ── 2) Par secteur ──
df_cont_sector <- clean_data %>%
  mutate(plsector = as.character(plsector)) %>%
  filter(!is.na(plsector), plsector != "None", plsector != "NA",
         !is.na(continuous_education)) %>%
  mutate(plsector = factor(plsector))

for (sec in levels(df_cont_sector$plsector)) {
  df_sec <- df_cont_sector %>% filter(plsector == sec)
  if (nrow(df_sec) < 5) next
  
  safe_sec <- gsub("[^A-Za-z0-9]+", "_", sec)
  save_donut_counts(
    df_sec, "continuous_education",
    title    = paste0("Continuous education — ", sec, " (n=", nrow(df_sec), ")"),
    filename = paste0("cont_edu_sector_", safe_sec, ".png"),
    palette  = cont_edu_palette,
    drop_na  = TRUE
  )
}

# ── 3) Par séniorité ──
df_cont_senior <- clean_data %>%
  mutate(plsenior = as.character(plsenior)) %>%
  filter(!is.na(plsenior), plsenior != "Never worked",
         !is.na(continuous_education)) %>%
  mutate(plsenior = factor(plsenior, levels = seniority_level_levels))

for (sen in levels(df_cont_senior$plsenior)) {
  df_sen <- df_cont_senior %>% filter(plsenior == sen)
  if (nrow(df_sen) < 5) next
  
  safe_sen <- gsub("[^A-Za-z0-9]+", "_", sen)
  save_donut_counts(
    df_sen, "continuous_education",
    title    = paste0("Continuous education — ", sen, " (n=", nrow(df_sen), ")"),
    filename = paste0("cont_edu_seniority_", safe_sen, ".png"),
    palette  = cont_edu_palette,
    drop_na  = TRUE
  )
}

# ── 4) Par expérience ──
df_cont_exp <- clean_data %>%
  filter(!is.na(plyexp), !is.na(continuous_education)) %>%
  mutate(exp_group = cut(plyexp,
                         breaks = c(0, 5, 10, 20, Inf),
                         labels = c("0–5 years", "6–10 years",
                                    "11–20 years", "20+ years"),
                         right = TRUE, include.lowest = TRUE))

for (grp in levels(df_cont_exp$exp_group)) {
  df_grp <- df_cont_exp %>% filter(exp_group == grp)
  if (nrow(df_grp) < 5) next
  
  safe_grp <- gsub("[^A-Za-z0-9]+", "_", grp)
  save_donut_counts(
    df_grp, "continuous_education",
    title    = paste0("Continuous education — ", grp, " (n=", nrow(df_grp), ")"),
    filename = paste0("cont_edu_exp_", safe_grp, ".png"),
    palette  = cont_edu_palette,
    drop_na  = TRUE
  )
}