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

# Helper: donut plot with legend on the side (no labels inside)
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
  
  # Optional wrapping for long category names (legend display)
  cat_disp <- dd$cat
  if (!is.null(wrap_width)) {
    cat_disp <- stringr::str_wrap(cat_disp, width = wrap_width)
  }
  
  # Legend labels (optionally include % and N)
  if (legend_show_pct) {
    dd$cat_lab <- paste0(cat_disp, " — ", dd$n, " (", scales::percent(dd$pct, accuracy = 1), ")")
  } else {
    dd$cat_lab <- cat_disp
  }
  
  # IMPORTANT: keep fill mapped to raw 'cat' (so palettes match)
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
      legend.key.size = unit(0.6, "lines"),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    # Replace legend text with our custom labels (but keep color mapping stable)
    scale_fill_discrete(labels = dd$cat_lab)
  
  # Manual colors if provided (NOW it will work because fill = cat)
  if (!is.null(palette)) {
    p <- p + scale_fill_manual(values = palette, labels = dd$cat_lab)
  }
  
  print(p)
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = 12, height = 6, dpi = 300
  )
}




# =========================
# 1) Birth year (10-year bins)
# =========================
if (any(!is.na(clean_data$dmbirth))) {
  min_b <- floor(min(clean_data$dmbirth, na.rm = TRUE) / 10) * 10
  max_b <- ceiling(max(clean_data$dmbirth, na.rm = TRUE) / 10) * 10 + 10
  birth_breaks <- seq(min_b, max_b, by = 10)
  
  save_binned_counts(
    clean_data,
    numvar = "dmbirth",
    breaks = birth_breaks,
    title = "Birth year (10-year bins)",
    filename = "basic_birthyear_10y.png",
    xlab = "Birth year bin",
    label_mode = "year"
  )
}
# =========================
# 2) Training completion year (5-year bins)
# =========================
if (any(!is.na(clean_data$tryear))) {
  min_t <- floor(min(clean_data$tryear, na.rm = TRUE) / 5) * 5
  max_t <- ceiling(max(clean_data$tryear, na.rm = TRUE) / 5) * 5 + 5
  tryear_breaks <- seq(min_t, max_t, by = 5)
  
  save_binned_counts(
    clean_data,
    numvar = "tryear",
    breaks = tryear_breaks,
    title = "Training completion year (5-year bins)",
    filename = "basic_trainingyear_5y.png",
    xlab = "Training year bin"
  )
}

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


# 4) Gender
save_barplot_counts(clean_data, "dmgender", "Gender", "basic_gender.png",
                    xlab = NULL)

save_donut_counts(
  clean_data, "dmgender",
  title = "Gender",
  filename = "donut_gender.png",
  palette = c(
    "Man" = "blue",
    "Woman" = "red",
    "Other" = "yellow",
    "Prefer not to say" = "pink"
  ),
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


# 18) Years of experience
save_barplot_counts(clean_data, "plyexp", "Years of experience",
                    "basic_years_experience.png", xlab = NULL)

# 19) Employment rate
save_barplot_counts(clean_data, "plrate", "Employment rate",
                    "basic_employment_rate.png", xlab = NULL)

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

