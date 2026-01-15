####################################################################
#     Libraries
####################################################################
library(lubridate)
library(dplyr)
library(tidyr)



####################################################################
#      CONSTANTS PARAMETERS
####################################################################
data_file <- "sample_survey_results.csv"
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
clean_data$salary <- suppressWarnings(as.numeric(x))

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
#   Study variables
####################################################################

