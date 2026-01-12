####################################################################
#     Libraries
####################################################################
library(lubridate)


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

####################################################################
#   Study variables
####################################################################

