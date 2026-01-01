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
data <- read.csv(data_file, header = TRUE, sep=";")

## Convert data into usable variables

### DATES

data$submitdate = as.Date(data$submitdate)
data$datestamp = as.Date(data$datestamp)
data$startdate = as.Date(data$startdate)

### NUMERICS

data$dmbirth = strtoi(data$dmbirth)
data$tryear = strtoi(data$tryear)

### LOGICALS

data$sssknow <- as.logical(data$sssknow)

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

data$dmgender <- factor(
  gender_levels[as.integer(data$dmgender)],
  levels = gender_levels
)

#### origin

data$origin <- origin_from_swiss(data$dmswiss, data$dmnatio)

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
data$dmres <- factor(residency_levels[as.integer(data$dmres)], levels = residency_levels)

#### work location
residency_levels <- c(residency_levels, "I do not work")
data$dmwork <- factor(residency_levels[as.integer(data$dmwork)], levels = residency_levels)

#### sss involvement
involvement_level <- c(
                        "Not a member",
                        "Passive",
                        "Occasional",
                        "Active",
                        "Volunteer"
                      )
data$sssmember <- factor(involvement_level[as.integer(data$sssmember)],levels = involvement_level)

#### Education

education_level <- c(
                      "Bachelor of applied science",
                      "University bachelor",
                      "Master of applied science",
                      "University master",
                      "PhD",
                      "Other"
                    )

data$trlvl <- factor(education_level[as.integer(data$trlvl)], levels = education_level)
data$study_location <- origin_from_swiss(data$trcontswiss,data$trreg)

#### training field

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
                          "Statistics"
                        )

####################################################################
#   Study variables
####################################################################

# 1. demographics data


# table of current year - date of birth where there is an input
ages <- strtoi(strftime(Sys.Date(),"%Y")) - subset(data$dmbirth, ! is.na(data$dmbirth))
hist(ages,breaks=100)