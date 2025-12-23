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

### LOGICALS

data$sssknow <- as.logical(data$sssknow)

### FACTORS

#### gender
gender_levels <- c("Man", "Woman", "Other", "Prefer not to say")

data$dmgender <- factor(
  gender_levels[as.integer(data$dmgender)],
  levels = gender_levels
)

#### origin
origin_levels <- c(
                    "Swiss",
                    "Europe",
                    "North America",
                    "South and Central America",
                    "Middle East",
                    "Africa",
                    "Asia"
                    )
continent_levels <- origin_levels[-1]  # removes "Swiss"
swiss_code <- as.integer(data$dmswiss)   # 1 or 2 (or NA)
cont_code  <- as.integer(data$dmnatio)   # 1..6 (or NA)


#    - if Swiss -> "Swiss"
#    - if Not Swiss -> take continent label using cont_code as an index
#    - otherwise -> NA
origin_chr <- ifelse(
  swiss_code == 1, "Swiss",
  ifelse(swiss_code == 2, continent_levels[cont_code], NA_character_)
)
data$origin <- factor(origin_chr, levels = origin_levels)

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

####################################################################
#   Study variables
####################################################################

# 1. demographics data


# table of current year - date of birth where there is an input
ages <- strtoi(strftime(Sys.Date(),"%Y")) - subset(data$dmbirth, ! is.na(data$dmbirth))
hist(ages,breaks=100)