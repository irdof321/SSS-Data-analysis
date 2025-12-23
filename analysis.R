
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


# if remove not submitted data
print(is.Date(data$submitdate))

####################################################################
#   Study variables
####################################################################

# 1. demographics data


# table of current year - date of birth where there is an input
ages <- strtoi(strftime(Sys.Date(),"%Y")) - subset(data$dmbirth, ! is.na(data$dmbirth))
hist(ages,breaks=100)