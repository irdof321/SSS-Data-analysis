####################################################################
#      CONSTANTS PARAMETERS
####################################################################
data_file <- "sample_survey_results.csv"





####################################################################
#   Preliminaries
####################################################################
data <- read.csv(data_file, header = TRUE, sep=";")

# 1. demographics data
ages <- strtoi(strftime(Sys.Date(),"%Y")) - subset(data$dmbirth, ! is.na(data$dmbirth))
hist(ages,breaks=100)