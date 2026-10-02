# ══════════════════════════════════════════════════════════════════
#  01_config.R — Librairies et constantes globales
# ══════════════════════════════════════════════════════════════════

####################################################################
#  Libraries
####################################################################
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(readr)
library(ggrepel)
library(stringr)
library(gt)
library(webshot2)   # backend utilisé par gt::gtsave() pour exporter en PNG

####################################################################
#  Parameters
####################################################################
#data_file <- "sample_survey_results.csv"
data_file <- "results.csv"
#data_file <- "simulated_sample_survey_results_300.csv"

REMOVE_NOT_SUB <- FALSE   # remove rows without a submitted date

ref_year <- 2026

####################################################################
#  Data compatibility / current questionnaire
####################################################################

# TRUE for the current LimeSurvey export using "Answer codes".
# FALSE for the old simulated sample, which is already in the expected format.
IS_LIMESURVEY_EXPORT <- TRUE

# Salary is now collected in bands. The analysis uses the observed bands as the
# primary representation and an approximate continuous value (class midpoint)
# for summaries/stratifications.
ANALYZE_SALARY <- TRUE

# Boxplot display rules. Groups smaller than these thresholds are not shown,
# because a boxplot is not informative/reliable with very small n.
# Job titles are often sparse, so a separate threshold can be used there.
GROUP_MIN_N <- 5            # minimum n for grouped descriptive comparisons
SALARY_BOXPLOT_MIN_N <- GROUP_MIN_N
SALARY_JOBROLE_MIN_N <- 3     # job titles are sparse; separate threshold
SALARY_BOXPLOT_SHOW_N <- TRUE
SALARY_BOXPLOT_SHOW_KEY <- TRUE

# Salary-band definitions from the current LimeSurvey questionnaire.
# AO01 (<60k) and AO16 (>=200k) are open-ended. For midpoint-based summaries
# we approximate them as 50-59,999 and 200-209,999 respectively, i.e. 55k/205k.
# These assumptions are deliberately explicit and can be changed here.
salary_band_codes <- sprintf("AO%02d", 1:16)
salary_band_labels <- c(
  "< 60 000",
  "60 000 - 69 999",
  "70 000 - 79 999",
  "80 000 - 89 999",
  "90 000 - 99 999",
  "100 000 - 109 999",
  "110 000 - 119 999",
  "120 000 - 129 999",
  "130 000 - 139 999",
  "140 000 - 149 999",
  "150 000 - 159 999",
  "160 000 - 169 999",
  "170 000 - 179 999",
  "180 000 - 189 999",
  "190 000 - 199 999",
  "≥ 200 000"
)
# Exact class bounds where they are known. The first and last bands are open-ended:
#   AO01: < 60k       -> lower bound unknown
#   AO16: >= 200k     -> upper bound unknown
# Midpoints for the two open-ended classes remain explicit working assumptions
# (55k and 205k) used ONLY for point estimates.
salary_band_lower <- c(
  NA,
  seq(60000, 190000, by = 10000),
  200000
)
salary_band_upper <- c(
  59999,
  seq(69999, 199999, by = 10000),
  NA
)
salary_band_midpoints <- seq(55000, 205000, by = 10000)

names(salary_band_labels)    <- salary_band_codes
names(salary_band_lower)     <- salary_band_codes
names(salary_band_upper)     <- salary_band_codes
names(salary_band_midpoints) <- salary_band_codes

# Map of LimeSurvey answer codes used by the current "professional role"
# question to the labels expected by the analysis.
# Codes not yet observed are kept as "Unmapped role (<code>)" so that they
# are visible instead of silently becoming NA.
plrole_code_map <- c(
  "AO04" = "Biostatistician",
  "AO05" = "Business intelligence (BI) analyst",
  "AO08" = "Data analyst",
  "AO10" = "Data engineer",
  "AO12" = "Data manager",
  "AO13" = "Data scientist",
  "AO14" = "Data steward",
  "AO17" = "Econometrician",
  "AO18" = "Epidemiologist",
  "AO20" = "Health economist",
  "AO23" = "Operations research analyst",
  "AO25" = "Professor of statistics",
  "AO26" = "Psychometrician",
  "AO30" = "Research statistician (methodological / theoretical)",
  "AO32" = "Statistical programmer",
  "AO33" = "Statistical project manager",
  "AO34" = "Statistical researcher (applied)",
  "AO35" = "Statistician",
  "AO36" = "Survey statistician / sampling statistician"
)

# Convert the current LimeSurvey answer-code export to the conventions used
# by the simulated sample / existing cleaning code.
prepare_limesurvey_data <- function(df) {

  if (!IS_LIMESURVEY_EXPORT) return(df)

  # --- Gender -------------------------------------------------------
  # Current LimeSurvey: 1=Female, 2=Male, 3=Other, 4=Prefer not to say
  # Simulated sample:    1=Man,    2=Woman, 3=Other, 4=Prefer not to say
  if ("dmgender" %in% names(df)) {
    x <- as.character(df$dmgender)
    df$dmgender <- dplyr::case_when(
      x == "1" ~ "2",
      x == "2" ~ "1",
      x == "3" ~ "3",
      x == "4" ~ "4",
      TRUE     ~ NA_character_
    )
  }

  # --- Yes / No variables ------------------------------------------
  # dmswiss and trcontswiss:
  # current LimeSurvey 1/0 -> simulated sample 1/2
  for (v in intersect(c("dmswiss", "trcontswiss"), names(df))) {
    x <- as.character(df[[v]])
    df[[v]] <- dplyr::case_when(
      x == "1" ~ "1",
      x == "0" ~ "2",
      TRUE     ~ NA_character_
    )
  }

  # sssknow:
  # current LimeSurvey 1=Yes, 2=No -> simulated sample 1=Yes, 0=No
  if ("sssknow" %in% names(df)) {
    x <- as.character(df$sssknow)
    df$sssknow <- dplyr::case_when(
      x == "1" ~ "1",
      x == "2" ~ "0",
      TRUE     ~ NA_character_
    )
  }

  # --- "Other" codes ------------------------------------------------
  # trlvl: sample uses 6 = Other
  if ("trlvl" %in% names(df)) {
    x <- as.character(df$trlvl)
    df$trlvl <- ifelse(x == "-oth-", "6", x)
  }

  # SSS involvement: add a dedicated code 6 = Other.
  if ("sssmember" %in% names(df)) {
    x <- as.character(df$sssmember)
    df$sssmember <- ifelse(x == "-oth-", "6", x)
  }

  # Seniority: code 6 is already "Never worked", so use 7 = Other.
  if ("plsenior" %in% names(df)) {
    x <- as.character(df$plsenior)
    df$plsenior <- ifelse(x == "-oth-", "7", x)
  }

  # --- Professional role -------------------------------------------
  # The simulated sample stored text labels directly. The current
  # questionnaire stores LimeSurvey AOxx answer codes.
  if ("plrole" %in% names(df)) {
    code <- as.character(df$plrole)

    # read.csv(check.names=TRUE) turns plrole[other] into plrole.other.
    other_col <- intersect(c("plrole.other.", "plrole.other"), names(df))
    other_txt <- rep(NA_character_, nrow(df))
    if (length(other_col) > 0) {
      other_txt <- trimws(as.character(df[[other_col[1]]]))
      other_txt[other_txt == ""] <- NA_character_
    }

    mapped <- unname(plrole_code_map[code])

    mapped[code == "-oth-"] <- other_txt[code == "-oth-"]

    unknown <- !is.na(code) & code != "" & code != "-oth-" & is.na(mapped)
    mapped[unknown] <- paste0("Unmapped role (", code[unknown], ")")

    df$plrole <- mapped
  }

  df
}


####################################################################
#  Output directories
####################################################################
out_dir <- "descriptives_plots"
if (!dir.exists(out_dir)) dir.create(out_dir)

tab_dir <- "descriptives_tables"
if (!dir.exists(tab_dir)) dir.create(tab_dir)

csv_dir <- "csv_files"

# ── Interrupteur : générer (ou non) le miroir CSV des tables ──────
generate_csv_files <- FALSE   # mettre FALSE pour désactiver

if (generate_csv_files && !dir.exists(csv_dir)) dir.create(csv_dir)

####################################################################
#  Style
####################################################################
my_fill   <- "#2C7FB8"
my_border <- NA

# Counter — global, incrémenté par les helpers
table_count <- as.integer(1)

####################################################################
#  Table styling (gt) — identité visuelle des tableaux
####################################################################
tab_accent     <- "#2C7FB8"   # couleur d'accent (cohérente avec my_fill)
tab_accent_dk  <- "#1a4a7a"   # variante foncée (en-tête)
tab_stripe     <- "#EAF2F8"   # bandes zébrées très légères
tab_border     <- "#D9E2EC"   # lignes fines
tab_font       <- "Arial"     # police (fallback système si absente)

####################################################################
#  Word report generation
####################################################################
# The report is generated at the end of analysis.R by 08_word_generation.R.
# Figures remain PNGs; result tables are inserted as editable Word tables
# using the CSV mirrors generated by 06/07.
GENERATE_WORD_REPORT <- TRUE
WORD_REPORT_FILE <- "SSS_Survey_Report.docx"
WORD_REPORT_TEMPLATE <- "report_reference.docx"
WORD_REPORT_INCLUDE_SSS_MEMBERS <- TRUE
WORD_REPORT_MAX_FIGURE_WIDTH <- 6.35   # inches
WORD_REPORT_MAX_FIGURE_HEIGHT <- 7.25  # inches
WORD_REPORT_TABLE_FONT_SIZE <- 8.5

# Word navigation / appendix
# The visible TOC is generated directly by R so that the document never opens
# with an apparently empty table of contents. A native Word TOC field is also
# inserted underneath it and can be refreshed in Word to obtain page numbers.
WORD_REPORT_STATIC_TOC <- TRUE
WORD_REPORT_DYNAMIC_TOC <- TRUE
WORD_REPORT_INCLUDE_APPENDIX_A <- TRUE

# The automatic Word report uses the CSV mirrors to create editable Word tables.
# Therefore CSV generation is forced on when the Word report is enabled.
if (isTRUE(GENERATE_WORD_REPORT)) {
  generate_csv_files <- TRUE
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
}

####################################################################
#  Factor level definitions (shared across cleaning + plots)
####################################################################
origin_levels <- c(
  "Swiss",
  "Europe",
  "North America",
  "South and Central America",
  "Middle East",
  "Africa",
  "Asia",
  "Oceania"
)
continent_levels <- origin_levels[-1]

gender_levels <- c("Man", "Woman", "Other", "Prefer not to say")

residency_levels <- c(
  "Not in Switzerland",
  "AG", "AR", "AI", "BL", "BS", "BE", "FR", "GE", "GL", "GR",
  "JU", "LU", "NE", "NW", "OW", "SH", "SZ", "SO", "SG", "TG",
  "TI", "UR", "VS", "VD", "ZG", "ZH"
)

involvement_level <- c(
  "Not a member", "Passive", "Occasional", "Active", "Volunteer", "Other"
)

time_sss_level <- c(
  "Less than one year",
  "Less than five years",
  "Less than ten years",
  "Ten years or more"
)

education_level <- c(
  "Bachelor of applied science",
  "University bachelor",
  "Master of applied science",
  "University master",
  "PhD",
  "Other"
)

training_field_study <- c(
  "Theology", "Law", "Science of economics", "Health, sport",
  "Psychology", "Sociology", "Other social sciences",
  "Language, literature", "History, civilizations study",
  "Art, music, design", "Mathematics",
  "Informatics / Computer science", "Statistics",
  "Data science", "Applied statistics",
  "Natural science, environmental science",
  "Technical science, engineering", "Education", "Other"
)

continuous_education_levels <- c(
  "No",
  "MAS, DAS, CAS",
  "Certified online training (Coursera, Edx, etc.)",
  "Postgraduate in Business/Finance (MBA, EMBA, etc.)",
  "Post-Doc",
  "Further training with an employer",
  "Other"
)

employment_status_level <- c(
  "Employed", "Self-employed", "Student", "Unemployed", "Retired"
)

sector_job_level <- c(
  "Banking / Finance / Insurance", "Luxury goods",
  "IT/ Telecommunicatins industry", "Consumer goods",
  "Audit/ Consulting/ Professional service", "Automotive",
  "Aviation/ Aerospace/ Defense", "Chemicals/ Ingredients",
  "Electrical / Electronics / Semiconductors",
  "Government / Public administration",
  "Machinery and Equipment / Automation", "Materials",
  "Pharmaceuticals", "Real estate", "Transportation/ Rail",
  "Watchmaking", "Biotechnology/ Bioengineering",
  "Construction/ Civil engineering", "Engineering consulting",
  "Hospital/ Healthcare", "Logistics/ Suplly chain industry",
  "Media / Advertising / Communication",
  "Medical technologies and devices",
  "Nonprofit organization / Social", "Oil and gas / Energy",
  "Primary or Secondary Education", "Architecture / Urban planning",
  "Higher education / Research / Academia",
  "Renewables / Environment", "Other", "None"
)

seniority_level_levels <- c(
  "Intern / Entry level position",
  "No managerial function",
  "Lower management",
  "Middle management",
  "Top management",
  "Never worked",
  "Other"
)

skills_levels <- c(
  "Statistical programming (R, SAS, Python (Statistics and ML libraries), SPSS, Stata, etc.)",
  "Other programming (C, C++, Java, Python, etc.)",
  "Data visualization (Power BI, Tableau, Looker Studio, etc.)",
  "Scientific writing and/or research",
  "Project management",
  "Time management"
)

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
  "No use", "Direct practice", "Supervision",
  "Direct practice and supervision"
)

satisf_levels <- c(
  "Very satisfied", "Somewhat satisfied", "Neutral",
  "Not so satisfied", "Not at all satisfied"
)

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

work_satisfaction_levels <- c(
  "Very satisfied", "Quite satisfied", "Neutral",
  "Not quite satisfied", "Not at all satisfied"
)