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
#data_file <- "simulated_sample_survey_results_100.csv"
data_file <- "simulated_sample_survey_results_300.csv"

REMOVE_NOT_SUB <- FALSE   # remove rows without a submitted date

ref_year <- 2026

####################################################################
#  Output directories
####################################################################
out_dir <- "descriptives_plots"
if (!dir.exists(out_dir)) dir.create(out_dir)

tab_dir <- "descriptives_tables"
if (!dir.exists(tab_dir)) dir.create(tab_dir)

csv_dir <- "csv_files"

# ── Interrupteur : générer (ou non) le miroir CSV des tables ──────
generate_csv_files <- TRUE   # mettre FALSE pour désactiver

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
#  Factor level definitions (shared across cleaning + plots)
####################################################################
origin_levels <- c(
  "Swiss",
  "Europe",
  "North America",
  "South and Central America",
  "Middle East",
  "Africa",
  "Asia"
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
  "Not a member", "Passive", "Occasional", "Active", "Volunteer"
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
  "Further training with an employer"
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
  "Never worked"
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