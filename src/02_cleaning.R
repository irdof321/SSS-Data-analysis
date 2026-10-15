# ══════════════════════════════════════════════════════════════════
#  02_cleaning.R — Import et nettoyage -> clean_data
#  Dépend de : 01_config.R (doit être sourcé avant)
# ══════════════════════════════════════════════════════════════════

####################################################################
#  Helper interne : conversion nationalité -> facteur origin
####################################################################
origin_from_swiss <- function(dmswiss, dmnatio) {
  to_int <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    suppressWarnings(as.integer(x))
  }
  swiss_code <- to_int(dmswiss)
  cont_code  <- to_int(dmnatio)
  out <- rep(NA_character_, length(swiss_code))
  out[!is.na(swiss_code) & swiss_code == 1] <- "Swiss"
  idx           <- !is.na(swiss_code) & swiss_code == 2
  idx_valid_cont <- idx & !is.na(cont_code) & cont_code >= 1 &
                    cont_code <= length(continent_levels)
  out[idx_valid_cont] <- continent_levels[cont_code[idx_valid_cont]]
  factor(out, levels = origin_levels)
}

to_int01 <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- suppressWarnings(as.integer(x))
  x[is.na(x) | !(x %in% c(0L, 1L))] <- 0L
  x
}

####################################################################
#  Import
####################################################################
raw_data <- read.csv(
  data_file,
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = TRUE
)

# Current LimeSurvey answer-code exports are harmonised here so that the
# rest of the cleaning pipeline can keep using the conventions of the
# simulated sample.
raw_data <- prepare_limesurvey_data(raw_data)

# Guard: a wrong export option (full answers instead of answer codes, or Y/N
# left unconverted) empties the key variables and would silently shrink the
# analysis sample instead of failing. Stop here with an explicit message.
.chk_employed <- suppressWarnings(as.integer(as.character(raw_data$plemployed)))
if (!any(.chk_employed %in% c(0L, 1L))) {
  stop("plemployed ne contient aucune valeur exploitable. Vérifiez l'export ",
       "LimeSurvey : 'Export responses as' = Answer codes, ",
       "'Export questions as' = Question code, séparateur = point-virgule.")
}

####################################################################
#  Comptages pour le flux d'échantillon (Tableau 2)
####################################################################
# Read from the export itself when it contains all responses; the config
# overrides take precedence when the export is restricted to completed ones.
.submitted <- !is.na(raw_data$submitdate) & trimws(as.character(raw_data$submitdate)) != ""
N_STARTED  <- if (!is.na(N_STARTED_OVERRIDE))  N_STARTED_OVERRIDE  else nrow(raw_data)
N_COMPLETE <- if (!is.na(N_COMPLETE_OVERRIDE)) N_COMPLETE_OVERRIDE else sum(.submitted)

if (REMOVE_NOT_SUB && any(!.submitted)) {
  message("\u21b7 ", sum(!.submitted), " questionnaire(s) non soumis retiré(s) ",
          "(partiels — comptés dans le Tableau 2)")
  raw_data <- raw_data[.submitted, , drop = FALSE]
  rownames(raw_data) <- NULL
}

clean_data <- raw_data[, 0, drop = FALSE]

####################################################################
#  Dates
####################################################################
clean_data$submitdate <- as.Date(raw_data$submitdate)
clean_data$datestamp  <- as.Date(raw_data$datestamp)
clean_data$startdate  <- as.Date(raw_data$startdate)

####################################################################
#  Numerics
####################################################################
clean_data$dmbirth <- strtoi(raw_data$dmbirth)
clean_data$tryear  <- strtoi(raw_data$tryear)
clean_data$age     <- ifelse(is.na(clean_data$dmbirth), NA_integer_,
                             ref_year - clean_data$dmbirth)

####################################################################
#  Logicals
####################################################################
clean_data$sssknow <- suppressWarnings(
  as.integer(as.character(raw_data$sssknow))
) == 1L

####################################################################
#  Factors — démographie
####################################################################
clean_data$dmgender <- factor(
  gender_levels[as.integer(raw_data$dmgender)],
  levels = gender_levels
)

clean_data$origin <- origin_from_swiss(raw_data$dmswiss, raw_data$dmnatio)

clean_data$dmres <- factor(
  residency_levels[as.integer(raw_data$dmres)],
  levels = residency_levels
)

work_residency_levels <- c(residency_levels, "I do not work")
clean_data$dmwork <- factor(
  work_residency_levels[as.integer(raw_data$dmwork)],
  levels = work_residency_levels
)

####################################################################
#  Factors — SSS
####################################################################
clean_data$sssmember <- factor(
  involvement_level[as.integer(raw_data$sssmember)],
  levels = involvement_level
)

clean_data$ssstime <- factor(
  time_sss_level[as.integer(raw_data$ssstime)],
  levels = time_sss_level
)

####################################################################
#  Factors — formation initiale
####################################################################
clean_data$trlvl <- factor(
  education_level[as.integer(raw_data$trlvl)],
  levels = education_level
)

# NOTE: unlike `dmnatio` (1 = Europe), the `trreg` answer codes start one
# position later (observed: code 2 is by far the most frequent among people who
# studied abroad, as Europe is for nationality). Codes are therefore shifted by
# one before mapping to `continent_levels`. Check against the LimeSurvey answer
# list for `trreg` if the questionnaire is modified.
trreg_shifted <- suppressWarnings(as.integer(as.character(raw_data$trreg))) - 1L
clean_data$study_location <- origin_from_swiss(
  raw_data$trcontswiss, trreg_shifted
)

# Domaines de formation (bitmap multi-réponse)
cols <- paste0("trarea.", 1:19, ".")
training_fields_bitmap <- as.matrix(
  data.frame(lapply(raw_data[, cols, drop = FALSE], to_int01))
)
storage.mode(training_fields_bitmap) <- "integer"

other_col <- intersect(c("trarea.other.", "trarea.other"), names(raw_data))
other_text <- if (length(other_col) > 0) {
  trimws(as.character(raw_data[[other_col[1]]]))
} else {
  rep(NA_character_, nrow(raw_data))
}
other_text[is.na(other_text) | other_text %in% c("", "0", "-", "n/a", "N/A", "none", "None")] <- NA_character_

clean_data$training_fields_list <- lapply(
  seq_len(nrow(training_fields_bitmap)), function(i) {
    r   <- training_fields_bitmap[i, ]
    sel <- training_field_study[r == 1L]
    if (r[19] == 1L && !is.na(other_text[i]))
      sel <- c(sel, other_text[i])
    sel
  }
)

####################################################################
#  Factors — formation continue
####################################################################
cont_cols   <- paste0("trcont.", 1:7, ".")
cont_bitmap <- as.matrix(
  data.frame(lapply(raw_data[, cont_cols, drop = FALSE], to_int01))
)
storage.mode(cont_bitmap) <- "integer"

empty_row <- rowSums(cont_bitmap) == 0L
cont_bitmap[empty_row, 1] <- 1L

clean_data$continuous_education <- apply(cont_bitmap, 1, function(r) {
  idx <- which(r == 1L)
  if (length(idx) == 1) continuous_education_levels[idx] else NA_character_
})

clean_data$trcont2 <- rowSums(cont_bitmap[, 2:7, drop = FALSE]) > 0L

####################################################################
#  Emploi
####################################################################
clean_data$employed <- suppressWarnings(
  as.integer(as.character(raw_data$plemployed))
) == 1L

status_code <- suppressWarnings(as.integer(as.character(raw_data$plstatus)))
clean_data$job_status <- case_when(
  clean_data$employed ~ "Employed",
  status_code == 2L   ~ "Self-employed",
  status_code == 3L   ~ "Student",
  status_code == 4L   ~ "Unemployed",
  status_code == 5L   ~ "Retired",
  as.character(raw_data$plstatus) == "-oth-" ~ "Other",
  TRUE ~ NA_character_
)

raw_data$plrole  <- as.character(raw_data$plrole)
clean_data$job_role <- ifelse(
  clean_data$job_status == "Student",
  "Student",
  raw_data$plrole
)
clean_data$job_role <- trimws(clean_data$job_role)
clean_data$job_role[clean_data$job_role == ""] <- NA

clean_data$plsector <- factor(
  sector_job_level[as.integer(raw_data$plsector)],
  levels = sector_job_level
)

clean_data$plyexp  <- as.integer(raw_data$plyexp)
clean_data$plrate  <- as.integer(raw_data$plrate)

clean_data$plsenior <- factor(
  seniority_level_levels[as.integer(raw_data$plsenior)],
  levels = seniority_level_levels
)

####################################################################
#  Compétences (bitmap multi-réponse)
####################################################################
skill_cols <- paste0("plskill.", 1:6, ".")
skill_mat  <- as.data.frame(
  lapply(raw_data[skill_cols], function(x) as.integer(as.character(x)))
)

clean_data$skills <- lapply(seq_len(nrow(skill_mat)), function(i) {
  idx <- which(skill_mat[i, ] == 1)
  skills_levels[idx]
})

clean_data$skills_str <- vapply(clean_data$skills, function(x) {
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}, character(1))

####################################################################
#  Utilisation du temps / Activités statistiques (ustime)
####################################################################
ustime_cols <- grep("^ustime\\.[1-6]\\.\\.[1-2]\\.$",
                    names(raw_data), value = TRUE)

ustime_list <- lapply(seq_len(nrow(raw_data)), function(i) {
  tmp <- raw_data[i, ustime_cols, drop = FALSE]
  long <- tidyr::pivot_longer(
    tmp,
    cols = everything(),
    names_to = c("theme_id", "scale_id"),
    names_pattern = "^ustime\\.(\\d+)\\.\\.(\\d+)\\.$",
    values_to = "value"
  ) %>%
    mutate(theme_id = as.integer(theme_id),
           scale_id = as.integer(scale_id)) %>%
    pivot_wider(names_from = scale_id, values_from = value,
                names_prefix = "scale_") %>%
    transmute(
      theme_id,
      theme            = theme_levels[theme_id],
      importance_code  = scale_1,
      involvement_code = scale_2,
      importance = ifelse(is.na(importance_code), NA_character_,
                          importance_levels[importance_code + 1]),
      involvement = ifelse(is.na(involvement_code), NA_character_,
                           involvement_levels[involvement_code + 1])
    )
  long
})
clean_data$ustime <- ustime_list

####################################################################
#  Salaire
####################################################################
# The current questionnaire stores salary as ordered bands (AO01-AO16).
# Keep the observed band as the primary variable, and derive a midpoint-based
# numeric approximation for descriptive summaries and FTE comparisons.
salary_code <- as.character(raw_data$issalary)

clean_data$salary_band <- factor(
  unname(salary_band_labels[salary_code]),
  levels = salary_band_labels,
  ordered = TRUE
)

# Salary interval as declared in the questionnaire.
# Bounds that are not known because the class is open-ended remain NA.
clean_data$salary_band_low  <- unname(salary_band_lower[salary_code])
clean_data$salary_midpoint  <- unname(salary_band_midpoints[salary_code])
clean_data$salary_band_high <- unname(salary_band_upper[salary_code])

# Backward-compatible alias used by the basic gross-salary summary.
# This is NOT an exact observed salary: it is the midpoint estimate of the
# respondent's declared salary band.
clean_data$salary_raw <- clean_data$salary_midpoint

# Convert the whole salary interval to a 100% full-time-equivalent (FTE).
# Example: declared 80-89,999 CHF at 50% -> approximately 160-180k CHF FTE.
# Open-ended bounds stay NA, while the midpoint estimate is still available.
# Only respondents with a work rate >= SALARY_MIN_WORKRATE (50%) get an FTE salary:
# dividing by a very small rate would multiply the imprecision of the band.
valid_rate <- !is.na(clean_data$plrate) & clean_data$plrate >= SALARY_MIN_WORKRATE

clean_data$salary_fte_low <- ifelse(
  valid_rate & !is.na(clean_data$salary_band_low),
  clean_data$salary_band_low / clean_data$plrate * 100,
  NA_real_
)

clean_data$salary_fte_mid <- ifelse(
  valid_rate & !is.na(clean_data$salary_midpoint),
  clean_data$salary_midpoint / clean_data$plrate * 100,
  NA_real_
)

clean_data$salary_fte_high <- ifelse(
  valid_rate & !is.na(clean_data$salary_band_high),
  clean_data$salary_band_high / clean_data$plrate * 100,
  NA_real_
)

# Open-ended top band (">= 200 000"): the true amount is unknown, so scaling it
# by the work rate is meaningless. It is kept at its working values (200k /
# 205k), i.e. it stays ">= 200 000" in FTE too.
is_top_band <- valid_rate & salary_code %in% "AO16"
clean_data$salary_fte_low[is_top_band] <- clean_data$salary_band_low[is_top_band]
clean_data$salary_fte_mid[is_top_band] <- clean_data$salary_midpoint[is_top_band]

# Backward-compatible alias: all existing salary plots/tables now use the
# midpoint-based FTE estimate without requiring changes elsewhere.
clean_data$salary <- clean_data$salary_fte_mid

# Alias retained for traceability of the original LimeSurvey answer code.
clean_data$salary_category <- salary_code

####################################################################
#  Satisfaction au travail — globale
####################################################################
clean_data$worksatisfction <- factor(
  work_satisfaction_levels[as.integer(raw_data$issatisf)],
  levels = work_satisfaction_levels
)

####################################################################
#  Satisfaction au travail — détaillée (issatisf2)
####################################################################
issatisf_cols <- grep("^issatisf2\\.[0-9]+\\.$", names(raw_data), value = TRUE)

issatisf_mat <- as.data.frame(lapply(raw_data[issatisf_cols], function(x) {
  suppressWarnings(as.integer(as.character(x)))
}))

clean_data$issatisf2 <- lapply(seq_len(nrow(issatisf_mat)), function(i) {
  codes <- as.integer(issatisf_mat[i, ])
  data.frame(
    item_id = seq_along(codes),
    item    = if (length(satisf_items) >= length(codes))
                satisf_items[seq_along(codes)] else NA_character_,
    code    = codes,
    label   = ifelse(is.na(codes), NA_character_, satisf_levels[codes]),
    stringsAsFactors = FALSE
  )
})

# ══════════════════════════════════════════════════════════════════
#  PATCH_02_cleaning_derived_vars.R
#  → À COLLER À LA FIN de 02_cleaning.R (avant le write_csv)
#  Variables dérivées selon protocole section 5.2
# ══════════════════════════════════════════════════════════════════

####################################################################
#  Variables dérivées (protocole 5.2)
####################################################################

### Salaire BRUT approximé (avant normalisation FTE)
# `salary_raw` is already created above from the salary-band midpoint.
# `salary` is the corresponding approximate full-time-equivalent value.

### Career stage (protocole 5.2.2.2)
# Regroupement des années d'expérience en stades de carrière.
# Bornes provisoires (à ajuster selon la distribution empirique
# des données réelles, cf. protocole : "data-driven")
career_stage_levels <- c("Early-career (0-5 y)",
                         "Mid-career (6-15 y)",
                         "Senior (16+ y)")

clean_data$career_stage <- cut(
  clean_data$plyexp,
  breaks = c(-Inf, 5, 15, Inf),
  labels = career_stage_levels,
  right  = TRUE
)

### Groupe d'âge (utilisé pour stratifications, cf. 5.3.3 "age group")
age_group_levels <- c("< 30", "30-39", "40-49", "50-59", "60+")
clean_data$age_group <- cut(
  clean_data$age,
  breaks = c(-Inf, 29, 39, 49, 59, Inf),
  labels = age_group_levels,
  right  = TRUE
)

### Groupe d'expérience (déplacé ici depuis 05_plots_advanced.R
#   pour être une vraie variable dérivée réutilisable)
exp_group_levels <- c("0-5 years", "6-10 years", "11-20 years", "20+ years")
clean_data$exp_group <- cut(
  clean_data$plyexp,
  breaks = c(0, 5, 10, 20, Inf),
  labels = exp_group_levels,
  right = TRUE, include.lowest = TRUE
)

### Membre SSS (booléen dérivé — utilisé pour dupliquer les analyses
#   sur la sous-population "SSS members" comme demandé au protocole §5)
clean_data$is_sss_member <- !is.na(clean_data$sssmember) &
  clean_data$sssmember != "Not a member"

####################################################################
#  Éligibilité (protocole §3 et §4.3.2)
####################################################################
# Inclusion (at least one of):
#   (a) currently employed (full-time, part-time or self-employed) in a
#       statistics-related role, or previously held such a role and is now
#       retired or temporarily unemployed;
#   (b) currently employed in a role that directly involves the use of statistics;
#   (c) currently enrolled in a PhD programme in statistics or a related field.
# Exclusion: full-time students who are not employed in a statistics-related position.
#
# The questionnaire is only addressed to people working with statistics, so the
# role itself cannot be re-checked from the data. What can be checked is the
# current status:
#   - employed / self-employed / retired / unemployed / other non-student status
#     -> included (retired and unemployed respondents describe their previous role);
#   - full-time student without a job -> excluded (a PhD student enrolled in a
#     statistics programme, criterion c, cannot be identified from the data and
#     would have to be re-included by hand);
#   - missing status -> none of the criteria can be established -> excluded.
#
# Publication rule (draft \u00a72.2, decided by the authors): the analysis sample
# is restricted to people who currently hold a job (employed or self-employed)
# in Switzerland. Retired, unemployed, full-time students and other
# non-working statuses are excluded, as are respondents who do not work or
# work outside Switzerland. The complete questionnaires BEFORE these
# exclusions are kept in `status_data`, used only for the outputs that
# describe employment status and place of work (who works, who is retired\u2026).
status_data <- clean_data

has_job  <- clean_data$job_status %in% c("Employed", "Self-employed")
works_ch <- !(as.character(clean_data$dmwork) %in% c("I do not work", "Not in Switzerland"))
eligible <- has_job & works_ch

# Sample flow (Table 2 of the publication; written by 09_publication_extras.R).
# Rows follow Table 2 of the draft; the "starting sample" row only appears
# when it differs from the number of complete questionnaires.
sample_flow <- data.frame(
  Step = c("Participants who started the questionnaire",
           "Questionnaires with all fields answered",
           "Complete questionnaires used as the starting sample",
           "\u2013 retired / unemployed / full-time students (or other / missing status)",
           "\u2013 not working, or working outside Switzerland",
           "Eligible respondents (analysis sample)",
           "of which with a salary and a work rate of at least 50%"),
  n = c(N_STARTED,
        N_COMPLETE,
        nrow(clean_data),
        sum(!has_job),
        sum(has_job & !works_ch),
        sum(eligible),
        sum(eligible & !is.na(clean_data$salary_fte_mid))),
  stringsAsFactors = FALSE
)
if (nrow(clean_data) == N_COMPLETE) sample_flow <- sample_flow[-3, ]
rownames(sample_flow) <- NULL
message("✔ Éligibilité (protocole) : ", sum(eligible), " / ", nrow(clean_data),
        " répondants retenus (", sum(!eligible), " exclus)")
if (nrow(clean_data) > 20 && sum(eligible) < 0.5 * nrow(clean_data)) {
  warning("Plus de la moitié des répondants sont exclus (", sum(eligible), " / ",
          nrow(clean_data), "). Cela vient en général d'un problème de format ",
          "d'export plutôt que des critères d'éligibilité — à vérifier.")
}

# Excluded records are kept in a separate object for transparency (protocol §4.3.2)
excluded_data <- clean_data[!eligible, , drop = FALSE]
clean_data <- clean_data[eligible, , drop = FALSE]
rownames(clean_data) <- NULL

####################################################################
#  Export CSV intermédiaire (optionnel)
####################################################################
readr::write_csv(clean_data, "my_df.csv")

message("✔ clean_data prêt — ", nrow(clean_data), " lignes")
