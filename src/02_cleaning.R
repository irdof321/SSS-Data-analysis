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
raw_data   <- read.csv(data_file, header = TRUE, sep = ";")
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
clean_data$sssknow <- as.logical(raw_data$sssknow)

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

clean_data$study_location <- origin_from_swiss(
  raw_data$trcontswiss, raw_data$trreg
)

# Domaines de formation (bitmap multi-réponse)
cols <- paste0("trarea.", 1:19, ".")
training_fields_bitmap <- as.matrix(
  data.frame(lapply(raw_data[, cols, drop = FALSE], to_int01))
)
storage.mode(training_fields_bitmap) <- "integer"

other_col  <- "trarea.other"
other_text <- trimws(as.character(raw_data[[other_col]]))
other_text[is.na(other_text) | other_text == ""] <- NA_character_

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
cont_cols   <- paste0("trcont.", 1:6, ".")
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

clean_data$trcont2 <- rowSums(cont_bitmap[, 2:6, drop = FALSE]) > 0L

####################################################################
#  Emploi
####################################################################
clean_data$employed <- as.logical(raw_data$plemployed)

clean_data$job_status <- ifelse(
  raw_data$plemployed == 1,
  "Employed",
  employment_status_level[raw_data$plstatus - 1]
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
x <- as.character(raw_data$issalary)
x <- gsub("'", "", x)
x <- gsub(" ", "", x)
x <- gsub(",", ".", x)
clean_data$salary <- suppressWarnings(as.numeric(x)) /
  clean_data$plrate * 100

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

### Salaire BRUT (variable de base, avant normalisation)
# NOTE: la variable `salary` existante est déjà la dérivée normalisée 100%.
# On garde ici le salaire brut tel que déclaré.
x_raw <- as.character(raw_data$issalary)
x_raw <- gsub("'", "", x_raw)
x_raw <- gsub(" ", "", x_raw)
x_raw <- gsub(",", ".", x_raw)
clean_data$salary_raw <- suppressWarnings(as.numeric(x_raw))

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
#  Export CSV intermédiaire (optionnel)
####################################################################
readr::write_csv(clean_data, "my_df.csv")

message("✔ clean_data prêt — ", nrow(clean_data), " lignes")
