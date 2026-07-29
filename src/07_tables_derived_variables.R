# ══════════════════════════════════════════════════════════════════
#  07_tables_derived_variables.R — Tables sur variables dérivées
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R, 06_tables.R
#  (réutilise style_table, save_gt, sss_blue/sss_dark, tab_dir, etc.)
#
#  Couvre deux sections du protocole, analysées sur la population
#  complète (pas de split SSS members — objectifs distincts, orientés
#  "parcours type" / marché du travail plutôt que profil de membre) :
#
#  §5.3.1 — Career information for students exploring the statistical
#  profession :
#    1. Educational attainment and career position
#    2. Job titles within activity-based job profiles
#    3. Distribution of field of study by gender and location
#    → Sortie : tab_dir/career_pathways/
#
#  §5.3.2 — Labour market needs: skills, sectors, and career paths :
#    1. Importance of statistical activities and work-related skills
#       (overall + stratified by sector)
#    2. Seniority and experience by sector
#    3. Participation in continuous education
#       (overall + stratified by sector, seniority, experience)
#    → Sortie : tab_dir/labour_market/
# ══════════════════════════════════════════════════════════════════
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gt)

tables_dir     <<- file.path(tab_dir, "career_pathways")
current_pop_n  <<- nrow(clean_data)
current_pop_lb <<- "All respondents"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ══════════════════════════════════════════════════════════════════
#  Variables dérivées propres à cette analyse
# ══════════════════════════════════════════════════════════════════

# ── Responsabilité managériale (dérivée de plsenior) ───────────────
clean_data$is_manager <- clean_data$plsenior %in%
  c("Lower management", "Middle management", "Top management")

# ── Profil d'activité (dérivé de ustime — rules-based, TBD) ───────
#    Classification heuristique basée sur l'importance perçue des 6
#    thèmes statistiques. À valider/affiner sur données réelles.
profile_order <- c("Data Analyst", "Statistician",
                   "Data Scientist / Engineer", "Manager / Supervisor",
                   "Generalist")

df_scores <- clean_data |>
  mutate(.id = row_number()) |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code)) |>
  select(.id, theme_id, importance_code) |>
  tidyr::pivot_wider(names_from = theme_id, values_from = importance_code,
                     names_prefix = "theme_") |>
  rename(cleaning = theme_1, descriptive = theme_2, inferential = theme_3,
         modeling = theme_4, automation = theme_5, supervision = theme_6) |>
  mutate(
    job_profile = case_when(
      supervision >= 3 & supervision == pmax(cleaning, descriptive, inferential,
                                             modeling, automation, supervision)
      ~ "Manager / Supervisor",
      (modeling >= 3 | automation >= 3) &
        pmax(modeling, automation) >= pmax(cleaning, descriptive, inferential)
      ~ "Data Scientist / Engineer",
      inferential >= 3 & inferential >= pmax(cleaning, descriptive, modeling)
      ~ "Statistician",
      (descriptive >= 3 | cleaning >= 3) &
        pmax(descriptive, cleaning) >= pmax(inferential, modeling)
      ~ "Data Analyst",
      TRUE ~ "Generalist"
    ),
    job_profile = factor(job_profile, levels = profile_order)
  )

clean_data <- clean_data |>
  mutate(.id = row_number()) |>
  left_join(df_scores |> select(.id, job_profile), by = ".id") |>
  select(-.id)

message("✔ Variables dérivées ajoutées : is_manager, job_profile")

# ══════════════════════════════════════════════════════════════════
#  Helpers spécifiques à ce fichier
# ══════════════════════════════════════════════════════════════════

# ── Cross-tab générique (counts, colonne Total, ordre imposable) ──
make_crosstab_table <- function(df, rowvar, colvar, title, filename,
                                subtitle  = NULL,
                                row_label = NULL,
                                row_order = NULL,
                                col_order = NULL,
                                sort_rows_by_total = TRUE,
                                wrap_width = NULL) {
  dd <- df |>
    filter(!is.na(.data[[rowvar]]), !is.na(.data[[colvar]]))
  
  wide <- dd |>
    count(.data[[rowvar]], .data[[colvar]], name = "N") |>
    tidyr::pivot_wider(names_from = all_of(colvar), values_from = N,
                       values_fill = 0)
  
  row_col <- names(wide)[1]
  val_cols <- setdiff(names(wide), row_col)
  
  if (!is.null(col_order)) val_cols <- intersect(col_order, val_cols)
  
  wide <- wide |>
    rowwise() |>
    mutate(Total = sum(c_across(all_of(val_cols)))) |>
    ungroup()
  
  if (!is.null(row_order)) {
    wide <- wide |>
      mutate(.ord = match(.data[[row_col]], row_order)) |>
      arrange(.ord) |>
      select(-.ord)
  } else if (sort_rows_by_total) {
    wide <- wide |> arrange(desc(Total))
  }
  
  wide <- wide |> select(all_of(row_col), all_of(val_cols), Total)
  names(wide)[1] <- (row_label %||% rowvar)
  
  if (!is.null(wrap_width)) {
    wide[[1]] <- stringr::str_wrap(as.character(wide[[1]]), width = wrap_width)
  }
  
  gt_obj <- wide |>
    gt() |>
    fmt_integer(columns = c(all_of(val_cols), Total)) |>
    cols_align(align = "left", columns = 1) |>
    cols_align(align = "right", columns = c(all_of(val_cols), Total)) |>
    tab_style(
      style     = list(cell_text(weight = "bold")),
      locations = cells_body(columns = Total)
    ) |>
    tab_spanner(label = colvar, columns = all_of(val_cols)) |>
    grand_summary_rows(
      columns = c(all_of(val_cols), Total),
      fns     = list(Total = ~ sum(.)),
      fmt     = ~ fmt_integer(.)
    ) |>
    data_color(
      columns = Total,
      palette = c("white", sss_blue),
      alpha   = 0.3
    ) |>
    style_table(title = title, subtitle = subtitle)
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ── Table de taux (N + N_true + %_true par groupe) ────────────────
`%||%` <- function(a, b) if (is.null(a)) b else a

make_rate_table <- function(df, groupvar, boolvar, true_label, title, filename,
                            subtitle = NULL, group_order = NULL,
                            sort_by = c("order", "rate", "freq")) {
  sort_by <- match.arg(sort_by)
  
  tab <- df |>
    filter(!is.na(.data[[groupvar]])) |>
    group_by(.data[[groupvar]]) |>
    summarise(
      N       = n(),
      N_true  = sum(.data[[boolvar]], na.rm = TRUE),
      `%`     = N_true / N,
      .groups = "drop"
    )
  
  names(tab)[1] <- groupvar
  names(tab)[names(tab) == "N_true"] <- paste0("N ", true_label)
  
  if (!is.null(group_order)) {
    tab <- tab |>
      mutate(.ord = match(.data[[groupvar]], group_order)) |>
      arrange(.ord) |>
      select(-.ord)
  } else if (sort_by == "rate") {
    tab <- tab |> arrange(desc(`%`))
  } else if (sort_by == "freq") {
    tab <- tab |> arrange(desc(N))
  }
  
  gt_obj <- tab |>
    gt() |>
    fmt_integer(columns = c(N, !!paste0("N ", true_label))) |>
    fmt_percent(columns = `%`, decimals = 1) |>
    cols_align(align = "left", columns = 1) |>
    cols_align(align = "right", columns = c(N, !!paste0("N ", true_label), `%`)) |>
    data_color(
      columns = `%`,
      palette = c("white", sss_blue),
      alpha   = 0.4
    ) |>
    style_table(title = title, subtitle = subtitle)
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ── Table groupée (row groups) : top-N par catégorie ──────────────
make_grouped_topn_table <- function(df, groupvar, itemvar, n_top,
                                    title, filename,
                                    subtitle    = NULL,
                                    group_order = NULL,
                                    group_label = NULL,
                                    item_label  = NULL) {
  dd <- df |> filter(!is.na(.data[[groupvar]]), !is.na(.data[[itemvar]]))
  
  tab <- dd |>
    count(.data[[groupvar]], .data[[itemvar]], name = "N") |>
    group_by(.data[[groupvar]]) |>
    slice_max(N, n = n_top, with_ties = FALSE) |>
    ungroup()
  
  names(tab)[1:2] <- c(group_label %||% groupvar, item_label %||% itemvar)
  grp_col <- names(tab)[1]
  
  if (!is.null(group_order)) {
    tab <- tab |>
      mutate(.ord = match(.data[[grp_col]], group_order)) |>
      arrange(.ord, desc(N)) |>
      select(-.ord)
  } else {
    tab <- tab |> arrange(desc(N))
  }
  
  gt_obj <- tab |>
    gt(groupname_col = names(tab)[1]) |>
    fmt_integer(columns = N) |>
    cols_align(align = "left", columns = 2) |>
    cols_align(align = "right", columns = N) |>
    tab_style(
      style = list(
        cell_fill(color = sss_dark),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_row_groups()
    ) |>
    data_color(
      columns = N,
      palette = c("white", sss_blue),
      alpha   = 0.4
    ) |>
    style_table(title = title, subtitle = subtitle)
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ══════════════════════════════════════════════════════════════════
#  5.3.1 — 1) EDUCATIONAL ATTAINMENT AND CAREER POSITION
#  Work position, seniority levels, and managerial responsibilities
#  by highest degree obtained.
# ══════════════════════════════════════════════════════════════════

# a) Work position (job title) by highest degree
make_crosstab_table(clean_data, "job_role", "trlvl",
                    title     = "Work position by highest degree obtained",
                    filename  = "career_jobrole_by_degree.png",
                    row_label = "Job title",
                    col_order = education_level,
                    subtitle  = "Counts — sorted by total respondents")

# b) Seniority level by highest degree
make_crosstab_table(clean_data, "plsenior", "trlvl",
                    title     = "Seniority level by highest degree obtained",
                    filename  = "career_seniority_by_degree.png",
                    row_label = "Seniority level",
                    row_order = seniority_level_levels,
                    col_order = education_level,
                    sort_rows_by_total = FALSE)

# c) Managerial responsibility rate by highest degree
make_rate_table(clean_data, "trlvl", "is_manager", "managers",
                title       = "Managerial responsibility by highest degree obtained",
                filename    = "career_manager_rate_by_degree.png",
                subtitle    = "Share of respondents in a managerial position (lower/middle/top management)",
                group_order = education_level)

message("✔ 5.3.1 (1) — Educational attainment and career position")

# ══════════════════════════════════════════════════════════════════
#  5.3.1 — 2) JOB TITLES WITHIN ACTIVITY-BASED JOB PROFILES
#  Work position by statistical activities importance.
# ══════════════════════════════════════════════════════════════════

# a) Top 5 job titles per activity-based profile (grouped table)
make_grouped_topn_table(clean_data, "job_profile", "job_role", n_top = 5,
                        title       = "Most common job titles within each activity-based profile",
                        filename    = "career_jobtitles_by_profile.png",
                        subtitle    = "Top 5 job titles per profile — profiles are rules-based on perceived importance of statistical activities (protocol 5.2, provisional)",
                        group_order = profile_order,
                        group_label = "Job profile",
                        item_label  = "Job title")

# b) Mean perceived importance of each statistical activity, by profile
df_us_profile <- clean_data |>
  mutate(.id = row_number()) |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code), !is.na(job_profile)) |>
  mutate(theme = factor(theme, levels = theme_levels))

imp_by_profile <- df_us_profile |>
  group_by(job_profile, theme) |>
  summarise(mean_imp = mean(importance_code, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = theme, values_from = mean_imp)

n_by_profile <- clean_data |>
  filter(!is.na(job_profile)) |>
  count(job_profile, name = "N")

tab_imp_profile <- imp_by_profile |>
  left_join(n_by_profile, by = "job_profile") |>
  mutate(job_profile = factor(job_profile, levels = profile_order)) |>
  arrange(job_profile) |>
  relocate(N, .after = job_profile)

names(tab_imp_profile)[1] <- "Job profile"
names(tab_imp_profile) <- stringr::str_wrap(names(tab_imp_profile), width = 20)

theme_cols <- setdiff(names(tab_imp_profile), c("Job profile", "N"))

gt_imp_profile <- tab_imp_profile |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = all_of(theme_cols), decimals = 2) |>
  cols_align(align = "left", columns = `Job profile`) |>
  cols_align(align = "right", columns = c(N, all_of(theme_cols))) |>
  data_color(
    columns = all_of(theme_cols),
    palette = c("white", "#1a7a4a"),
    domain  = c(0, 4)
  ) |>
  style_table(
    title    = "Mean perceived importance of statistical activities, by job profile",
    subtitle = "Scale: 0 (Not at all important) to 4 (Very important)"
  )

save_gt(gt_imp_profile, "career_importance_by_profile.png")

message("✔ 5.3.1 (2) — Job titles within activity-based job profiles")

# ══════════════════════════════════════════════════════════════════
#  5.3.1 — 3) DISTRIBUTION OF FIELD OF STUDY BY GENDER AND LOCATION
# ══════════════════════════════════════════════════════════════════

df_field <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = ".field") |>
  filter(!is.na(.field), .field != "")

# a) Field of study by gender
wide_gender <- df_field |>
  filter(!is.na(dmgender)) |>
  count(.field, dmgender, name = "N") |>
  tidyr::pivot_wider(names_from = dmgender, values_from = N, values_fill = 0) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)))) |>
  ungroup() |>
  arrange(desc(Total))

names(wide_gender)[1] <- "Field of study"
gender_cols <- setdiff(names(wide_gender), c("Field of study", "Total"))

gt_field_gender <- wide_gender |>
  gt() |>
  fmt_integer(columns = c(all_of(gender_cols), Total)) |>
  cols_align(align = "left", columns = `Field of study`) |>
  cols_align(align = "right", columns = c(all_of(gender_cols), Total)) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = Total)
  ) |>
  tab_spanner(label = "Gender", columns = all_of(gender_cols)) |>
  grand_summary_rows(
    columns = c(all_of(gender_cols), Total),
    fns     = list(Total = ~ sum(.)),
    fmt     = ~ fmt_integer(.)
  ) |>
  style_table(
    title    = "Distribution of field of study by gender",
    subtitle = "Multiple fields of study allowed per respondent — counts"
  )

save_gt(gt_field_gender, "career_field_by_gender.png")

# b) Field of study by location of graduation
wide_location <- df_field |>
  filter(!is.na(study_location)) |>
  count(.field, study_location, name = "N") |>
  tidyr::pivot_wider(names_from = study_location, values_from = N, values_fill = 0) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)))) |>
  ungroup() |>
  arrange(desc(Total))

names(wide_location)[1] <- "Field of study"
loc_cols <- setdiff(names(wide_location), c("Field of study", "Total"))

gt_field_location <- wide_location |>
  gt() |>
  fmt_integer(columns = c(all_of(loc_cols), Total)) |>
  cols_align(align = "left", columns = `Field of study`) |>
  cols_align(align = "right", columns = c(all_of(loc_cols), Total)) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = Total)
  ) |>
  tab_spanner(label = "Location of graduation", columns = all_of(loc_cols)) |>
  grand_summary_rows(
    columns = c(all_of(loc_cols), Total),
    fns     = list(Total = ~ sum(.)),
    fmt     = ~ fmt_integer(.)
  ) |>
  style_table(
    title    = "Distribution of field of study by location of graduation",
    subtitle = "Multiple fields of study allowed per respondent — counts"
  )

save_gt(gt_field_location, "career_field_by_location.png")

message("✔ 5.3.1 (3) — Distribution of field of study by gender and location")
message("✔ 5.3.1 — tables dans ", tables_dir)


# ══════════════════════════════════════════════════════════════════
#  Protocole §5.3.2 — Labour market needs: skills, sectors, and
#  career paths :
#    1. Importance of statistical activities and work-related skills
#       (overall + stratified by sector)
#    2. Seniority and experience by sector
#    3. Participation in continuous education
#       (overall + stratified by sector, seniority, experience)
#
#  Sortie : tab_dir/labour_market/
# ══════════════════════════════════════════════════════════════════

tables_dir     <<- file.path(tab_dir, "labour_market")
current_pop_n  <<- nrow(clean_data)
current_pop_lb <<- "All respondents"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ── Secteurs éligibles pour stratification (n >= 5) ────────────────
#    Évite des cellules non interprétables sur des secteurs à
#    très faible effectif (ex: "Medical technologies", n=1).
sector_counts <- clean_data |>
  filter(!is.na(plsector), as.character(plsector) != "None") |>
  count(plsector, name = "N")

sectors_eligible <- sector_counts |> filter(N >= 5) |> pull(plsector) |> as.character()
n_sectors_excluded <- sum(sector_counts$N < 5)

sector_note <- paste0(
  "Sectors with fewer than 5 respondents are excluded (",
  n_sectors_excluded, " sector(s) excluded, covering ",
  sum(sector_counts$N[sector_counts$N < 5]), " respondent(s))."
)

message("✔ Secteurs éligibles (n≥5) : ", length(sectors_eligible), " / ",
        nrow(sector_counts))

# ══════════════════════════════════════════════════════════════════
#  5.3.2 — 1) IMPORTANCE OF STATISTICAL ACTIVITIES AND
#             WORK-RELATED SKILLS (overall + by sector)
# ══════════════════════════════════════════════════════════════════

# a) Statistical activities — overall (recap, self-contained section)
df_us_all <- clean_data |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code), !is.na(involvement_code)) |>
  mutate(
    importance  = factor(importance,  levels = importance_levels),
    involvement = factor(involvement, levels = involvement_levels)
  )

us_overall <- df_us_all |>
  group_by(theme) |>
  summarise(
    N                 = n(),
    `Mean importance` = mean(importance_code, na.rm = TRUE),
    `% Important+`    = mean(importance %in% c("Important", "Very important"),
                             na.rm = TRUE),
    `% Active use`    = mean(involvement != "No use", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(`% Important+`))

names(us_overall)[1] <- "Statistical activity"
us_overall$`Statistical activity` <- stringr::str_wrap(us_overall$`Statistical activity`, width = 42)

gt_us_overall <- us_overall |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = `Mean importance`, decimals = 2) |>
  fmt_percent(columns = c(`% Important+`, `% Active use`), decimals = 1) |>
  cols_align(align = "left", columns = `Statistical activity`) |>
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Statistical activity`)) |>
  data_color(columns = `% Important+`, palette = c("white", "#1a7a4a"), alpha = 0.4) |>
  data_color(columns = `% Active use`, palette = c("white", sss_blue),  alpha = 0.4) |>
  style_table(
    title    = "Statistical activities — overall importance and use",
    subtitle = "Sorted by perceived importance"
  )

save_gt(gt_us_overall, "labour_activities_overall.png")

# b) Work-related skills — overall
make_multi_table(clean_data, "skills",
                 "Work-related skills — overall",
                 "labour_skills_overall.png",
                 wrap_width = 45)

# c) Statistical activities — importance matrix by sector
df_us_sector <- clean_data |>
  filter(as.character(plsector) %in% sectors_eligible) |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code)) |>
  mutate(theme = factor(theme, levels = theme_levels))

n_by_sector <- clean_data |>
  filter(as.character(plsector) %in% sectors_eligible) |>
  count(plsector, name = "N")

imp_by_sector <- df_us_sector |>
  group_by(plsector, theme) |>
  summarise(mean_imp = mean(importance_code, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = theme, values_from = mean_imp) |>
  left_join(n_by_sector, by = "plsector") |>
  relocate(N, .after = plsector) |>
  arrange(desc(N))

names(imp_by_sector)[1] <- "Sector"
theme_cols <- intersect(theme_levels, names(imp_by_sector))
names(imp_by_sector)[names(imp_by_sector) %in% theme_cols] <-
  stringr::str_wrap(names(imp_by_sector)[names(imp_by_sector) %in% theme_cols], width = 18)
theme_cols_wrapped <- stringr::str_wrap(theme_cols, width = 18)

gt_imp_sector <- imp_by_sector |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = all_of(theme_cols_wrapped), decimals = 2) |>
  cols_align(align = "left", columns = Sector) |>
  cols_align(align = "right", columns = c(N, all_of(theme_cols_wrapped))) |>
  data_color(
    columns = all_of(theme_cols_wrapped),
    palette = c("white", "#1a7a4a"),
    domain  = c(0, 4)
  ) |>
  style_table(
    title    = "Mean perceived importance of statistical activities, by sector",
    subtitle = paste0("Scale: 0 (Not at all important) to 4 (Very important). ", sector_note)
  )

save_gt(gt_imp_sector, "labour_activities_by_sector.png")

# d) Work-related skills — share of respondents, by sector
df_sk_sector <- clean_data |>
  filter(as.character(plsector) %in% sectors_eligible) |>
  tidyr::unnest_longer(skills, values_to = ".sk") |>
  filter(!is.na(.sk), .sk != "")

sk_by_sector <- df_sk_sector |>
  count(plsector, .sk, name = "n") |>
  left_join(n_by_sector, by = "plsector") |>
  mutate(pct = n / N) |>
  select(plsector, .sk, pct) |>
  tidyr::pivot_wider(names_from = .sk, values_from = pct, values_fill = 0) |>
  left_join(n_by_sector, by = "plsector") |>
  relocate(N, .after = plsector) |>
  arrange(desc(N))

names(sk_by_sector)[1] <- "Sector"
skill_cols <- intersect(skills_levels, names(sk_by_sector))
skill_labels <- c(
  "Statistical programming (R, SAS, Python (Statistics and ML libraries), SPSS, Stata, etc.)" = "Stat. programming",
  "Other programming (C, C++, Java, Python, etc.)" = "Other programming",
  "Data visualization (Power BI, Tableau, Looker Studio, etc.)" = "Data viz",
  "Scientific writing and/or research" = "Sci. writing",
  "Project management" = "Project mgmt",
  "Time management" = "Time mgmt"
)
names(sk_by_sector)[match(names(skill_labels), names(sk_by_sector))] <- skill_labels

gt_sk_sector <- sk_by_sector |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_percent(columns = all_of(unname(skill_labels)), decimals = 0) |>
  cols_align(align = "left", columns = Sector) |>
  cols_align(align = "right", columns = c(N, all_of(unname(skill_labels)))) |>
  data_color(
    columns = all_of(unname(skill_labels)),
    palette = c("white", sss_blue),
    domain  = c(0, 1)
  ) |>
  style_table(
    title    = "Work-related skills — share of respondents, by sector",
    subtitle = paste0("Multiple skills allowed per respondent. ", sector_note)
  )

save_gt(gt_sk_sector, "labour_skills_by_sector.png")

message("✔ 5.3.2 (1) — Importance of statistical activities and work-related skills")

# ══════════════════════════════════════════════════════════════════
#  5.3.2 — 2) SENIORITY AND EXPERIENCE BY SECTOR
# ══════════════════════════════════════════════════════════════════

df_sector_pool <- clean_data |> filter(as.character(plsector) %in% sectors_eligible)

# a) Years of professional experience by sector
exp_by_sector <- df_sector_pool |>
  filter(!is.na(plyexp)) |>
  group_by(plsector) |>
  summarise(
    N      = n(),
    Mean   = mean(plyexp),
    SD     = sd(plyexp),
    Median = median(plyexp),
    .groups = "drop"
  ) |>
  arrange(desc(Mean))

names(exp_by_sector)[1] <- "Sector"

gt_exp_sector <- exp_by_sector |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = c(Mean, SD, Median), decimals = 1) |>
  cols_align(align = "left", columns = Sector) |>
  cols_align(align = "right", columns = c(N, Mean, SD, Median)) |>
  data_color(columns = Mean, palette = c("white", sss_blue), alpha = 0.4) |>
  style_table(
    title    = "Years of professional experience, by sector",
    subtitle = paste0("Sorted by mean years of experience. ", sector_note)
  )

save_gt(gt_exp_sector, "labour_experience_by_sector.png")

# b) Seniority level by sector
make_crosstab_table(df_sector_pool, "plsector", "plsenior",
                    title     = "Seniority level, by sector",
                    filename  = "labour_seniority_by_sector.png",
                    row_label = "Sector",
                    col_order = seniority_level_levels,
                    subtitle  = sector_note)

# c) Managerial responsibility rate by sector
make_rate_table(df_sector_pool, "plsector", "is_manager", "managers",
                title    = "Managerial responsibility, by sector",
                filename = "labour_manager_rate_by_sector.png",
                subtitle = paste0("Share of respondents in a managerial position. ", sector_note),
                sort_by  = "rate")

message("✔ 5.3.2 (2) — Seniority and experience by sector")

# ══════════════════════════════════════════════════════════════════
#  5.3.2 — 3) PARTICIPATION IN CONTINUOUS EDUCATION
#     (overall + by sector, seniority, and years of experience)
# ══════════════════════════════════════════════════════════════════

cont_ed_note <- "Respondents who selected more than one continuous education option are excluded (ambiguous single-choice derivation)."

# a) Overall
make_freq_table(clean_data, "continuous_education",
                "Continuous education — overall",
                "labour_continuing_ed_overall.png",
                wrap_width = 40,
                subtitle = cont_ed_note)

# b) By sector
make_crosstab_table(df_sector_pool, "plsector", "continuous_education",
                    title     = "Continuous education, by sector",
                    filename  = "labour_continuing_ed_by_sector.png",
                    row_label = "Sector",
                    col_order = continuous_education_levels,
                    subtitle  = paste0(cont_ed_note, " ", sector_note))

# c) By seniority level
make_crosstab_table(clean_data, "plsenior", "continuous_education",
                    title     = "Continuous education, by seniority level",
                    filename  = "labour_continuing_ed_by_seniority.png",
                    row_label = "Seniority level",
                    row_order = seniority_level_levels,
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# d) By years of experience (derived exp_group)
make_crosstab_table(clean_data, "exp_group", "continuous_education",
                    title     = "Continuous education, by years of experience",
                    filename  = "labour_continuing_ed_by_experience.png",
                    row_label = "Years of experience",
                    row_order = levels(clean_data$exp_group),
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

message("✔ 5.3.2 (3) — Participation in continuous education")


# ══════════════════════════════════════════════════════════════════
#  Protocole §5.3.3 — Salary levels and employment conditions across
#  the field :
#    1. Salary levels (by sector, position, degree, experience,
#       seniority, age group, region, gender)
#    2. Job satisfaction (by sector, degree, seniority, salary,
#       work rate, experience)
#    3. Employment conditions (work rate, contractual status)
#
#  Sortie : tab_dir/salary_and_conditions/
# ══════════════════════════════════════════════════════════════════

tables_dir     <<- file.path(tab_dir, "salary_and_conditions")
current_pop_n  <<- nrow(clean_data)
current_pop_lb <<- "All respondents"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ── Variables dérivées propres à cette analyse ──────────────────────

# Work rate, grouped (protocole : "work rate (percentage of full-time)")
clean_data$workrate_group <- cut(
  clean_data$plrate,
  breaks = c(0, 50, 80, 100, Inf),
  labels = c("<50%", "50-79%", "80-99%", "100%+"),
  right = FALSE, include.lowest = TRUE
)

# Salary, grouped in quartiles (pour la stratification satisfaction × salaire)
salary_quartiles <- quantile(clean_data$salary, c(0, .25, .5, .75, 1), na.rm = TRUE)
clean_data$salary_group <- cut(
  clean_data$salary,
  breaks = salary_quartiles,
  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"),
  include.lowest = TRUE
)

# Score de satisfaction numérique — échelle inversée pour que
# "plus haut = plus satisfait" (5 = Very satisfied, 1 = Not at all)
clean_data$satisf_score <- length(work_satisfaction_levels) + 1 -
  as.integer(clean_data$worksatisfction)

message("✔ Variables dérivées ajoutées : workrate_group, salary_group, satisf_score")

# ── Work region éligible (n >= 5), exclut "I do not work" ──────────
dmwork_counts <- clean_data |>
  filter(!is.na(dmwork), dmwork != "I do not work") |>
  count(dmwork, name = "N")

dmwork_eligible <- dmwork_counts |> filter(N >= 5) |> pull(dmwork) |> as.character()
n_dmwork_excluded <- sum(dmwork_counts$N < 5)

dmwork_note <- paste0(
  "Work regions (canton) with fewer than 5 respondents are excluded (",
  n_dmwork_excluded, " region(s) excluded, covering ",
  sum(dmwork_counts$N[dmwork_counts$N < 5]), " respondent(s))."
)

message("✔ Régions de travail éligibles (n≥5) : ", length(dmwork_eligible), " / ",
        nrow(dmwork_counts))

# ══════════════════════════════════════════════════════════════════
#  Helper — table numérique groupée (N, Mean, SD, Median), réutilisée
#  pour toutes les stratifications de salaire ci-dessous.
# ══════════════════════════════════════════════════════════════════

make_grouped_numeric_table <- function(df, groupvar, numvar, title, filename,
                                       subtitle    = NULL,
                                       group_order = NULL,
                                       sort_desc   = TRUE,
                                       unit        = "",
                                       min_n       = 5) {
  tab <- df |>
    filter(!is.na(.data[[groupvar]]), !is.na(.data[[numvar]])) |>
    group_by(.data[[groupvar]]) |>
    summarise(
      N      = n(),
      Mean   = mean(.data[[numvar]]),
      SD     = sd(.data[[numvar]]),
      Median = median(.data[[numvar]]),
      .groups = "drop"
    )
  
  n_before <- nrow(tab)
  tab <- tab |> filter(N >= min_n)
  n_dropped <- n_before - nrow(tab)
  
  names(tab)[1] <- groupvar
  
  if (!is.null(group_order)) {
    tab <- tab |>
      mutate(.ord = match(.data[[groupvar]], group_order)) |>
      arrange(.ord) |>
      select(-.ord)
  } else if (sort_desc) {
    tab <- tab |> arrange(desc(Mean))
  }
  
  full_subtitle <- subtitle
  if (n_dropped > 0) {
    full_subtitle <- paste0(
      subtitle %||% "",
      if (!is.null(subtitle)) " " else "",
      "(", n_dropped, " group(s) with fewer than ", min_n,
      " respondents excluded from this specific table.)"
    )
  }
  
  gt_obj <- tab |>
    gt() |>
    fmt_integer(columns = N) |>
    fmt_number(columns = c(Mean, SD, Median), decimals = 1) |>
    cols_align(align = "left", columns = 1) |>
    cols_align(align = "right", columns = c(N, Mean, SD, Median)) |>
    data_color(columns = Mean, palette = c("white", sss_blue), alpha = 0.4) |>
    style_table(title = title, subtitle = full_subtitle)
  
  if (unit != "") {
    gt_obj <- gt_obj |>
      tab_footnote(
        footnote  = paste0("Values expressed in ", unit, "."),
        locations = cells_column_labels(columns = Mean)
      )
  }
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ══════════════════════════════════════════════════════════════════
#  Helper — table de satisfaction groupée (N, Mean score, % Sat/Dissat)
# ══════════════════════════════════════════════════════════════════

make_satisfaction_by_group_table <- function(df, groupvar, title, filename,
                                             subtitle    = NULL,
                                             group_order = NULL,
                                             min_n       = 5) {
  tab <- df |>
    filter(!is.na(.data[[groupvar]]), !is.na(worksatisfction)) |>
    group_by(.data[[groupvar]]) |>
    summarise(
      N               = n(),
      `Mean score`    = mean(satisf_score, na.rm = TRUE),
      `% Satisfied`   = mean(worksatisfction %in% c("Very satisfied", "Quite satisfied"),
                             na.rm = TRUE),
      `% Dissatisfied` = mean(worksatisfction %in% c("Not quite satisfied", "Not at all satisfied"),
                              na.rm = TRUE),
      .groups = "drop"
    )
  
  n_before <- nrow(tab)
  tab <- tab |> filter(N >= min_n)
  n_dropped <- n_before - nrow(tab)
  
  names(tab)[1] <- groupvar
  
  if (!is.null(group_order)) {
    tab <- tab |>
      mutate(.ord = match(.data[[groupvar]], group_order)) |>
      arrange(.ord) |>
      select(-.ord)
  } else {
    tab <- tab |> arrange(desc(`% Satisfied`))
  }
  
  base_subtitle <- paste0(
    if (!is.null(subtitle)) paste0(subtitle, " — ") else "",
    "Satisfaction score: 1 (Not at all satisfied) to 5 (Very satisfied)"
  )
  if (n_dropped > 0) {
    base_subtitle <- paste0(
      base_subtitle, " (", n_dropped, " group(s) with fewer than ",
      min_n, " respondents excluded from this specific table.)"
    )
  }
  
  gt_obj <- tab |>
    gt() |>
    fmt_integer(columns = N) |>
    fmt_number(columns = `Mean score`, decimals = 2) |>
    fmt_percent(columns = c(`% Satisfied`, `% Dissatisfied`), decimals = 1) |>
    cols_align(align = "left", columns = 1) |>
    cols_align(align = "right", columns = c(N, `Mean score`, `% Satisfied`, `% Dissatisfied`)) |>
    data_color(columns = `% Satisfied`,    palette = c("#fce4e4", "#1a7a4a"), alpha = 0.4) |>
    data_color(columns = `% Dissatisfied`, palette = c("#e8f5e9", "#c0392b"), alpha = 0.4) |>
    style_table(title = title, subtitle = base_subtitle)
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ══════════════════════════════════════════════════════════════════
#  5.3.3 — 1) SALARY LEVELS
#  By sector, work position, degree level, years of experience,
#  seniority level, age group, work region, and gender.
# ══════════════════════════════════════════════════════════════════

# a) By sector of employment (n>=5 sectors, cf. section 5.3.2)
make_grouped_numeric_table(
  clean_data |> filter(as.character(plsector) %in% sectors_eligible),
  "plsector", "salary",
  title    = "Salary levels, by sector of employment",
  filename = "salary_by_sector.png",
  subtitle = paste0("Full-time equivalent (100% workload), in CHF. ", sector_note),
  unit     = "CHF"
)

# b) By work position (job title)
make_grouped_numeric_table(clean_data, "job_role", "salary",
                           title    = "Salary levels, by work position (job title)",
                           filename = "salary_by_jobrole.png",
                           subtitle = "Full-time equivalent (100% workload), in CHF",
                           unit     = "CHF")

# c) By degree level
make_grouped_numeric_table(clean_data, "trlvl", "salary",
                           title       = "Salary levels, by highest degree obtained",
                           filename    = "salary_by_degree.png",
                           subtitle    = "Full-time equivalent (100% workload), in CHF",
                           group_order = education_level,
                           unit        = "CHF")

# d) By years of professional experience
make_grouped_numeric_table(clean_data, "exp_group", "salary",
                           title       = "Salary levels, by years of professional experience",
                           filename    = "salary_by_experience.png",
                           subtitle    = "Full-time equivalent (100% workload), in CHF",
                           group_order = levels(clean_data$exp_group),
                           unit        = "CHF")

# e) By seniority level
make_grouped_numeric_table(clean_data, "plsenior", "salary",
                           title       = "Salary levels, by seniority level",
                           filename    = "salary_by_seniority.png",
                           subtitle    = "Full-time equivalent (100% workload), in CHF",
                           group_order = seniority_level_levels,
                           unit        = "CHF")

# f) By age group
make_grouped_numeric_table(clean_data, "age_group", "salary",
                           title       = "Salary levels, by age group",
                           filename    = "salary_by_age_group.png",
                           subtitle    = "Full-time equivalent (100% workload), in CHF",
                           group_order = levels(clean_data$age_group),
                           unit        = "CHF")

# g) By work region (canton, n>=5)
make_grouped_numeric_table(
  clean_data |> filter(as.character(dmwork) %in% dmwork_eligible),
  "dmwork", "salary",
  title    = "Salary levels, by work region (canton)",
  filename = "salary_by_work_region.png",
  subtitle = paste0("Full-time equivalent (100% workload), in CHF. ", dmwork_note),
  unit     = "CHF"
)

# h) By gender
make_grouped_numeric_table(clean_data, "dmgender", "salary",
                           title    = "Salary levels, by gender",
                           filename = "salary_by_gender.png",
                           subtitle = "Full-time equivalent (100% workload), in CHF",
                           unit     = "CHF")

message("✔ 5.3.3 (1) — Salary levels")

# ══════════════════════════════════════════════════════════════════
#  5.3.3 — 2) JOB SATISFACTION
#  By sector, degree level, seniority level, salary, work rate,
#  and years of professional experience.
# ══════════════════════════════════════════════════════════════════

# a) By sector of employment
make_satisfaction_by_group_table(
  clean_data |> filter(as.character(plsector) %in% sectors_eligible),
  "plsector",
  title    = "Job satisfaction, by sector of employment",
  filename = "satisfaction_by_sector.png",
  subtitle = sector_note
)

# b) By degree level
make_satisfaction_by_group_table(clean_data, "trlvl",
                                 title       = "Job satisfaction, by highest degree obtained",
                                 filename    = "satisfaction_by_degree.png",
                                 group_order = education_level)

# c) By seniority level
make_satisfaction_by_group_table(clean_data, "plsenior",
                                 title       = "Job satisfaction, by seniority level",
                                 filename    = "satisfaction_by_seniority.png",
                                 group_order = seniority_level_levels)

# d) By salary (quartile groups)
make_satisfaction_by_group_table(clean_data, "salary_group",
                                 title       = "Job satisfaction, by salary level (quartiles)",
                                 filename    = "satisfaction_by_salary.png",
                                 subtitle    = "Quartiles computed on full-time equivalent salary",
                                 group_order = levels(clean_data$salary_group))

# e) By work rate
make_satisfaction_by_group_table(clean_data, "workrate_group",
                                 title       = "Job satisfaction, by work rate",
                                 filename    = "satisfaction_by_workrate.png",
                                 group_order = levels(clean_data$workrate_group))

# f) By years of professional experience
make_satisfaction_by_group_table(clean_data, "exp_group",
                                 title       = "Job satisfaction, by years of professional experience",
                                 filename    = "satisfaction_by_experience.png",
                                 group_order = levels(clean_data$exp_group))

message("✔ 5.3.3 (2) — Job satisfaction")

# ══════════════════════════════════════════════════════════════════
#  5.3.3 — 3) EMPLOYMENT CONDITIONS
#  Work rate, contractual status, and other working arrangements.
#
#  NOTE IMPORTANTE : le questionnaire ne contient pas de variable
#  distinguant contrat à durée déterminée vs. indéterminée (fixed-term
#  vs. permanent), ni de statut temps plein/temps partiel en tant que
#  tel — seul le taux d'activité (plrate, %) et le statut d'emploi
#  (job_status : Employed / Self-employed / Student / Unemployed /
#  Retired) sont disponibles. Cette limite doit être mentionnée dans
#  le rapport final.
# ══════════════════════════════════════════════════════════════════

# a) Work rate — descriptive statistics (overall)
make_numeric_table(clean_data, "plrate",
                   "Work rate (% of full-time) — descriptive statistics",
                   "conditions_workrate_summary.png",
                   unit = "%")

# b) Work rate — grouped distribution
make_freq_table(clean_data, "workrate_group",
                "Work rate — distribution",
                "conditions_workrate_distribution.png",
                order_by = "level")

# c) Employment / contractual status (closest available proxy)
make_freq_table(clean_data, "job_status",
                "Employment status",
                "conditions_employment_status.png",
                subtitle = "No fixed-term vs. permanent contract variable is available in the survey — this table reports employment status only (employed / self-employed / student / unemployed / retired)")

message("✔ 5.3.3 (3) — Employment conditions")


# ══════════════════════════════════════════════════════════════════
#  Protocole §5.3.4 — Foundations for future research on the
#  statistical profession :
#    1. Associations between job role, skills, and satisfaction
#    2. Continuous education — comparisons across many groups
#
#  Sortie : tab_dir/future_research/
# ══════════════════════════════════════════════════════════════════

tables_dir     <<- file.path(tab_dir, "future_research")
current_pop_n  <<- nrow(clean_data)
current_pop_lb <<- "All respondents"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ══════════════════════════════════════════════════════════════════
#  5.3.4 — 1) ASSOCIATIONS BETWEEN JOB ROLE, SKILLS, AND SATISFACTION
# ══════════════════════════════════════════════════════════════════

# a) Job satisfaction, by job role
make_satisfaction_by_group_table(clean_data, "job_role",
                                 title    = "Job satisfaction, by job role",
                                 filename = "assoc_satisfaction_by_jobrole.png")

# b) Job satisfaction, by work-related skill held (multiple skills allowed)
df_sk_sat <- clean_data |>
  tidyr::unnest_longer(skills, values_to = ".sk") |>
  filter(!is.na(.sk), .sk != "", !is.na(worksatisfction))

sk_sat_summary <- df_sk_sat |>
  group_by(.sk) |>
  summarise(
    N               = n(),
    `Mean score`    = mean(satisf_score, na.rm = TRUE),
    `% Satisfied`   = mean(worksatisfction %in% c("Very satisfied", "Quite satisfied"),
                           na.rm = TRUE),
    `% Dissatisfied` = mean(worksatisfction %in% c("Not quite satisfied", "Not at all satisfied"),
                            na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(`Mean score`))

names(sk_sat_summary)[1] <- "Skill"
sk_sat_summary$Skill <- stringr::str_wrap(sk_sat_summary$Skill, width = 45)

gt_sk_sat <- sk_sat_summary |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = `Mean score`, decimals = 2) |>
  fmt_percent(columns = c(`% Satisfied`, `% Dissatisfied`), decimals = 1) |>
  cols_align(align = "left", columns = Skill) |>
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_body(columns = Skill)) |>
  data_color(columns = `% Satisfied`,    palette = c("#fce4e4", "#1a7a4a"), alpha = 0.4) |>
  data_color(columns = `% Dissatisfied`, palette = c("#e8f5e9", "#c0392b"), alpha = 0.4) |>
  style_table(
    title    = "Job satisfaction, by work-related skill held",
    subtitle = "Multiple skills allowed per respondent — sorted by mean satisfaction score (1-5, 5 = Very satisfied)"
  )

save_gt(gt_sk_sat, "assoc_satisfaction_by_skill.png")

# c) [Bonus, not explicitly requested] Job satisfaction, by activity-based
#    job profile — ties job role + skills signal together in one lens.
make_satisfaction_by_group_table(clean_data, "job_profile",
                                 title       = "Job satisfaction, by activity-based job profile",
                                 filename    = "assoc_satisfaction_by_jobprofile.png",
                                 subtitle    = "Bonus table (not explicitly requested by protocol) — profiles as defined in 5.3.2.2",
                                 group_order = profile_order)

message("✔ 5.3.4 (1) — Associations between job role, skills, and satisfaction")

# ══════════════════════════════════════════════════════════════════
#  5.3.4 — 2) CONTINUOUS EDUCATION — comparisons across groups
# ══════════════════════════════════════════════════════════════════

# a) By highest degree obtained
make_crosstab_table(clean_data, "trlvl", "continuous_education",
                    title     = "Continuous education, by highest degree obtained",
                    filename  = "future_contedu_by_degree.png",
                    row_label = "Degree",
                    row_order = education_level,
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# b) By field of study (multiple fields allowed per respondent)
df_field_ce <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = ".field") |>
  filter(!is.na(.field), .field != "", !is.na(continuous_education))

wide_field_ce <- df_field_ce |>
  count(.field, continuous_education, name = "N") |>
  tidyr::pivot_wider(names_from = continuous_education, values_from = N, values_fill = 0)

field_col  <- names(wide_field_ce)[1]
ce_cols    <- intersect(continuous_education_levels, names(wide_field_ce))
wide_field_ce <- wide_field_ce |>
  rowwise() |>
  mutate(Total = sum(c_across(all_of(ce_cols)))) |>
  ungroup() |>
  arrange(desc(Total)) |>
  select(all_of(field_col), all_of(ce_cols), Total)

names(wide_field_ce)[1] <- "Field of study"

gt_field_ce <- wide_field_ce |>
  gt() |>
  fmt_integer(columns = c(all_of(ce_cols), Total)) |>
  cols_align(align = "left", columns = `Field of study`) |>
  cols_align(align = "right", columns = c(all_of(ce_cols), Total)) |>
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_body(columns = Total)) |>
  tab_spanner(label = "Continuous education", columns = all_of(ce_cols)) |>
  grand_summary_rows(
    columns = c(all_of(ce_cols), Total),
    fns     = list(Total = ~ sum(.)),
    fmt     = ~ fmt_integer(.)
  ) |>
  style_table(
    title    = "Continuous education, by field of study",
    subtitle = paste0("Multiple fields of study allowed per respondent. ", cont_ed_note)
  )

save_gt(gt_field_ce, "future_contedu_by_field.png")

# c) By sector of employment (n>=5 sectors, self-contained re-derivation
#    for this section — see also labour_market/labour_continuing_ed_by_sector.png)
make_crosstab_table(
  clean_data |> filter(as.character(plsector) %in% sectors_eligible),
  "plsector", "continuous_education",
  title     = "Continuous education, by sector of employment",
  filename  = "future_contedu_by_sector.png",
  row_label = "Sector",
  col_order = continuous_education_levels,
  subtitle  = paste0(cont_ed_note, " ", sector_note)
)

# d) By activity-based job profile (as defined in section 5.3.2.2)
make_crosstab_table(clean_data, "job_profile", "continuous_education",
                    title     = "Continuous education, by activity-based job profile",
                    filename  = "future_contedu_by_jobprofile.png",
                    row_label = "Job profile",
                    row_order = profile_order,
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = paste0(cont_ed_note, " Profiles as defined in section 5.3.2.2."))

# e) By years of professional experience
make_crosstab_table(clean_data, "exp_group", "continuous_education",
                    title     = "Continuous education, by years of professional experience",
                    filename  = "future_contedu_by_experience.png",
                    row_label = "Years of experience",
                    row_order = levels(clean_data$exp_group),
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# f) By seniority level
make_crosstab_table(clean_data, "plsenior", "continuous_education",
                    title     = "Continuous education, by seniority level",
                    filename  = "future_contedu_by_seniority.png",
                    row_label = "Seniority level",
                    row_order = seniority_level_levels,
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# g) By work rate
make_crosstab_table(clean_data, "workrate_group", "continuous_education",
                    title     = "Continuous education, by work rate",
                    filename  = "future_contedu_by_workrate.png",
                    row_label = "Work rate",
                    row_order = levels(clean_data$workrate_group),
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# h) By involvement in the SSS
make_crosstab_table(clean_data, "sssmember", "continuous_education",
                    title     = "Continuous education, by involvement in the SSS",
                    filename  = "future_contedu_by_sss_involvement.png",
                    row_label = "SSS involvement",
                    row_order = involvement_level,
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# i) By age group
make_crosstab_table(clean_data, "age_group", "continuous_education",
                    title     = "Continuous education, by age group",
                    filename  = "future_contedu_by_age_group.png",
                    row_label = "Age group",
                    row_order = levels(clean_data$age_group),
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

# j) By gender
make_crosstab_table(clean_data, "dmgender", "continuous_education",
                    title     = "Continuous education, by gender",
                    filename  = "future_contedu_by_gender.png",
                    row_label = "Gender",
                    col_order = continuous_education_levels,
                    sort_rows_by_total = FALSE,
                    subtitle  = cont_ed_note)

message("✔ 5.3.4 (2) — Continuous education, comparisons across groups")


# ══════════════════════════════════════════════════════════════════
#  Protocole §5.3.5 — Identifying roles extensively using statistical
#  methods but not labelled as "statisticians" :
#    1. Job titles and activity-based job profiles
#    2. Sector of employment and field of study
#    3. Work-related skills and statistical activities
#    4. Salary and seniority
#
#  DÉFINITIONS OPÉRATIONNELLES (à valider/ajuster si besoin) :
#  - "High statistical activity" = classé dans un profil autre que
#    "Generalist" (job_profile, section 5.3.2.2) — càd au moins un
#    thème statistique jugé "Important" ou "Very important" (score
#    >= 3) et dominant. Seuls 17/300 répondants sont "Generalist".
#  - "Title mentions statistics/data" = job_role contient les mots
#    "stat" ou "data" (insensible à la casse) — couvre Statistician,
#    Senior Statistician, Biostatistician, Data Scientist, Data Analyst.
#  - Restreint aux répondants actuellement employés (employed==TRUE) :
#    exclut Student/Unemployed/Retired, qui n'ont pas d'emploi "libellé"
#    au sens du protocole (PhD student/Postdoc restent inclus s'ils
#    sont employés, ce qui est fréquent).
#  - "Hidden statistical role"   = employed & high-activity & titre
#    ne mentionnant pas stat/data (n=146)
#  - "Labelled statistical role" = employed & high-activity & titre
#    mentionnant stat/data (n=87)
#
#  Sortie : tab_dir/hidden_statistical_roles/
# ══════════════════════════════════════════════════════════════════

tables_dir     <<- file.path(tab_dir, "hidden_statistical_roles")
current_pop_n  <<- nrow(clean_data)
current_pop_lb <<- "All respondents"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ── Variables dérivées propres à cette analyse ──────────────────────
clean_data$high_stat_activity <- !is.na(clean_data$job_profile) &
  clean_data$job_profile != "Generalist"

clean_data$title_mentions_stats <- grepl("stat|data", clean_data$job_role,
                                         ignore.case = TRUE)

clean_data$hidden_role   <- clean_data$employed & clean_data$high_stat_activity &
  !clean_data$title_mentions_stats
clean_data$labelled_role <- clean_data$employed & clean_data$high_stat_activity &
  clean_data$title_mentions_stats

role_group_levels <- c("Hidden statistical role", "Labelled statistical role")
clean_data$role_group <- factor(
  case_when(
    clean_data$hidden_role   ~ "Hidden statistical role",
    clean_data$labelled_role ~ "Labelled statistical role",
    TRUE ~ NA_character_
  ),
  levels = role_group_levels
)

# Vue d'ensemble : où se situe chaque répondant employé sur ce spectre
clean_data$role_landscape <- factor(
  case_when(
    !clean_data$employed          ~ NA_character_,
    !clean_data$high_stat_activity ~ "Low statistical activity (Generalist)",
    clean_data$hidden_role         ~ "Hidden statistical role",
    clean_data$labelled_role       ~ "Labelled statistical role",
    TRUE ~ NA_character_
  ),
  levels = c("Labelled statistical role", "Hidden statistical role",
             "Low statistical activity (Generalist)")
)

message("✔ Variables dérivées ajoutées : high_stat_activity, title_mentions_stats, ",
        "hidden_role, labelled_role, role_group, role_landscape")
message("  Hidden: ", sum(clean_data$hidden_role, na.rm = TRUE),
        " | Labelled: ", sum(clean_data$labelled_role, na.rm = TRUE),
        " (among ", sum(clean_data$employed, na.rm = TRUE), " employed respondents)")

# ── Vue d'ensemble (table d'intro) ──────────────────────────────────
make_freq_table(clean_data, "role_landscape",
                "Overview: statistical activity vs. job title, among employed respondents",
                "hidden_overview_landscape.png",
                subtitle = "'Generalist' = no statistical theme rated Important/Very important (job_profile, 5.3.2.2)")

# ══════════════════════════════════════════════════════════════════
#  5.3.5 — 1) JOB TITLES AND ACTIVITY-BASED JOB PROFILES
# ══════════════════════════════════════════════════════════════════

# a) Job titles among "hidden" statistical roles (does NOT mention stats/data)
make_freq_table(
  clean_data |> filter(hidden_role),
  "job_role",
  title    = "Job titles — hidden statistical roles",
  filename = "hidden_jobtitles.png",
  subtitle = "High statistical activity, but job title does not mention statistics or data analysis"
)

# b) Job titles among "labelled" statistical roles (for contrast)
make_freq_table(
  clean_data |> filter(labelled_role),
  "job_role",
  title    = "Job titles — labelled statistical roles (for contrast)",
  filename = "hidden_jobtitles_labelled_contrast.png",
  subtitle = "High statistical activity, job title explicitly mentions statistics or data analysis"
)

# c) Activity-based job profile, hidden vs. labelled
make_crosstab_table(clean_data, "role_group", "job_profile",
                    title     = "Activity-based job profile, hidden vs. labelled statistical roles",
                    filename  = "hidden_jobprofile_by_rolegroup.png",
                    row_label = "Role group",
                    col_order = profile_order,
                    sort_rows_by_total = FALSE)

message("✔ 5.3.5 (1) — Job titles and activity-based job profiles")

# ══════════════════════════════════════════════════════════════════
#  5.3.5 — 2) SECTOR OF EMPLOYMENT AND FIELD OF STUDY
#     (description of hidden statistical roles)
# ══════════════════════════════════════════════════════════════════

# a) Sector of employment among hidden statistical roles
make_freq_table(
  clean_data |> filter(hidden_role),
  "plsector",
  title    = "Sector of employment — hidden statistical roles",
  filename = "hidden_sector.png",
  wrap_width = 35
)

# b) Field of study among hidden statistical roles (multiple allowed)
make_multi_table(
  clean_data |> filter(hidden_role),
  "training_fields_list",
  title    = "Field of study — hidden statistical roles",
  filename = "hidden_field_of_study.png"
)

message("✔ 5.3.5 (2) — Sector of employment and field of study")

# ══════════════════════════════════════════════════════════════════
#  5.3.5 — 3) WORK-RELATED SKILLS AND STATISTICAL ACTIVITIES
#     (hidden vs. labelled comparison)
# ══════════════════════════════════════════════════════════════════

# a) Work-related skills — share of respondents, hidden vs. labelled
n_by_rolegrp <- clean_data |> filter(!is.na(role_group)) |> count(role_group, name = "N")

df_sk_role <- clean_data |>
  filter(!is.na(role_group)) |>
  tidyr::unnest_longer(skills, values_to = ".sk") |>
  filter(!is.na(.sk), .sk != "")

sk_by_role <- df_sk_role |>
  count(role_group, .sk, name = "n") |>
  left_join(n_by_rolegrp, by = "role_group") |>
  mutate(pct = n / N) |>
  select(role_group, .sk, pct) |>
  tidyr::pivot_wider(names_from = .sk, values_from = pct, values_fill = 0) |>
  left_join(n_by_rolegrp, by = "role_group") |>
  relocate(N, .after = role_group)

names(sk_by_role)[1] <- "Role group"
names(sk_by_role)[match(names(skill_labels), names(sk_by_role))] <- skill_labels

gt_sk_role <- sk_by_role |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_percent(columns = all_of(unname(skill_labels)), decimals = 0) |>
  cols_align(align = "left", columns = `Role group`) |>
  cols_align(align = "right", columns = c(N, all_of(unname(skill_labels)))) |>
  data_color(columns = all_of(unname(skill_labels)), palette = c("white", sss_blue), domain = c(0, 1)) |>
  style_table(
    title    = "Work-related skills — hidden vs. labelled statistical roles",
    subtitle = "Share of respondents holding each skill. Multiple skills allowed."
  )

save_gt(gt_sk_role, "hidden_skills_by_rolegroup.png")

# b) Statistical activities — mean perceived importance, hidden vs. labelled
df_us_role <- clean_data |>
  filter(!is.na(role_group)) |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code)) |>
  mutate(theme = factor(theme, levels = theme_levels))

imp_by_role <- df_us_role |>
  group_by(role_group, theme) |>
  summarise(mean_imp = mean(importance_code, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = theme, values_from = mean_imp) |>
  left_join(n_by_rolegrp, by = "role_group") |>
  relocate(N, .after = role_group)

names(imp_by_role)[1] <- "Role group"
theme_cols_role <- intersect(theme_levels, names(imp_by_role))
names(imp_by_role)[names(imp_by_role) %in% theme_cols_role] <-
  stringr::str_wrap(names(imp_by_role)[names(imp_by_role) %in% theme_cols_role], width = 18)
theme_cols_role_wrapped <- stringr::str_wrap(theme_cols_role, width = 18)

gt_imp_role <- imp_by_role |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = all_of(theme_cols_role_wrapped), decimals = 2) |>
  cols_align(align = "left", columns = `Role group`) |>
  cols_align(align = "right", columns = c(N, all_of(theme_cols_role_wrapped))) |>
  data_color(columns = all_of(theme_cols_role_wrapped), palette = c("white", "#1a7a4a"), domain = c(0, 4)) |>
  style_table(
    title    = "Statistical activities — mean perceived importance, hidden vs. labelled roles",
    subtitle = "Scale: 0 (Not at all important) to 4 (Very important)"
  )

save_gt(gt_imp_role, "hidden_activities_by_rolegroup.png")

message("✔ 5.3.5 (3) — Work-related skills and statistical activities")

# ══════════════════════════════════════════════════════════════════
#  5.3.5 — 4) SALARY AND SENIORITY
#     (hidden vs. labelled comparison)
# ══════════════════════════════════════════════════════════════════

# a) Salary levels
make_grouped_numeric_table(clean_data, "role_group", "salary",
                           title    = "Salary levels, hidden vs. labelled statistical roles",
                           filename = "hidden_salary_by_rolegroup.png",
                           subtitle = "Full-time equivalent (100% workload), in CHF",
                           group_order = role_group_levels,
                           unit        = "CHF")

# b) Years of professional experience
make_grouped_numeric_table(clean_data, "role_group", "plyexp",
                           title       = "Years of professional experience, hidden vs. labelled statistical roles",
                           filename    = "hidden_experience_by_rolegroup.png",
                           group_order = role_group_levels,
                           unit        = "years")

# c) Seniority level
make_crosstab_table(clean_data, "role_group", "plsenior",
                    title     = "Seniority level, hidden vs. labelled statistical roles",
                    filename  = "hidden_seniority_by_rolegroup.png",
                    row_label = "Role group",
                    col_order = seniority_level_levels,
                    sort_rows_by_total = FALSE)

message("✔ 5.3.5 (4) — Salary and seniority")
message("✔ 07_tables_derived_variables.R terminé — tables dans ", tab_dir,
        " (sous-dossiers career_pathways/, labour_market/, salary_and_conditions/,",
        " future_research/, hidden_statistical_roles/)")