# ══════════════════════════════════════════════════════════════════
#  06_tables.R — Tables descriptives pour le rapport
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
# ══════════════════════════════════════════════════════════════════
library(gt)

tables_dir <- file.path(out_dir, "tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ── Style commun ─────────────────────────────────────────────────
sss_blue      <- "#2C7FB8"
sss_dark      <- "#1a3a5c"
sss_light     <- "#eaf2fa"
sss_lightgrey <- "#f7f8fa"

style_table <- function(gt_obj, title, subtitle = NULL) {
  gt_obj |>
    tab_header(
      title    = md(paste0("**", title, "**")),
      subtitle = if (!is.null(subtitle)) md(paste0("*", subtitle, "*"))
    ) |>
    tab_options(
      table.font.size        = px(13),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(12),
      heading.background.color   = sss_dark,
      heading.title.font.color   = "white",
      heading.subtitle.font.color = "#c0d8ef",
      column_labels.background.color = sss_blue,
      column_labels.font.color   = "white",
      column_labels.font.weight  = "bold",
      column_labels.font.size    = px(13),
      row.striping.background_color = sss_lightgrey,
      row.striping.include_body  = TRUE,
      table.border.top.color     = sss_dark,
      table.border.top.width     = px(3),
      table.border.bottom.color  = sss_dark,
      table.border.bottom.width  = px(2),
      table_body.hlines.color    = "#e0e4e8",
      table_body.border.bottom.color = sss_dark,
      source_notes.font.size     = px(10),
      source_notes.padding       = px(6),
      data_row.padding           = px(5)
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "SSS Survey — *n* = ", nrow(clean_data),
        " | Generated ", Sys.Date()
      ))
    ) |>
    opt_table_font(font = list(google_font("Inter"), "Helvetica", default_fonts()))
}

save_gt <- function(gt_obj, filename) {
  path <- file.path(tables_dir, filename)
  gtsave(gt_obj, path)
  message("  → ", path)
}

# ══════════════════════════════════════════════════════════════════
#  A) FREQUENCY TABLES — variables catégorielles
# ══════════════════════════════════════════════════════════════════

make_freq_table <- function(df, var, title, filename,
                            subtitle = NULL, wrap_width = NULL) {
  tab <- df |>
    filter(!is.na(.data[[var]]), as.character(.data[[var]]) != "") |>
    count(.data[[var]], name = "N") |>
    arrange(desc(N)) |>
    mutate(
      `%`     = N / sum(N),
      Cum.    = cumsum(N),
      `Cum.%` = cumsum(`%`)
    )
  
  names(tab)[1] <- "Category"
  
  if (!is.null(wrap_width))
    tab$Category <- stringr::str_wrap(as.character(tab$Category), width = wrap_width)
  
  gt_obj <- tab |>
    gt() |>
    fmt_percent(columns = c(`%`, `Cum.%`), decimals = 1) |>
    fmt_integer(columns = c(N, Cum.)) |>
    cols_align(align = "left", columns = Category) |>
    cols_align(align = "right", columns = c(N, `%`, Cum., `Cum.%`)) |>
    tab_style(
      style     = cell_fill(color = sss_light),
      locations = cells_body(rows = seq(1, nrow(tab), by = 2))
    ) |>
    style_table(title = title, subtitle = subtitle) |>
    grand_summary_rows(
      columns  = N,
      fns      = list(Total = ~ sum(.)),
      fmt      = ~ fmt_integer(.)
    )
  
  save_gt(gt_obj, filename)
  gt_obj
}

# Gender
make_freq_table(clean_data, "dmgender",
                "Gender distribution",
                "freq_gender.html")

# Origin
make_freq_table(clean_data, "origin",
                "Country of origin",
                "freq_origin.html")

# Canton of residence
make_freq_table(clean_data, "dmres",
                "Canton of residence",
                "freq_residency.html")

# Work location
make_freq_table(clean_data, "dmwork",
                "Work location (canton)",
                "freq_work_location.html")

# SSS awareness
make_freq_table(clean_data, "sssknow",
                "SSS awareness",
                "freq_sss_awareness.html")

# SSS involvement
make_freq_table(clean_data, "sssmember",
                "SSS involvement level",
                "freq_sss_involvement.html")

# SSS membership duration
make_freq_table(clean_data, "ssstime",
                "SSS membership duration",
                "freq_sss_time.html")

# Education level
make_freq_table(clean_data, "trlvl",
                "Highest education level",
                "freq_education_level.html")

# Study location
make_freq_table(clean_data, "study_location",
                "Study location",
                "freq_study_location.html")

# Continuous education
make_freq_table(clean_data, "continuous_education",
                "Continuous education",
                "freq_continuous_education.html",
                wrap_width = 40)

# Employment status
make_freq_table(clean_data, "job_status",
                "Employment status",
                "freq_employment_status.html")

# Sector
make_freq_table(clean_data, "plsector",
                "Job sector",
                "freq_sector.html",
                wrap_width = 35)

# Seniority level
make_freq_table(clean_data, "plsenior",
                "Seniority level",
                "freq_seniority.html")

# Work satisfaction
make_freq_table(clean_data, "worksatisfction",
                "Overall work satisfaction",
                "freq_work_satisfaction.html")

# ══════════════════════════════════════════════════════════════════
#  B) TRAINING FIELDS — multi-réponse
# ══════════════════════════════════════════════════════════════════
df_tf_tab <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") |>
  filter(!is.na(training_field), training_field != "") |>
  count(training_field, name = "Selections") |>
  arrange(desc(Selections)) |>
  mutate(
    `% of respondents` = Selections / nrow(clean_data)
  )

names(df_tf_tab)[1] <- "Training field"

gt_tf <- df_tf_tab |>
  gt() |>
  fmt_integer(columns = Selections) |>
  fmt_percent(columns = `% of respondents`, decimals = 1) |>
  cols_align(align = "left", columns = `Training field`) |>
  data_color(
    columns  = Selections,
    palette  = c("white", sss_blue),
    alpha    = 0.4
  ) |>
  style_table(
    title    = "Training fields",
    subtitle = "Multiple selections allowed"
  )

save_gt(gt_tf, "freq_training_fields.html")

# ══════════════════════════════════════════════════════════════════
#  C) SKILLS — multi-réponse
# ══════════════════════════════════════════════════════════════════
df_sk_tab <- clean_data |>
  tidyr::unnest_longer(skills, values_to = "skill") |>
  filter(!is.na(skill), skill != "") |>
  mutate(skill = stringr::str_wrap(skill, width = 45)) |>
  count(skill, name = "Selections") |>
  arrange(desc(Selections)) |>
  mutate(`% of respondents` = Selections / nrow(clean_data))

names(df_sk_tab)[1] <- "Skill"

gt_sk <- df_sk_tab |>
  gt() |>
  fmt_integer(columns = Selections) |>
  fmt_percent(columns = `% of respondents`, decimals = 1) |>
  cols_align(align = "left", columns = Skill) |>
  data_color(
    columns  = Selections,
    palette  = c("white", sss_blue),
    alpha    = 0.4
  ) |>
  style_table(
    title    = "Work-related skills",
    subtitle = "Multiple selections allowed"
  )

save_gt(gt_sk, "freq_skills.html")

# ══════════════════════════════════════════════════════════════════
#  D) DESCRIPTIVE STATS — variables numériques
# ══════════════════════════════════════════════════════════════════
num_vars <- list(
  list(var = "age",    label = "Age (years)"),
  list(var = "tryear", label = "Training completion year"),
  list(var = "plyexp", label = "Years of professional experience"),
  list(var = "plrate", label = "Employment rate (%)"),
  list(var = "salary", label = "Salary (CHF, normalized 100%)")
)

desc_rows <- lapply(num_vars, function(v) {
  x <- clean_data[[v$var]]
  x <- x[!is.na(x)]
  data.frame(
    Variable = v$label,
    N        = length(x),
    Mean     = mean(x),
    SD       = sd(x),
    Min      = min(x),
    Q1       = quantile(x, 0.25),
    Median   = median(x),
    Q3       = quantile(x, 0.75),
    Max      = max(x),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

df_desc <- do.call(rbind, desc_rows)
rownames(df_desc) <- NULL

gt_desc <- df_desc |>
  gt() |>
  fmt_number(columns = c(Mean, SD, Q1, Median, Q3), decimals = 1) |>
  fmt_number(columns = c(Min, Max), decimals = 0) |>
  fmt_integer(columns = N) |>
  cols_align(align = "left", columns = Variable) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = Variable)
  ) |>
  tab_spanner(label = "Distribution", columns = c(Min, Q1, Median, Q3, Max)) |>
  tab_spanner(label = "Central tendency", columns = c(Mean, SD)) |>
  style_table(
    title    = "Descriptive statistics — Numerical variables",
    subtitle = "Salary normalized to 100% workload"
  )

save_gt(gt_desc, "desc_numerical.html")

# ══════════════════════════════════════════════════════════════════
#  E) CROSS-TABLES
# ══════════════════════════════════════════════════════════════════

# ── E1) Gender × Education level ─────────────────────────────────
df_cross1 <- clean_data |>
  filter(!is.na(dmgender), !is.na(trlvl)) |>
  count(trlvl, dmgender, name = "N") |>
  tidyr::pivot_wider(names_from = dmgender, values_from = N, values_fill = 0) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(match(trlvl, education_level))

names(df_cross1)[1] <- "Education level"

gt_cross1 <- df_cross1 |>
  gt() |>
  fmt_integer(columns = where(is.numeric)) |>
  cols_align(align = "left", columns = `Education level`) |>
  grand_summary_rows(
    columns  = where(is.numeric),
    fns      = list(Total = ~ sum(.)),
    fmt      = ~ fmt_integer(.)
  ) |>
  data_color(
    columns  = Total,
    palette  = c("white", sss_blue),
    alpha    = 0.3
  ) |>
  style_table(
    title    = "Gender by education level",
    subtitle = "Cross-tabulation (counts)"
  )

save_gt(gt_cross1, "cross_gender_education.html")

# ── E2) Education level × Seniority ──────────────────────────────
df_cross2 <- clean_data |>
  filter(!is.na(trlvl), !is.na(plsenior)) |>
  count(trlvl, plsenior, name = "N") |>
  tidyr::pivot_wider(names_from = plsenior, values_from = N, values_fill = 0) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(match(trlvl, education_level))

names(df_cross2)[1] <- "Education level"

gt_cross2 <- df_cross2 |>
  gt() |>
  fmt_integer(columns = where(is.numeric)) |>
  cols_align(align = "left", columns = `Education level`) |>
  grand_summary_rows(
    columns  = where(is.numeric),
    fns      = list(Total = ~ sum(.)),
    fmt      = ~ fmt_integer(.)
  ) |>
  data_color(
    columns  = where(is.numeric),
    palette  = c("white", sss_blue),
    alpha    = 0.3
  ) |>
  style_table(
    title    = "Seniority level by education level",
    subtitle = "Cross-tabulation (counts)"
  )

save_gt(gt_cross2, "cross_education_seniority.html")

# ── E3) SSS involvement × membership duration ────────────────────
df_cross3 <- clean_data |>
  filter(!is.na(sssmember), !is.na(ssstime)) |>
  count(ssstime, sssmember, name = "N") |>
  tidyr::pivot_wider(names_from = sssmember, values_from = N, values_fill = 0) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(match(ssstime, time_sss_level))

names(df_cross3)[1] <- "Membership duration"

gt_cross3 <- df_cross3 |>
  gt() |>
  fmt_integer(columns = where(is.numeric)) |>
  cols_align(align = "left", columns = `Membership duration`) |>
  grand_summary_rows(
    columns  = where(is.numeric),
    fns      = list(Total = ~ sum(.)),
    fmt      = ~ fmt_integer(.)
  ) |>
  data_color(
    columns  = where(is.numeric),
    palette  = c("white", sss_blue),
    alpha    = 0.3
  ) |>
  style_table(
    title    = "SSS involvement by membership duration",
    subtitle = "Cross-tabulation (counts)"
  )

save_gt(gt_cross3, "cross_sss_involvement_time.html")

# ── E4) Gender × Sector (top 15) ─────────────────────────────────
top_sectors <- clean_data |>
  filter(!is.na(plsector), as.character(plsector) != "None") |>
  count(plsector, sort = TRUE) |>
  head(15) |>
  pull(plsector)

df_cross4 <- clean_data |>
  filter(!is.na(dmgender), plsector %in% top_sectors) |>
  count(plsector, dmgender, name = "N") |>
  tidyr::pivot_wider(names_from = dmgender, values_from = N, values_fill = 0) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(desc(Total))

names(df_cross4)[1] <- "Sector"
df_cross4$Sector <- stringr::str_wrap(as.character(df_cross4$Sector), width = 30)

gt_cross4 <- df_cross4 |>
  gt() |>
  fmt_integer(columns = where(is.numeric)) |>
  cols_align(align = "left", columns = Sector) |>
  grand_summary_rows(
    columns  = where(is.numeric),
    fns      = list(Total = ~ sum(.)),
    fmt      = ~ fmt_integer(.)
  ) |>
  data_color(
    columns  = Total,
    palette  = c("white", sss_blue),
    alpha    = 0.3
  ) |>
  style_table(
    title    = "Gender distribution by sector",
    subtitle = "Top 15 sectors (counts)"
  )

save_gt(gt_cross4, "cross_gender_sector.html")

# ══════════════════════════════════════════════════════════════════
#  F) SATISFACTION DETAILED — Likert summary table
# ══════════════════════════════════════════════════════════════════
df_sat <- clean_data |>
  tidyr::unnest(issatisf2) |>
  filter(!is.na(code), !is.na(item)) |>
  mutate(label = factor(label, levels = satisf_levels))

df_sat_summary <- df_sat |>
  group_by(item) |>
  summarise(
    N         = n(),
    `Mean`    = mean(code, na.rm = TRUE),
    `% Satisfied` = mean(label %in% c("Very satisfied", "Somewhat satisfied"),
                         na.rm = TRUE),
    `% Dissatisfied` = mean(label %in% c("Not so satisfied",
                                         "Not at all satisfied"),
                            na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(`% Satisfied`))

names(df_sat_summary)[1] <- "Item"
df_sat_summary$Item <- stringr::str_wrap(df_sat_summary$Item, width = 42)

gt_sat <- df_sat_summary |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = Mean, decimals = 2) |>
  fmt_percent(columns = c(`% Satisfied`, `% Dissatisfied`), decimals = 1) |>
  cols_align(align = "left", columns = Item) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = Item)
  ) |>
  data_color(
    columns  = `% Satisfied`,
    palette  = c("#fce4e4", "#1a7a4a"),
    alpha    = 0.4
  ) |>
  data_color(
    columns  = `% Dissatisfied`,
    palette  = c("#e8f5e9", "#c0392b"),
    alpha    = 0.4
  ) |>
  style_table(
    title    = "Job satisfaction — Detailed items",
    subtitle = "Ranked by proportion of satisfied respondents"
  )

save_gt(gt_sat, "satisfaction_detail.html")

# ══════════════════════════════════════════════════════════════════
#  G) STATISTICAL ACTIVITIES — Importance summary
# ══════════════════════════════════════════════════════════════════
df_us_tab <- clean_data |>
  mutate(id = row_number()) |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code), !is.na(involvement_code)) |>
  mutate(
    importance  = factor(importance, levels = importance_levels),
    involvement = factor(involvement, levels = involvement_levels)
  )

df_act_summary <- df_us_tab |>
  group_by(theme) |>
  summarise(
    N                = n(),
    `Mean importance` = mean(importance_code, na.rm = TRUE),
    `% Important+`    = mean(importance %in% c("Important", "Very important"),
                             na.rm = TRUE),
    `% Active use`    = mean(involvement != "No use", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(`% Important+`))

names(df_act_summary)[1] <- "Statistical activity"

gt_act <- df_act_summary |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_number(columns = `Mean importance`, decimals = 2) |>
  fmt_percent(columns = c(`% Important+`, `% Active use`), decimals = 1) |>
  cols_align(align = "left", columns = `Statistical activity`) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = `Statistical activity`)
  ) |>
  data_color(
    columns  = `% Important+`,
    palette  = c("white", "#1a7a4a"),
    alpha    = 0.4
  ) |>
  data_color(
    columns  = `% Active use`,
    palette  = c("white", sss_blue),
    alpha    = 0.4
  ) |>
  style_table(
    title    = "Statistical activities — Importance & involvement",
    subtitle = "Sorted by perceived importance"
  )

save_gt(gt_act, "activities_summary.html")

# ══════════════════════════════════════════════════════════════════
#  H) SALARY BY SECTOR — summary stats
# ══════════════════════════════════════════════════════════════════
df_sal_sector <- clean_data |>
  filter(!is.na(salary), !is.na(plsector),
         as.character(plsector) != "None") |>
  group_by(plsector) |>
  filter(n() >= 5) |>
  summarise(
    N      = n(),
    Mean   = mean(salary, na.rm = TRUE),
    Median = median(salary, na.rm = TRUE),
    SD     = sd(salary, na.rm = TRUE),
    Min    = min(salary, na.rm = TRUE),
    Max    = max(salary, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(Median))

names(df_sal_sector)[1] <- "Sector"
df_sal_sector$Sector <- stringr::str_wrap(as.character(df_sal_sector$Sector),
                                          width = 30)

gt_sal <- df_sal_sector |>
  gt() |>
  fmt_integer(columns = N) |>
  fmt_currency(
    columns  = c(Mean, Median, SD, Min, Max),
    currency = "CHF", decimals = 0
  ) |>
  cols_align(align = "left", columns = Sector) |>
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_body(columns = Sector)
  ) |>
  tab_spanner(label = "Distribution (CHF)", columns = c(Min, Median, Max)) |>
  tab_spanner(label = "Central tendency (CHF)", columns = c(Mean, SD)) |>
  data_color(
    columns  = Median,
    palette  = c("#fce4e4", "#1a7a4a"),
    alpha    = 0.35
  ) |>
  style_table(
    title    = "Salary by sector",
    subtitle = "Normalized to 100% workload — sectors with n ≥ 5"
  )

save_gt(gt_sal, "salary_by_sector.html")

message("✔ 06_tables.R terminé — tables dans ", tables_dir)
