# ══════════════════════════════════════════════════════════════════
#  06_tables.R — Tables descriptives pour le rapport
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
#
#  Toutes les tables sont générées pour 2 populations :
#    - full_population/  : tous les répondants
#    - sss_members/       : uniquement is_sss_member == TRUE
# ══════════════════════════════════════════════════════════════════
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gt)

if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

# ── Style commun ─────────────────────────────────────────────────
sss_blue      <- "#2C7FB8"
sss_dark      <- "#1a3a5c"
sss_light     <- "#eaf2fa"
sss_lightgrey <- "#f7f8fa"

# Globals mis à jour par generate_all_tables() pour chaque population
tables_dir     <- tab_dir
current_pop_n  <- NA_integer_
current_pop_lb <- ""

style_table <- function(gt_obj, title, subtitle = NULL) {
  gt_obj |>
    tab_header(
      title    = md(paste0("**", title, "**")),
      subtitle = if (!is.null(subtitle)) md(paste0("*", subtitle, "*"))
    ) |>
    tab_options(
      table.font.size            = px(13),
      heading.title.font.size    = px(16),
      heading.subtitle.font.size = px(12),
      heading.background.color   = sss_dark,
      column_labels.background.color = sss_blue,
      column_labels.font.weight  = "bold",
      column_labels.font.size    = px(13),
      row.striping.background_color = sss_lightgrey,
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
        "SSS Survey — ", current_pop_lb, " — *n* = ", current_pop_n,
        " | Generated ", Sys.Date()
      ))
    ) |>
    opt_table_font(font = list(google_font("Inter"), "Helvetica", default_fonts()))
}

save_gt <- function(gt_obj, filename) {
  path <- file.path(tables_dir, filename)
  gtsave(gt_obj, path)
  
  # Miroir CSV : même sous-dossier que tables_dir, mais sous csv_dir/
  rel_subdir <- sub(paste0("^", tab_dir, "/?"), "", tables_dir)
  csv_subdir <- if (rel_subdir == "") csv_dir else file.path(csv_dir, rel_subdir)
  if (!dir.exists(csv_subdir)) dir.create(csv_subdir, recursive = TRUE)
  
  csv_path <- file.path(csv_subdir, sub("\\.png$", ".csv", filename))
  readr::write_csv(gt_obj[["_data"]], csv_path)
  message("  → ", csv_path)
  
  table_count <<- table_count + 1
}

# ══════════════════════════════════════════════════════════════════
#  A) FREQUENCY TABLES — variables catégorielles
# ══════════════════════════════════════════════════════════════════

make_freq_table <- function(df, var, title, filename,
                            subtitle = NULL, wrap_width = NULL,
                            order_by = c("freq", "level")) {
  order_by <- match.arg(order_by)
  
  tab <- df |>
    filter(!is.na(.data[[var]]), as.character(.data[[var]]) != "") |>
    count(.data[[var]], name = "N")
  
  if (order_by == "freq") tab <- tab |> arrange(desc(N))
  
  tab <- tab |>
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
      locations = cells_body(rows = which(seq_len(nrow(tab)) %% 2 == 1))
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

# ══════════════════════════════════════════════════════════════════
#  B) NUMERIC SUMMARY TABLE — variable continue (une ligne)
# ══════════════════════════════════════════════════════════════════

make_numeric_table <- function(df, var, title, filename,
                               subtitle = NULL, unit = "") {
  v <- df[[var]]
  v <- v[!is.na(v)]
  
  tab <- data.frame(
    N      = length(v),
    Mean   = mean(v),
    SD     = sd(v),
    Min    = min(v),
    Q1     = quantile(v, 0.25),
    Median = median(v),
    Q3     = quantile(v, 0.75),
    Max    = max(v),
    row.names = NULL,
    check.names = FALSE
  )
  
  gt_obj <- tab |>
    gt() |>
    fmt_integer(columns = N) |>
    fmt_number(columns = c(Mean, SD, Q1, Median, Q3), decimals = 1) |>
    fmt_number(columns = c(Min, Max), decimals = 0) |>
    tab_spanner(label = "Distribution", columns = c(Min, Q1, Median, Q3, Max)) |>
    tab_spanner(label = "Central tendency", columns = c(Mean, SD)) |>
    style_table(title = title, subtitle = subtitle)
  
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
#  C) MULTI-SELECT TABLE — variable liste (une ligne par répondant,
#     plusieurs réponses possibles). N selections + % of respondents
#     (ne somme pas à 100%, contrairement à une freq table classique).
# ══════════════════════════════════════════════════════════════════

make_multi_table <- function(df, listvar, title, filename,
                             subtitle = NULL, wrap_width = NULL,
                             levels_order = NULL) {
  tab <- df |>
    tidyr::unnest_longer(!!sym(listvar), values_to = ".val") |>
    filter(!is.na(.val), .val != "") |>
    count(.val, name = "N")
  
  if (!is.null(levels_order)) {
    tab <- tab |>
      mutate(.ord = match(.val, levels_order)) |>
      arrange(.ord) |>
      select(-.ord)
  } else {
    tab <- tab |> arrange(desc(N))
  }
  
  tab <- tab |> mutate(`% of respondents` = N / nrow(df))
  names(tab)[1] <- "Category"
  
  if (!is.null(wrap_width))
    tab$Category <- stringr::str_wrap(as.character(tab$Category), width = wrap_width)
  
  gt_obj <- tab |>
    gt() |>
    fmt_integer(columns = N) |>
    fmt_percent(columns = `% of respondents`, decimals = 1) |>
    cols_align(align = "left", columns = Category) |>
    cols_align(align = "right", columns = c(N, `% of respondents`)) |>
    data_color(
      columns  = N,
      palette  = c("white", sss_blue),
      alpha    = 0.4
    ) |>
    style_table(
      title    = title,
      subtitle = paste0(
        if (!is.null(subtitle)) paste0(subtitle, " — ") else "",
        "multiple selections allowed, shares do not sum to 100%"
      )
    )
  
  save_gt(gt_obj, filename)
  gt_obj
}

# ══════════════════════════════════════════════════════════════════
#  GÉNÉRATEUR — toutes les tables (blocs 1 à 6) pour une population
# ══════════════════════════════════════════════════════════════════

generate_all_tables <- function(df, subdir, pop_label) {
  
  # Redirige les sauvegardes + le n affiché vers cette population
  tables_dir     <<- file.path(tab_dir, subdir)
  current_pop_n  <<- nrow(df)
  current_pop_lb <<- pop_label
  if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
  
  message("── Génération : ", pop_label, " (n=", nrow(df), ") → ", tables_dir, " ──")
  
  # ════════════════════════════════════════════════════════════════
  #  1. DEMOGRAPHICS — gender, age, nationality, place of residence and work
  #     (protocole, section 5.1 — point 1)
  # ════════════════════════════════════════════════════════════════
  
  make_freq_table(df, "dmgender",
                  "Gender distribution",
                  "freq_gender.png")
  
  make_numeric_table(df, "age",
                     "Age — descriptive statistics",
                     "desc_age.png",
                     unit = "years")
  
  make_freq_table(df, "origin",
                  "Nationality / country of origin",
                  "freq_origin.png")
  
  make_freq_table(df, "dmres",
                  "Place of residence (canton)",
                  "freq_residency.png")
  
  make_freq_table(df, "dmwork",
                  "Place of work (canton)",
                  "freq_work_location.png")
  
  message("  ✔ Bloc 1 — Demographics")
  
  # ════════════════════════════════════════════════════════════════
  #  2. SSS MEMBERSHIP AND ENGAGEMENT — awareness, level of involvement,
  #     duration of membership (protocole, section 5.1 — point 2)
  # ════════════════════════════════════════════════════════════════
  
  make_freq_table(df, "sssknow",
                  "SSS awareness",
                  "freq_sss_awareness.png")
  
  make_freq_table(df, "is_sss_member",
                  "SSS membership (member vs. non-member)",
                  "freq_sss_membership.png",
                  subtitle = "Derived variable — TRUE if involvement level is not 'Not a member'")
  
  make_freq_table(df, "sssmember",
                  "Level of involvement in SSS",
                  "freq_sss_involvement.png",
                  order_by = "level")
  
  make_freq_table(df, "ssstime",
                  "Duration of SSS membership",
                  "freq_sss_duration.png",
                  order_by = "level")
  
  message("  ✔ Bloc 2 — SSS Membership and Engagement")
  
  # ════════════════════════════════════════════════════════════════
  #  3. EDUCATION AND TRAINING — level, study domain, year of graduation,
  #     location of graduation, continuous education
  #     (protocole, section 5.1 — point 3)
  # ════════════════════════════════════════════════════════════════
  
  make_freq_table(df, "trlvl",
                  "Highest education level",
                  "freq_education_level.png",
                  order_by = "level")
  
  make_multi_table(df, "training_fields_list",
                   "Study domain (field of study)",
                   "multi_training_fields.png")
  
  make_numeric_table(df, "tryear",
                     "Year of graduation — descriptive statistics",
                     "desc_graduation_year.png")
  
  make_freq_table(df, "study_location",
                  "Location of graduation",
                  "freq_study_location.png")
  
  make_freq_table(df, "continuous_education",
                  "Continuous education",
                  "freq_continuous_education.png",
                  wrap_width = 40,
                  subtitle = "Respondents who selected more than one option are excluded (ambiguous single-choice derivation)")
  
  message("  ✔ Bloc 3 — Education and Training")
  
  # ════════════════════════════════════════════════════════════════
  #  4. PROFESSIONAL LIFE — employment status, position name, workload,
  #     sector, seniority, and experience
  #     (protocole, section 5.1 — point 4)
  # ════════════════════════════════════════════════════════════════
  
  make_freq_table(df, "job_status",
                  "Employment status",
                  "freq_employment_status.png")
  
  make_freq_table(df, "employed",
                  "Currently employed",
                  "freq_employed.png")
  
  make_freq_table(df, "plsector",
                  "Job sector",
                  "freq_sector.png",
                  wrap_width = 35)
  
  make_freq_table(df, "job_role",
                  "Position name (job title)",
                  "freq_job_role.png")
  
  make_numeric_table(df, "plyexp",
                     "Years of professional experience — descriptive statistics",
                     "desc_experience.png",
                     unit = "years")
  
  make_freq_table(df, "plsenior",
                  "Seniority level",
                  "freq_seniority.png",
                  order_by = "level")
  
  make_freq_table(df, "career_stage",
                  "Career stage",
                  "freq_career_stage.png",
                  order_by = "level",
                  subtitle = "Derived from years of professional experience")
  
  message("  ✔ Bloc 4 — Professional Life")
  
  # ════════════════════════════════════════════════════════════════
  #  5. USE OF STATISTICS IN PROFESSIONAL ACTIVITY — extent and type of
  #     statistical involvement (data cleaning, descriptive/inferential
  #     analysis, modelling, supervision)
  #     (protocole, section 5.1 — point 5)
  #     Variable = `ustime` : liste imbriquée par répondant, 6 thèmes ×
  #     (niveau d'importance perçue + niveau d'implication/pratique).
  # ════════════════════════════════════════════════════════════════
  
  df_us <- df |>
    tidyr::unnest(ustime) |>
    filter(!is.na(importance_code), !is.na(involvement_code)) |>
    mutate(
      importance  = factor(importance,  levels = importance_levels),
      involvement = factor(involvement, levels = involvement_levels)
    )
  
  if (nrow(df_us) > 0) {
    df_us_summary <- df_us |>
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
    
    names(df_us_summary)[1] <- "Statistical activity"
    df_us_summary$`Statistical activity` <- stringr::str_wrap(
      df_us_summary$`Statistical activity`, width = 42
    )
    
    gt_us <- df_us_summary |>
      gt() |>
      fmt_integer(columns = N) |>
      fmt_number(columns = `Mean importance`, decimals = 2) |>
      fmt_percent(columns = c(`% Important+`, `% Active use`), decimals = 1) |>
      cols_align(align = "left", columns = `Statistical activity`) |>
      tab_style(
        style     = list(cell_text(weight = "bold")),
        locations = cells_body(columns = `Statistical activity`)
      ) |>
      data_color(columns = `% Important+`, palette = c("white", "#1a7a4a"), alpha = 0.4) |>
      data_color(columns = `% Active use`, palette = c("white", sss_blue),  alpha = 0.4) |>
      style_table(
        title    = "Statistical activities — extent and type of involvement",
        subtitle = "Data cleaning, descriptive/inferential analysis, modelling, supervision — sorted by perceived importance"
      )
    
    save_gt(gt_us, "ustime_summary.png")
  }
  
  message("  ✔ Bloc 5 — Use of Statistics in Professional Activity")
  
  # ════════════════════════════════════════════════════════════════
  #  6. INCOME AND JOB SATISFACTION — gross annual income, satisfaction
  #     with various work aspects
  #     (protocole, section 5.1 — point 6)
  # ════════════════════════════════════════════════════════════════
  
  make_numeric_table(df, "salary_raw",
                     "Gross annual income — descriptive statistics",
                     "desc_salary_raw.png",
                     unit = "CHF")
  
  make_freq_table(df, "worksatisfction",
                  "Overall work satisfaction",
                  "freq_work_satisfaction.png",
                  order_by = "level")
  
  df_sat <- df |>
    tidyr::unnest(issatisf2) |>
    filter(!is.na(code), !is.na(item)) |>
    mutate(label = factor(label, levels = satisf_levels))
  
  if (nrow(df_sat) > 0) {
    df_sat_summary <- df_sat |>
      group_by(item) |>
      summarise(
        N              = n(),
        `Mean`         = mean(code, na.rm = TRUE),
        `% Satisfied`  = mean(label %in% c("Very satisfied", "Somewhat satisfied"),
                              na.rm = TRUE),
        `% Dissatisfied` = mean(label %in% c("Not so satisfied",
                                             "Not at all satisfied"),
                                na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`% Satisfied`))
    
    names(df_sat_summary)[1] <- "Job aspect"
    df_sat_summary$`Job aspect` <- stringr::str_wrap(df_sat_summary$`Job aspect`, width = 42)
    
    gt_sat <- df_sat_summary |>
      gt() |>
      fmt_integer(columns = N) |>
      fmt_number(columns = Mean, decimals = 2) |>
      fmt_percent(columns = c(`% Satisfied`, `% Dissatisfied`), decimals = 1) |>
      cols_align(align = "left", columns = `Job aspect`) |>
      tab_style(
        style     = list(cell_text(weight = "bold")),
        locations = cells_body(columns = `Job aspect`)
      ) |>
      data_color(columns = `% Satisfied`,    palette = c("#fce4e4", "#1a7a4a"), alpha = 0.4) |>
      data_color(columns = `% Dissatisfied`, palette = c("#e8f5e9", "#c0392b"), alpha = 0.4) |>
      style_table(
        title    = "Job satisfaction — detailed aspects",
        subtitle = "Remuneration, advancement, work-life balance, recognition, etc. — ranked by % satisfied"
      )
    
    save_gt(gt_sat, "satisfaction_detail.png")
  }
  
  message("  ✔ Bloc 6 — Income and Job Satisfaction")
  message("✔ Terminé : ", pop_label, "\n")
}

# ══════════════════════════════════════════════════════════════════
#  EXÉCUTION — 2 populations
# ══════════════════════════════════════════════════════════════════

# 1) Population complète
generate_all_tables(clean_data,
                    subdir    = "full_population",
                    pop_label = "All respondents")

# 2) Membres SSS uniquement (is_sss_member == TRUE)
sss_data <- clean_data |> filter(is_sss_member)

generate_all_tables(sss_data,
                    subdir    = "sss_members",
                    pop_label = "SSS members only")

message("✔ 06_tables.R terminé — tables dans ", tab_dir,
        " (sous-dossiers full_population/ et sss_members/)")