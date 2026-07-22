# ══════════════════════════════════════════════════════════════════
#  04_plots_basic.R — Graphiques descriptifs (miroir de 06_tables.R)
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
#
#  Un graphe par table de 06_tables.R, même nom de fichier, même titre.
# ══════════════════════════════════════════════════════════════════

generate_all_plots <- function(df, subdir, pop_label) {
  
  plots_dir <<- file.path(out_dir, subdir)
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  
  message("── Plots : ", pop_label, " (n=", nrow(df), ") → ", plots_dir, " ──")
  
  # ══════════════════════════════════════════════════════════════
  #  1. DEMOGRAPHICS
  # ══════════════════════════════════════════════════════════════
  save_bar_freq(df, "dmgender",
                "Gender distribution", "freq_gender.png",
                pop_label = pop_label)
  
  save_hist(df, "age",
            "Age — descriptive statistics", "desc_age.png",
            binwidth = 5, pop_label = pop_label)
  
  save_bar_freq(df, "origin",
                "Nationality / country of origin", "freq_origin.png",
                order = "freq", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_freq(df, "dmres",
                "Place of residence (canton)", "freq_residency.png",
                order = "freq", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_freq(df, "dmwork",
                "Place of work (canton)", "freq_work_location.png",
                order = "freq", horizontal = TRUE, pop_label = pop_label)
  
  message("  ✔ Bloc 1 — Demographics")
  
  # ══════════════════════════════════════════════════════════════
  #  2. SSS MEMBERSHIP AND ENGAGEMENT
  # ══════════════════════════════════════════════════════════════
  save_bar_freq(df, "sssknow",
                "SSS awareness", "freq_sss_awareness.png",
                pop_label = pop_label)
  
  save_bar_freq(df, "is_sss_member",
                "SSS membership (member vs. non-member)", "freq_sss_membership.png",
                subtitle = "Derived variable — TRUE if involvement level is not 'Not a member'",
                pop_label = pop_label)
  
  save_bar_freq(df, "sssmember",
                "Level of involvement in SSS", "freq_sss_involvement.png",
                order = "level", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_freq(df, "ssstime",
                "Duration of SSS membership", "freq_sss_duration.png",
                order = "level", horizontal = TRUE, pop_label = pop_label)
  
  message("  ✔ Bloc 2 — SSS Membership and Engagement")
  
  # ══════════════════════════════════════════════════════════════
  #  3. EDUCATION AND TRAINING
  # ══════════════════════════════════════════════════════════════
  save_bar_freq(df, "trlvl",
                "Highest education level", "freq_education_level.png",
                order = "level", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_multi(df, "training_fields_list",
                 "Study domain (field of study)", "multi_training_fields.png",
                 pop_label = pop_label)
  
  save_hist(df, "tryear",
            "Year of graduation — descriptive statistics", "desc_graduation_year.png",
            binwidth = 1, pop_label = pop_label)
  
  save_bar_freq(df, "study_location",
                "Location of graduation", "freq_study_location.png",
                order = "freq", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_freq(df, "continuous_education",
                "Continuous education", "freq_continuous_education.png",
                order = "freq", horizontal = TRUE, wrap_width = 40,
                subtitle = "Respondents who selected more than one option are excluded (ambiguous single-choice derivation)",
                pop_label = pop_label)
  
  message("  ✔ Bloc 3 — Education and Training")
  
  # ══════════════════════════════════════════════════════════════
  #  4. PROFESSIONAL LIFE
  # ══════════════════════════════════════════════════════════════
  save_bar_freq(df, "job_status",
                "Employment status", "freq_employment_status.png",
                pop_label = pop_label)
  
  save_bar_freq(df, "employed",
                "Currently employed", "freq_employed.png",
                pop_label = pop_label)
  
  save_bar_freq(df, "plsector",
                "Job sector", "freq_sector.png",
                order = "freq", horizontal = TRUE, wrap_width = 35, pop_label = pop_label)
  
  save_bar_freq(df, "job_role",
                "Position name (job title)", "freq_job_role.png",
                order = "freq", horizontal = TRUE, top_n = 15, pop_label = pop_label)
  
  save_hist(df, "plyexp",
            "Years of professional experience — descriptive statistics", "desc_experience.png",
            binwidth = 2, pop_label = pop_label)
  
  save_bar_freq(df, "plsenior",
                "Seniority level", "freq_seniority.png",
                order = "level", horizontal = TRUE, pop_label = pop_label)
  
  save_bar_freq(df, "career_stage",
                "Career stage", "freq_career_stage.png",
                order = "level", horizontal = FALSE,
                subtitle = "Derived from years of professional experience",
                pop_label = pop_label)
  
  message("  ✔ Bloc 4 — Professional Life")
  
  # ══════════════════════════════════════════════════════════════
  #  5. USE OF STATISTICS IN PROFESSIONAL ACTIVITY
  # ══════════════════════════════════════════════════════════════
  df_us <- df |>
    tidyr::unnest(ustime) |>
    filter(!is.na(importance_code), !is.na(involvement_code)) |>
    mutate(importance = factor(importance, levels = importance_levels))
  
  if (nrow(df_us) > 0) {
    df_us_summary <- df_us |>
      group_by(theme) |>
      summarise(pct_important = mean(importance %in% c("Important", "Very important"), na.rm = TRUE),
                .groups = "drop") |>
      mutate(theme = stringr::str_wrap(theme, width = 42),
             theme = forcats::fct_reorder(theme, pct_important))
    
    p_us <- ggplot(df_us_summary, aes(x = theme, y = pct_important)) +
      geom_col(fill = sss_blue, width = 0.6) +
      geom_text(aes(label = scales::percent(pct_important, accuracy = 1)),
                hjust = -0.15, size = 3.4, color = sss_grey_text) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = sss_wrap_title("Statistical activities — extent and type of involvement"),
           subtitle = sss_wrap_subtitle("Share of respondents rating the activity 'Important' or 'Very important'"),
           caption = sss_caption_note(nrow(df), pop_label)) +
      theme_sss(horizontal = TRUE)
    
    save_plot(p_us, "ustime_summary.png", height = max(4, 0.5 * nrow(df_us_summary) + 1.8))
  }
  
  message("  ✔ Bloc 5 — Use of Statistics in Professional Activity")
  
  # ══════════════════════════════════════════════════════════════
  #  6. INCOME AND JOB SATISFACTION
  # ══════════════════════════════════════════════════════════════
  save_hist(df, "salary_raw",
            "Gross annual income — descriptive statistics", "desc_salary_raw.png",
            bins = 25, pop_label = pop_label)
  
  save_bar_freq(df, "worksatisfction",
                "Overall work satisfaction", "freq_work_satisfaction.png",
                order = "level", horizontal = TRUE, pop_label = pop_label)
  
  df_sat <- df |>
    tidyr::unnest(issatisf2) |>
    filter(!is.na(code), !is.na(item)) |>
    mutate(label = factor(label, levels = satisf_levels))
  
  if (nrow(df_sat) > 0) {
    df_sat_summary <- df_sat |>
      group_by(item) |>
      summarise(pct_satisfied = mean(label %in% c("Very satisfied", "Somewhat satisfied"), na.rm = TRUE),
                .groups = "drop") |>
      mutate(item = stringr::str_wrap(item, width = 42),
             item = forcats::fct_reorder(item, pct_satisfied))
    
    p_sat <- ggplot(df_sat_summary, aes(x = item, y = pct_satisfied)) +
      geom_col(fill = sss_blue, width = 0.6) +
      geom_text(aes(label = scales::percent(pct_satisfied, accuracy = 1)),
                hjust = -0.15, size = 3.4, color = sss_grey_text) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1),
                         expand = expansion(mult = c(0, 0))) +
      labs(title = sss_wrap_title("Job satisfaction — detailed aspects"),
           subtitle = sss_wrap_subtitle("Share of respondents 'Very satisfied' or 'Somewhat satisfied' — ranked by % satisfied"),
           caption = sss_caption_note(nrow(df), pop_label)) +
      theme_sss(horizontal = TRUE)
    
    save_plot(p_sat, "satisfaction_detail.png", height = max(4, 0.5 * nrow(df_sat_summary) + 1.8))
  }
  
  message("  ✔ Bloc 6 — Income and Job Satisfaction")
  message("✔ Terminé : ", pop_label, "\n")
}

# ══════════════════════════════════════════════════════════════════
#  EXÉCUTION — 2 populations (même logique que 06_tables.R)
# ══════════════════════════════════════════════════════════════════

generate_all_plots(clean_data,
                   subdir    = "full_population",
                   pop_label = "All respondents")

generate_all_plots(clean_data |> filter(is_sss_member),
                   subdir    = "sss_members",
                   pop_label = "SSS members")