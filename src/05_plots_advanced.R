# ══════════════════════════════════════════════════════════════════
#  05_plots_advanced.R — Graphiques croisés (miroir de 07_tables_derived_variables.R)
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
#  Réutilise `is_manager` et `job_profile`, créés dans 07 — donc à
#  sourcer APRÈS 07_tables_derived_variables.R.
#
#  Même population que 07 (pas de split SSS members — cf. commentaire
#  d'en-tête du 07), mêmes noms de fichiers pour retrouver le graphe
#  jumeau de chaque table.
# ══════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════
#  §5.3.1 — CAREER PATHWAYS  →  out_dir/career_pathways/
# ══════════════════════════════════════════════════════════════════
plots_dir <- file.path(out_dir, "career_pathways")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
message("── Plots : career_pathways ──")

save_heatmap_crosstab(clean_data, "job_role", "trlvl",
                      "Work position by highest degree obtained",
                      "career_jobrole_by_degree.png",
                      subtitle = "Counts", wrap_width = 30)

save_heatmap_crosstab(clean_data, "plsenior", "trlvl",
                      "Seniority level by highest degree obtained",
                      "career_seniority_by_degree.png",
                      subtitle = "Counts")

save_rate_bar(clean_data, "trlvl", "is_manager",
              "Managerial responsibility by highest degree obtained",
              "career_manager_rate_by_degree.png",
              subtitle = "Share of respondents in a managerial position (lower/middle/top management)")

save_facet_topn(clean_data, "job_profile", "job_role", n_top = 5,
                "Most common job titles within each activity-based profile",
                "career_jobtitles_by_profile.png",
                subtitle = "Top 5 job titles per profile — profiles are rules-based (protocol 5.2, provisional)")

# Mean perceived importance of statistical activities, by job profile -> heatmap
df_us_profile <- clean_data |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code), !is.na(job_profile)) |>
  mutate(theme = stringr::str_wrap(theme, width = 28))

if (nrow(df_us_profile) > 0) {
  tab_imp <- df_us_profile |>
    group_by(job_profile, theme) |>
    summarise(mean_imp = mean(importance_code, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(tab_imp, aes(x = theme, y = job_profile, fill = mean_imp)) +
    geom_tile(color = sss_bg, linewidth = 1.5) +
    geom_text(aes(label = round(mean_imp, 1),
                  color = mean_imp > (max(tab_imp$mean_imp) * 0.55)),
              size = 3.3, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = sss_blue_dark)) +
    scale_fill_gradient(low = "#eaf2fa", high = sss_blue_dark, guide = "none") +
    labs(title = sss_wrap_title("Mean perceived importance of statistical activities, by job profile"),
         subtitle = sss_wrap_subtitle("Scale: 0 (Not at all important) to 4 (Very important)"),
         caption = sss_caption_note(nrow(clean_data), "All respondents")) +
    theme_sss(horizontal = TRUE) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))
  
  save_plot(p, "career_importance_by_profile.png", height = 4.5)
}

# Field of study by gender (multi-select field) -> heatmap
df_fields_gender <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = "field") |>
  filter(!is.na(field), field != "", !is.na(dmgender))

save_heatmap_crosstab(df_fields_gender, "field", "dmgender",
                      "Distribution of field of study by gender",
                      "career_field_by_gender.png",
                      subtitle = "Multiple fields of study allowed per respondent — counts",
                      wrap_width = 28)

# Field of study by location of graduation -> heatmap
df_fields_loc <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = "field") |>
  filter(!is.na(field), field != "", !is.na(study_location))

save_heatmap_crosstab(df_fields_loc, "field", "study_location",
                      "Distribution of field of study by location of graduation",
                      "career_field_by_location.png",
                      subtitle = "Multiple fields of study allowed per respondent — counts",
                      wrap_width = 28)

message("  ✔ career_pathways")

# ══════════════════════════════════════════════════════════════════
#  §5.3.2 — LABOUR MARKET  →  out_dir/labour_market/
# ══════════════════════════════════════════════════════════════════
plots_dir <- file.path(out_dir, "labour_market")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
message("── Plots : labour_market ──")

sector_counts <- clean_data |> filter(!is.na(plsector)) |> count(plsector)
sector_eligible <- sector_counts$plsector[sector_counts$n >= 5]
df_sector_pool <- clean_data |> filter(plsector %in% sector_eligible)

df_us_overall <- clean_data |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code)) |>
  mutate(theme = stringr::str_wrap(theme, width = 42))

if (nrow(df_us_overall) > 0) {
  tab_us_overall <- df_us_overall |>
    group_by(theme) |>
    summarise(pct_important = mean(importance_code >= 3, na.rm = TRUE), .groups = "drop") |>
    mutate(theme = forcats::fct_reorder(theme, pct_important))
  
  p <- ggplot(tab_us_overall, aes(x = theme, y = pct_important)) +
    geom_col(fill = sss_blue, width = 0.6) +
    geom_text(aes(label = scales::percent(pct_important, accuracy = 1)),
              hjust = -0.15, size = 3.4, color = sss_grey_text) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    labs(title = sss_wrap_title("Statistical activities — overall importance and use"),
         subtitle = sss_wrap_subtitle("Sorted by perceived importance"),
         caption = sss_caption_note(nrow(clean_data), "All respondents")) +
    theme_sss(horizontal = TRUE)
  save_plot(p, "labour_activities_overall.png", height = 4.5)
}

save_bar_multi(clean_data, "skills",
               "Work-related skills — overall", "labour_skills_overall.png",
               wrap_width = 45)

df_us_sector <- clean_data |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code), plsector %in% sector_eligible) |>
  mutate(theme = stringr::str_wrap(theme, width = 28))

save_heatmap_crosstab(df_us_sector, "plsector", "theme",
                      "Mean perceived importance of statistical activities, by sector",
                      "labour_activities_by_sector.png",
                      subtitle = "Scale: 0-4. Sectors with fewer than 5 respondents excluded.",
                      wrap_width = 30)

df_skills_sector <- df_sector_pool |>
  tidyr::unnest_longer(skills, values_to = "skill") |>
  filter(!is.na(skill), skill != "")

save_heatmap_crosstab(df_skills_sector, "plsector", "skill",
                      "Work-related skills — share of respondents, by sector",
                      "labour_skills_by_sector.png",
                      subtitle = "Multiple skills allowed per respondent. Sectors with fewer than 5 respondents excluded.",
                      wrap_width = 30, col_wrap_width = 18)

save_boxplot(df_sector_pool, "plyexp",
             "Years of professional experience, by sector",
             "labour_experience_by_sector.png",
             groupvar = "plsector",
             subtitle = "Sorted by median years of experience. Sectors with fewer than 5 respondents excluded.")

save_heatmap_crosstab(df_sector_pool, "plsector", "plsenior",
                      "Seniority level, by sector",
                      "labour_seniority_by_sector.png",
                      subtitle = "Sectors with fewer than 5 respondents excluded.",
                      wrap_width = 30)

save_rate_bar(df_sector_pool, "plsector", "is_manager",
              "Managerial responsibility, by sector",
              "labour_manager_rate_by_sector.png",
              subtitle = "Share of respondents in a managerial position. Sectors with fewer than 5 respondents excluded.")

save_bar_freq(clean_data, "continuous_education",
              "Continuous education", "labour_continuing_ed_overall.png",
              order = "freq", horizontal = TRUE, wrap_width = 40,
              subtitle = "Respondents who selected more than one option are excluded")

save_stacked_100_bar(df_sector_pool, "plsector", "continuous_education",
                     "Continuous education, by sector",
                     "labour_continuing_ed_by_sector.png",
                     subtitle = "Sectors with fewer than 5 respondents excluded")

save_stacked_100_bar(clean_data, "plsenior", "continuous_education",
                     "Continuous education, by seniority level",
                     "labour_continuing_ed_by_seniority.png",
                     group_order = seniority_level_levels)

save_stacked_100_bar(clean_data, "exp_group", "continuous_education",
                     "Continuous education, by years of experience",
                     "labour_continuing_ed_by_experience.png",
                     group_order = exp_group_levels)

message("  ✔ labour_market")

# ══════════════════════════════════════════════════════════════════
#  §5.3.3 — SALARY AND CONDITIONS  →  out_dir/salary_and_conditions/
# ══════════════════════════════════════════════════════════════════
plots_dir <- file.path(out_dir, "salary_and_conditions")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
message("── Plots : salary_and_conditions ──")

dmwork_counts <- clean_data |> filter(!is.na(dmwork), dmwork != "I do not work") |> count(dmwork)
dmwork_eligible <- dmwork_counts$dmwork[dmwork_counts$n >= 5]
df_work_pool <- clean_data |> filter(dmwork %in% dmwork_eligible)

save_boxplot(clean_data, "salary",
             "Salary levels, by sector of employment", "salary_by_sector.png",
             groupvar = "plsector",
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(clean_data, "salary",
             "Salary levels, by work position (job title)", "salary_by_jobrole.png",
             groupvar = "job_role",
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(clean_data, "salary",
             "Salary levels, by highest degree obtained", "salary_by_degree.png",
             groupvar = "trlvl", order_by_median = FALSE,
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(clean_data, "salary",
             "Salary levels, by years of professional experience", "salary_by_experience.png",
             groupvar = "exp_group", order_by_median = FALSE,
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(clean_data, "salary",
             "Salary levels, by seniority level", "salary_by_seniority.png",
             groupvar = "plsenior", order_by_median = FALSE,
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(clean_data, "salary",
             "Salary levels, by age group", "salary_by_age_group.png",
             groupvar = "age_group", order_by_median = FALSE,
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_boxplot(df_work_pool, "salary",
             "Salary levels, by work region (canton)", "salary_by_work_region.png",
             groupvar = "dmwork",
             subtitle = "Full-time equivalent (100% workload), in CHF. Regions with fewer than 5 respondents excluded.")

save_boxplot(clean_data, "salary",
             "Salary levels, by gender", "salary_by_gender.png",
             groupvar = "dmgender",
             subtitle = "Full-time equivalent (100% workload), in CHF")

save_diverging_satisfaction(clean_data, "plsector",
                            "Job satisfaction, by sector of employment",
                            "satisfaction_by_sector.png")

save_diverging_satisfaction(clean_data, "trlvl",
                            "Job satisfaction, by highest degree obtained",
                            "satisfaction_by_degree.png",
                            group_order = education_level)

save_diverging_satisfaction(clean_data, "plsenior",
                            "Job satisfaction, by seniority level",
                            "satisfaction_by_seniority.png",
                            group_order = seniority_level_levels)

clean_data$salary_quartile <- cut(clean_data$salary,
                                  breaks = quantile(clean_data$salary, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"),
                                  include.lowest = TRUE)

save_diverging_satisfaction(clean_data, "salary_quartile",
                            "Job satisfaction, by salary level (quartiles)",
                            "satisfaction_by_salary.png",
                            subtitle = "Quartiles computed on full-time equivalent salary",
                            group_order = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))

clean_data$workrate_group <- cut(clean_data$plrate,
                                 breaks = c(0, 50, 80, 100),
                                 labels = c("≤ 50%", "51-80%", "81-100%"),
                                 include.lowest = TRUE)

save_diverging_satisfaction(clean_data, "workrate_group",
                            "Job satisfaction, by work rate",
                            "satisfaction_by_workrate.png",
                            group_order = c("≤ 50%", "51-80%", "81-100%"))

save_diverging_satisfaction(clean_data, "exp_group",
                            "Job satisfaction, by years of professional experience",
                            "satisfaction_by_experience.png",
                            group_order = exp_group_levels)

save_hist(clean_data, "plrate",
          "Work rate — descriptive statistics", "desc_workrate.png")

save_bar_freq(clean_data, "workrate_group",
              "Work rate group", "freq_workrate_group.png",
              order = "level")

save_bar_freq(clean_data, "job_status",
              "Employment status", "freq_employment_status_conditions.png",
              subtitle = "No fixed-term vs. permanent contract variable is available in the survey")

message("  ✔ salary_and_conditions")

# ══════════════════════════════════════════════════════════════════
#  §5.3.4 — FUTURE RESEARCH  →  out_dir/future_research/
# ══════════════════════════════════════════════════════════════════
plots_dir <- file.path(out_dir, "future_research")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
message("── Plots : future_research ──")

save_diverging_satisfaction(clean_data, "job_role",
                            "Job satisfaction, by job role",
                            "assoc_satisfaction_by_jobrole.png")

df_skill_sat <- clean_data |>
  tidyr::unnest_longer(skills, values_to = "skill") |>
  filter(!is.na(skill), skill != "", !is.na(satisf_score))

if (nrow(df_skill_sat) > 0) {
  tab_sk_sat <- df_skill_sat |>
    mutate(skill = stringr::str_wrap(skill, width = 40)) |>
    group_by(skill) |>
    summarise(mean_score = mean(satisf_score, na.rm = TRUE), .groups = "drop") |>
    mutate(skill = forcats::fct_reorder(skill, mean_score))
  
  p <- ggplot(tab_sk_sat, aes(x = skill, y = mean_score)) +
    geom_col(fill = sss_blue, width = 0.6) +
    geom_text(aes(label = round(mean_score, 2)), hjust = -0.15, size = 3.4, color = sss_grey_text) +
    coord_flip(clip = "off") +
    labs(title = sss_wrap_title("Job satisfaction, by work-related skill held"),
         subtitle = sss_wrap_subtitle("Multiple skills allowed per respondent — mean satisfaction score (1-5, 5 = Very satisfied)"),
         caption = sss_caption_note(nrow(clean_data), "All respondents")) +
    theme_sss(horizontal = TRUE)
  save_plot(p, "assoc_satisfaction_by_skill.png", height = max(4, 0.5 * nrow(tab_sk_sat) + 1.8))
}

save_diverging_satisfaction(clean_data, "job_profile",
                            "Job satisfaction, by activity-based job profile",
                            "assoc_satisfaction_by_jobprofile.png",
                            subtitle = "Bonus plot (not explicitly requested by protocol)",
                            group_order = profile_order)

save_stacked_100_bar(clean_data, "trlvl", "continuous_education",
                     "Continuous education, by highest degree obtained",
                     "future_contedu_by_degree.png",
                     group_order = education_level)

df_field_ce <- clean_data |>
  tidyr::unnest_longer(training_fields_list, values_to = "field") |>
  filter(!is.na(field), field != "", !is.na(continuous_education))

save_heatmap_crosstab(df_field_ce, "field", "continuous_education",
                      "Continuous education, by field of study",
                      "future_contedu_by_field.png",
                      subtitle = "Multiple fields of study allowed per respondent",
                      wrap_width = 26)

save_stacked_100_bar(df_sector_pool, "plsector", "continuous_education",
                     "Continuous education, by sector of employment",
                     "future_contedu_by_sector.png",
                     subtitle = "Sectors with fewer than 5 respondents excluded")

save_stacked_100_bar(clean_data, "job_profile", "continuous_education",
                     "Continuous education, by activity-based job profile",
                     "future_contedu_by_jobprofile.png",
                     subtitle = "Profiles as defined in section 5.3.2.2",
                     group_order = profile_order)

message("  ✔ future_research")

# ══════════════════════════════════════════════════════════════════
#  §5.3.5 — HIDDEN STATISTICAL ROLES  →  out_dir/hidden_statistical_roles/
# ══════════════════════════════════════════════════════════════════
plots_dir <- file.path(out_dir, "hidden_statistical_roles")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
message("── Plots : hidden_statistical_roles ──")

# Heuristique simple pour rolegroup — À ALIGNER avec la règle exacte
# utilisée dans 07_tables_derived_variables.R si elle diffère.
clean_data$rolegroup <- ifelse(
  grepl("stat|data", clean_data$job_role, ignore.case = TRUE),
  "Explicit statistical/data role", "Hidden statistical role"
)

df_skills_role <- clean_data |>
  tidyr::unnest_longer(skills, values_to = "skill") |>
  filter(!is.na(skill), skill != "")

save_heatmap_crosstab(df_skills_role, "skill", "rolegroup",
                      "Work-related skills, by role group",
                      "hidden_skills_by_rolegroup.png",
                      subtitle = "Explicit statistical/data job titles vs. hidden statistical roles",
                      wrap_width = 28)

df_us_role <- clean_data |>
  tidyr::unnest(ustime) |>
  filter(!is.na(importance_code)) |>
  mutate(theme = stringr::str_wrap(theme, width = 28))

save_heatmap_crosstab(df_us_role, "theme", "rolegroup",
                      "Statistical activities, by role group",
                      "hidden_activities_by_rolegroup.png",
                      subtitle = "Mean perceived importance — explicit vs. hidden statistical roles")

message("  ✔ hidden_statistical_roles")
message("✔ 05_plots_advanced.R terminé — plots dans ", out_dir, "\n")