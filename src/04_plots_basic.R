# ══════════════════════════════════════════════════════════════════
#  04_plots_basic.R — Graphiques descriptifs (section 5.4.0)
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
# ══════════════════════════════════════════════════════════════════

# ── 1) Âge (tranches de 10 ans) ───────────────────────────────────
if (any(!is.na(clean_data$age))) {
  min_a <- floor(min(clean_data$age, na.rm = TRUE) / 10) * 10
  max_a <- ceiling(max(clean_data$age, na.rm = TRUE) / 10) * 10 + 10
  save_binned_counts(clean_data, "age",
                     breaks = seq(min_a, max_a, by = 10),
                     title  = "Age (10-year bins)",
                     filename = "basic_age_10y.png",
                     xlab = "Age bin", label_mode = "default")
}

# ── 2) Année de fin de formation ──────────────────────────────────
save_hist_counts(clean_data, "tryear",
                 "Training completion year",
                 "basic_trainingyear_hist.png",
                 binwidth = 1, xlab = "Year")

# ── 3) Connaissance de la SSS ─────────────────────────────────────
save_barplot_counts(clean_data, "sssknow", "SSS awareness",
                    "basic_sss_awareness.png",
                    xlab = NULL, rotate_x = FALSE)

save_donut_counts(clean_data, "sssknow",
                  title    = "SSS awareness",
                  filename = "donut_sss_awareness.png",
                  palette  = c("FALSE" = "#D55E00", "TRUE" = "#009E73"),
                  drop_na  = TRUE)

# ── 4) Origine ────────────────────────────────────────────────────
save_barplot_counts(clean_data, "origin", "Origin", "basic_origin.png")

# ── 5) Résidence (canton) ─────────────────────────────────────────
save_barplot_counts(clean_data, "dmres", "Residency (canton)",
                    "basic_residency.png")

# ── 6) Lieu de travail ────────────────────────────────────────────
save_barplot_counts(clean_data, "dmwork", "Work location",
                    "basic_work_location.png")

# ── 7) Implication SSS ────────────────────────────────────────────
save_barplot_counts(clean_data, "sssmember", "SSS involvement",
                    "basic_sss_involvement.png")

# ── 8) Durée d'adhésion SSS ───────────────────────────────────────
save_barplot_counts(clean_data, "ssstime", "SSS membership duration",
                    "basic_sss_time.png")

save_donut_counts(clean_data, "ssstime",
                  title    = "SSS membership duration",
                  filename = "donut_sss_time.png",
                  drop_na  = TRUE)

# Heatmap SSS time × implication
df_sss <- clean_data %>%
  filter(!is.na(ssstime), !is.na(sssmember)) %>%
  count(ssstime, sssmember, name = "n")

p_sss_cross <- ggplot(df_sss, aes(x = sssmember, y = ssstime, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3.5, color = "white") +
  scale_fill_gradient(low = "#1a2a3a", high = my_fill) +
  labs(title = paste("Table-", table_count, " SSS time × involvement"),
       x = NULL, y = NULL, fill = "N") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p_sss_cross)
ggsave(file.path(out_dir, "sss_time_by_involvement.png"),
       p_sss_cross, width = 10, height = 5, dpi = 300)
table_count <<- table_count + 1

# ── 9) Niveau de diplôme ──────────────────────────────────────────
save_barplot_counts(clean_data, "trlvl", "Education level",
                    "basic_education_level.png")

# ── 10) Lieu d'études ─────────────────────────────────────────────
save_barplot_counts(clean_data, "study_location", "Study location",
                    "basic_study_location.png")

# ── 11) Domaines de formation (multi-réponse) ─────────────────────
df_tf <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "") %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

p_tf <- df_tf %>%
  count(training_field, name = "n") %>%
  ggplot(aes(x = training_field, y = n)) +
  geom_col(fill = my_fill, color = my_border) +
  labs(title = paste("Table-", table_count,
                     " Training fields (multiple answers allowed)"),
       x = NULL, y = "N respondents (selections)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_tf)
ggsave(file.path(out_dir, "basic_training_fields.png"),
       p_tf, width = 14, height = 7, dpi = 300)
table_count <<- table_count + 1

# ── 12) Formation continue ────────────────────────────────────────
clean_data$continuous_education <- fct_relabel(
  clean_data$continuous_education,
  ~ stringr::str_wrap(.x, width = 28)
)

save_barplot_counts(clean_data, "continuous_education",
                    "Continuous education",
                    "basic_continuous_education.png")

save_barplot_counts(clean_data, "trcont2",
                    "Continuous education beyond 'No'",
                    "basic_continuous_education_yesno.png",
                    rotate_x = FALSE)

save_donut_counts(clean_data, "continuous_education",
                  title     = "Continuous education",
                  filename  = "donut_continuous_education.png",
                  wrap_width = 26, drop_na = TRUE)

# ── 13) Emploi ────────────────────────────────────────────────────
save_barplot_counts(clean_data, "employed", "Employment status",
                    "basic_employment_status.png", rotate_x = FALSE)

save_donut_counts(clean_data, "employed",
                  title    = "Employment status",
                  filename = "donut_employment_status.png",
                  palette  = c("FALSE" = "#D55E00", "TRUE" = "#009E73"),
                  drop_na  = TRUE)

save_barplot_counts(clean_data, "job_status", "Job status",
                    "basic_job_status.png")

save_donut_counts(clean_data, "job_status",
                  title      = "Job status",
                  filename   = "donut_job_status.png",
                  wrap_width = 18, drop_na = TRUE)

# ── 14) Rôle (Top 20 + Other) ─────────────────────────────────────
save_topn_barplot(clean_data, "job_role", n_top = 20,
                  title    = "Job role (Top 20 + Other)",
                  filename = "basic_job_role_top20.png")

# ── 15) Secteur ───────────────────────────────────────────────────
p_sector <- clean_data %>%
  filter(!is.na(plsector)) %>%
  mutate(plsector_wrap = stringr::str_wrap(as.character(plsector), width = 22)) %>%
  ggplot(aes(x = plsector_wrap)) +
  geom_bar(fill = my_fill, color = my_border) +
  labs(title = paste("Table-", table_count, " Sector"),
       x = NULL, y = "N respondents") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_sector)
ggsave(file.path(out_dir, "basic_sector.png"),
       p_sector, width = 18, height = 9, dpi = 300)
table_count <<- table_count + 1

# ── 16) Expérience professionnelle ────────────────────────────────
save_hist_counts(clean_data, "plyexp",
                 "Years of professional experience in current field",
                 "basic_years_experience_hist.png",
                 binwidth = 1, xlab = "Years")

# ── 17) Taux d'activité ───────────────────────────────────────────
save_hist_counts(clean_data, "plrate",
                 "Employment rate (workload %)",
                 "basic_employment_rate_hist.png",
                 binwidth = 10, xlab = "Workload (%)")

# ── 18) Séniorité ─────────────────────────────────────────────────
save_barplot_counts(clean_data, "plsenior", "Seniority level",
                    "basic_seniority.png")

# ── 19) Salaire (tranches 10k) ────────────────────────────────────
if (any(!is.na(clean_data$salary))) {
  min_s <- floor(min(clean_data$salary, na.rm = TRUE) / 10000) * 10000
  max_s <- ceiling(max(clean_data$salary, na.rm = TRUE) / 10000) * 10000 + 10000
  save_binned_counts(clean_data, "salary",
                     breaks   = seq(min_s, max_s, by = 10000),
                     title    = "Salary (normalized to 100% workload, 10k bins)",
                     filename = "basic_salary_10k.png",
                     xlab     = "Salary bin (CHF)", label_mode = "k")
}

# ── 20) Satisfaction globale ──────────────────────────────────────
save_barplot_counts(clean_data, "worksatisfction", "Work satisfaction",
                    "basic_work_satisfaction.png")

# ── 21) Satisfaction détaillée (diverging bar) ────────────────────
df_satisf <- clean_data %>%
  tidyr::unnest(issatisf2) %>%
  filter(!is.na(code), !is.na(item)) %>%
  mutate(
    label = factor(label, levels = satisf_levels),
    item  = stringr::str_wrap(item, width = 38)
  )

positive_labels <- c("Very satisfied", "Somewhat satisfied")
negative_labels <- c("Not at all satisfied", "Not so satisfied")

likert_palette <- c(
  "Very satisfied"       = "#1a7a4a",
  "Somewhat satisfied"   = "#6dbe8d",
  "Neutral"              = "#b0b8c1",
  "Not so satisfied"     = "#e08060",
  "Not at all satisfied" = "#c0392b"
)

df_div <- df_satisf %>%
  count(item, label, name = "n") %>%
  group_by(item) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    side = case_when(
      label %in% positive_labels ~ "positive",
      label %in% negative_labels ~ "negative",
      TRUE ~ "neutral"
    ),
    pct_directed = case_when(
      side == "negative" ~ -pct,
      side == "neutral"  ~  pct / 2,
      TRUE               ~  pct
    )
  )

item_order <- df_div %>%
  filter(label %in% positive_labels) %>%
  group_by(item) %>%
  summarise(pos_pct = sum(pct)) %>%
  arrange(pos_pct) %>%
  pull(item)

df_div <- df_div %>% mutate(item = factor(item, levels = item_order))

p_satisf2 <- ggplot(df_div, aes(x = pct_directed, y = item, fill = label)) +
  geom_col(position = "stack", width = 0.7) +
  geom_vline(xintercept = 0, color = "white", linewidth = 0.5) +
  scale_fill_manual(values = likert_palette, breaks = satisf_levels,
                    drop = FALSE) +
  scale_x_continuous(
    labels = function(x) paste0(abs(round(x * 100)), "%"),
    limits = c(-1, 1)
  ) +
  labs(title = paste("Table-", table_count,
                     " Job satisfaction — detailed items"),
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

print(p_satisf2)
ggsave(file.path(out_dir, "satisf2_diverging.png"),
       p_satisf2, width = 13, height = 7, dpi = 300)
table_count <<- table_count + 1

# ── 22) Compétences (multi-réponse + co-occurrence) ───────────────
df_skills <- clean_data %>%
  tidyr::unnest_longer(skills, values_to = "skill") %>%
  filter(!is.na(skill), skill != "") %>%
  mutate(skill = factor(skill, levels = skills_levels))

p_skills <- df_skills %>%
  count(skill, name = "n") %>%
  mutate(skill = forcats::fct_reorder(
    stringr::str_wrap(as.character(skill), 28), n)) %>%
  ggplot(aes(x = n, y = skill)) +
  geom_col(fill = my_fill, color = my_border) +
  labs(title = paste("Table-", table_count,
                     " Work-related skills (multiple answers allowed)"),
       x = "N respondents", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

print(p_skills)
ggsave(file.path(out_dir, "basic_skills.png"),
       p_skills, width = 11, height = 6, dpi = 300)
table_count <<- table_count + 1

# Co-occurrence
skill_wide <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest_longer(skills, values_to = "skill") %>%
  filter(!is.na(skill)) %>%
  mutate(
    val = 1,
    skill_short = case_when(
      grepl("Statistical", skill)   ~ "Stat. prog.",
      grepl("Other prog", skill)    ~ "Other prog.",
      grepl("visualization", skill) ~ "DataViz",
      grepl("writing", skill)       ~ "Sci. writing",
      grepl("Project", skill)       ~ "Project mgmt",
      grepl("Time", skill)          ~ "Time mgmt"
    )
  ) %>%
  select(id, skill_short, val) %>%
  tidyr::pivot_wider(names_from = skill_short, values_from = val,
                     values_fill = 0)

skill_mat_co <- as.matrix(skill_wide[, -1])
cooc         <- t(skill_mat_co) %*% skill_mat_co
diag(cooc)   <- NA

cooc_df <- as.data.frame(as.table(cooc)) %>%
  rename(skill1 = Var1, skill2 = Var2, n = Freq) %>%
  filter(!is.na(n))

p_cooc <- ggplot(cooc_df, aes(x = skill1, y = skill2, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3.5, color = "white") +
  scale_fill_gradient(low = "#1a2a3a", high = my_fill, na.value = "grey20") +
  labs(title = paste("Table-", table_count, " Skills co-occurrence"),
       x = NULL, y = NULL, fill = "N") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid = element_blank()
  )

print(p_cooc)
ggsave(file.path(out_dir, "skills_cooccurrence.png"),
       p_cooc, width = 9, height = 7, dpi = 300)
table_count <<- table_count + 1

# ── 23) Activités statistiques — importance & implication ─────────
df_us <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code), !is.na(involvement_code)) %>%
  mutate(
    theme      = factor(theme, levels = theme_levels),
    importance = factor(importance, levels = importance_levels),
    involvement = factor(involvement, levels = involvement_levels)
  )

importance_palette <- c(
  "Not at all important" = "#c0392b",
  "Slightly important"   = "#e09060",
  "Moderately important" = "#b0b8c1",
  "Important"            = "#6dbe8d",
  "Very important"       = "#1a7a4a"
)

# Importance — stacked 100%
df_imp <- df_us %>%
  count(theme, importance, name = "n") %>%
  group_by(theme) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  mutate(theme = forcats::fct_reorder(
    theme,
    ifelse(importance %in% c("Important", "Very important"), pct, 0), sum
  ))

p_imp <- ggplot(df_imp, aes(x = pct, y = theme, fill = importance)) +
  geom_col(width = 0.7) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = importance_palette) +
  labs(title = paste("Table-", table_count,
                     " Importance of statistical activities"),
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

print(p_imp)
ggsave(file.path(out_dir, "ustime_importance.png"),
       p_imp, width = 12, height = 6, dpi = 300)
table_count <<- table_count + 1

# Implication — stacked 100%
df_inv <- df_us %>%
  count(theme, involvement, name = "n") %>%
  group_by(theme) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  mutate(theme = forcats::fct_reorder(
    theme, ifelse(involvement == "No use", pct, 0), sum, .desc = TRUE
  ))

p_inv <- ggplot(df_inv, aes(x = pct, y = theme, fill = involvement)) +
  geom_col(width = 0.7) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "No use"                          = "#e8eaed",
    "Direct practice"                 = "#2C7FB8",
    "Supervision"                     = "#6dbe8d",
    "Direct practice and supervision" = "#1a4a7a"
  )) +
  labs(title = paste("Table-", table_count,
                     " Involvement in statistical activities"),
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

print(p_inv)
ggsave(file.path(out_dir, "ustime_involvement.png"),
       p_inv, width = 12, height = 6, dpi = 300)
table_count <<- table_count + 1

# Bubble : importance moyenne vs % actifs
df_bubble <- df_us %>%
  group_by(theme) %>%
  summarise(
    imp_mean   = mean(importance_code, na.rm = TRUE),
    active_pct = mean(involvement != "No use", na.rm = TRUE)
  )

p_bubble <- ggplot(df_bubble,
  aes(x = imp_mean, y = active_pct,
      label = stringr::str_wrap(as.character(theme), 25))) +
  geom_point(size = 5, color = my_fill, alpha = 0.8) +
  ggrepel::geom_label_repel(size = 3.2, fill = "white", color = "grey30",
                             box.padding = 0.6, max.overlaps = 10) +
  scale_x_continuous(limits = c(0, 4), breaks = 0:4,
    labels = c("Not at all", "Slightly", "Moderately",
               "Important", "Very important")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(title = paste("Table-", table_count,
                     " Statistical activities — importance vs active use"),
       x = "Mean importance score",
       y = "% respondents actively involved") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

print(p_bubble)
ggsave(file.path(out_dir, "ustime_bubble.png"),
       p_bubble, width = 11, height = 7, dpi = 300)
table_count <<- table_count + 1

message("✔ 04_plots_basic.R terminé")
