# ══════════════════════════════════════════════════════════════════
#  05_plots_advanced.R — Analyses croisées (section 5.4.1)
#  Dépend de : 01_config.R, 02_cleaning.R, 03_helpers.R
# ══════════════════════════════════════════════════════════════════

# ── Genre × Domaine de formation ──────────────────────────────────
df_fields <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "",
         !is.na(dmgender), dmgender != "") %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

gender_palette <- c(
  "Man"               = "#2C7FB8",
  "Woman"             = "#e05252",
  "Other"             = "#e6a817",
  "Prefer not to say" = "#6dbe8d"
)

df_plot <- df_fields %>%
  count(training_field, dmgender, name = "n") %>%
  group_by(training_field) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_gender_by_field <- ggplot(df_plot,
  aes(x = training_field, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = gender_palette, drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Gender distribution by training field"),
       x = "Training field", y = "Share within field", fill = "Gender") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

print(p_gender_by_field)
ggsave(file.path(out_dir, "gender_by_training_field.png"),
       p_gender_by_field, width = 14, height = 7, dpi = 300)
table_count <<- table_count + 1

# ── Genre × Lieu d'études ─────────────────────────────────────────
df_loc_gender <- clean_data %>%
  filter(!is.na(study_location), !is.na(dmgender)) %>%
  count(study_location, dmgender, name = "n") %>%
  group_by(study_location) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_loc_gender <- ggplot(df_loc_gender,
  aes(x = study_location, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge2(width = 0.85, preserve = "single"),
           width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(values = gender_palette, drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Gender distribution by study location"),
       x = NULL, y = "Share within location", fill = "Gender") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_loc_gender)
ggsave(file.path(out_dir, "gender_by_study_location.png"),
       p_loc_gender, width = 12, height = 6, dpi = 300)
table_count <<- table_count + 1

# ── Genre × Niveau de diplôme ─────────────────────────────────────
df_edu_gender <- clean_data %>%
  filter(!is.na(trlvl), !is.na(dmgender)) %>%
  count(trlvl, dmgender, name = "n") %>%
  group_by(trlvl) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_edu_gender <- ggplot(df_edu_gender,
  aes(x = trlvl, y = pct, fill = dmgender)) +
  geom_col(position = position_dodge2(width = 0.85, preserve = "single"),
           width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(values = gender_palette, drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Gender distribution by education level"),
       x = NULL, y = "Share within level", fill = "Gender") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_edu_gender)
ggsave(file.path(out_dir, "gender_by_education_level.png"),
       p_edu_gender, width = 12, height = 6, dpi = 300)
table_count <<- table_count + 1

# ── Lieu d'études × Domaine de formation ─────────────────────────
df_field_loc <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "",
         !is.na(study_location)) %>%
  mutate(training_field = factor(training_field, levels = training_field_study)) %>%
  count(training_field, study_location, name = "n") %>%
  group_by(training_field) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p_field_loc <- ggplot(df_field_loc,
  aes(x = training_field, y = pct, fill = study_location)) +
  geom_col(position = position_stack(), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Study location by training field"),
       x = NULL, y = "Share within field", fill = "Study location") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

print(p_field_loc)
ggsave(file.path(out_dir, "study_location_by_training_field.png"),
       p_field_loc, width = 14, height = 7, dpi = 300)
table_count <<- table_count + 1

# ── Séniorité × Niveau de diplôme (3 versions) ───────────────────
df_sen_edu <- clean_data %>%
  filter(!is.na(plsenior), !is.na(trlvl)) %>%
  count(trlvl, plsenior, name = "n") %>%
  group_by(trlvl) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# v1 — grouped bar
p_sen_edu_1 <- ggplot(df_sen_edu,
  aes(x = plsenior, y = pct, fill = trlvl)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Blues", drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Seniority level by education level — grouped bar"),
       x = NULL, y = "Share within education level", fill = "Education") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_sen_edu_1)
ggsave(file.path(out_dir, "seniority_by_education_v1_grouped.png"),
       p_sen_edu_1, width = 13, height = 7, dpi = 300)
table_count <<- table_count + 1

# v2 — stacked 100%
p_sen_edu_2 <- ggplot(df_sen_edu,
  aes(x = trlvl, y = pct, fill = plsenior)) +
  geom_col(width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, drop = FALSE) +
  labs(title = "Seniority level by education level — stacked bar",
       x = NULL, y = "Share within education level", fill = "Seniority") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

print(p_sen_edu_2)
ggsave(file.path(out_dir, "seniority_by_education_v2_stacked.png"),
       p_sen_edu_2, width = 13, height = 7, dpi = 300)

# v3 — facet
p_sen_edu_3 <- ggplot(df_sen_edu, aes(x = plsenior, y = pct)) +
  geom_col(fill = my_fill, width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ trlvl, nrow = 2) +
  labs(title = paste("Table-", table_count,
                     " Seniority level by education level — facet"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

print(p_sen_edu_3)
ggsave(file.path(out_dir, "seniority_by_education_v3_facet.png"),
       p_sen_edu_3, width = 14, height = 8, dpi = 300)
table_count <<- table_count + 1

# ── Profils d'activité (règles manuelles — TBD) ───────────────────
# TODO: règles à valider sur données réelles ; remplacer potentiellement par k-means

df_scores <- clean_data %>%
  mutate(id = row_number()) %>%
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code)) %>%
  select(id, theme_id, importance_code) %>%
  tidyr::pivot_wider(names_from = theme_id, values_from = importance_code,
                     names_prefix = "theme_") %>%
  rename(cleaning    = theme_1, descriptive = theme_2,
         inferential = theme_3, modeling    = theme_4,
         automation  = theme_5, supervision = theme_6)

df_scores <- df_scores %>%
  mutate(
    job_profile = case_when(
      supervision >= 3 & supervision == pmax(cleaning, descriptive,
        inferential, modeling, automation, supervision)
        ~ "Manager / Supervisor",
      (modeling >= 3 | automation >= 3) &
        pmax(modeling, automation) >= pmax(cleaning, descriptive, inferential)
        ~ "Data Scientist / Engineer",
      inferential >= 3 &
        inferential >= pmax(cleaning, descriptive, modeling)
        ~ "Statistician",
      (descriptive >= 3 | cleaning >= 3) &
        pmax(descriptive, cleaning) >= pmax(inferential, modeling)
        ~ "Data Analyst",
      TRUE ~ "Generalist"
    ),
    job_profile = factor(job_profile, levels = c(
      "Data Analyst", "Statistician", "Data Scientist / Engineer",
      "Manager / Supervisor", "Generalist"
    ))
  )

clean_data <- clean_data %>%
  mutate(id = row_number()) %>%
  left_join(df_scores %>% select(id, job_profile), by = "id") %>%
  select(-id)

save_barplot_counts(clean_data, "job_profile",
                    "[TBD] Activity-based job profiles (rules to be validated)",
                    "jobprofile_distribution.png")

save_donut_counts(clean_data, "job_profile",
                  title    = "[TBD] Activity-based job profiles",
                  filename = "donut_jobprofile.png",
                  drop_na  = TRUE)

df_role_profile <- clean_data %>%
  filter(!is.na(job_role), !is.na(job_profile)) %>%
  count(job_profile, job_role, name = "n") %>%
  group_by(job_profile) %>%
  slice_max(n, n = 8) %>%
  mutate(job_role = forcats::fct_reorder(job_role, n)) %>%
  ungroup()

p_role_profile <- ggplot(df_role_profile, aes(x = n, y = job_role)) +
  geom_col(fill = my_fill, color = my_border) +
  facet_wrap(~ job_profile, scales = "free_y", ncol = 2) +
  labs(title = paste("Table-", table_count,
                     " [TBD] Top job roles by activity-based profile"),
       subtitle = "Rules-based profiles — to be validated on real data",
       x = "N respondents", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50", face = "italic"),
        strip.text    = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

print(p_role_profile)
ggsave(file.path(out_dir, "jobrole_by_profile_TBD.png"),
       p_role_profile, width = 14, height = 10, dpi = 300)
table_count <<- table_count + 1

# ── Genre × Domaine × Lieu (boucles) ─────────────────────────────
df_field_gender_loc <- clean_data %>%
  tidyr::unnest_longer(training_fields_list, values_to = "training_field") %>%
  mutate(training_field = trimws(as.character(training_field))) %>%
  filter(!is.na(training_field), training_field != "",
         !is.na(dmgender), !is.na(study_location)) %>%
  mutate(training_field = factor(training_field, levels = training_field_study))

# Par lieu d'études
for (loc in levels(df_field_gender_loc$study_location)) {
  df_g1 <- df_field_gender_loc %>%
    filter(study_location == loc) %>%
    count(training_field, dmgender, name = "n") %>%
    group_by(training_field) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()

  if (nrow(df_g1) == 0) next

  p_g1 <- ggplot(df_g1, aes(x = training_field, y = pct, fill = dmgender)) +
    geom_col(width = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = gender_palette, drop = FALSE) +
    labs(title = paste0("Table-", table_count,
                        " Gender by training field — ", loc),
         x = NULL, y = "Share within field", fill = "Gender") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          panel.grid.major.x = element_blank())

  safe_loc <- gsub("[^A-Za-z0-9]+", "_", loc)
  ggsave(file.path(out_dir, paste0("field_gender_loc_", safe_loc, ".png")),
         p_g1, width = 14, height = 7, dpi = 300)
  table_count <<- table_count + 1
}

# Par genre
for (gen in levels(df_field_gender_loc$dmgender)) {
  df_g2 <- df_field_gender_loc %>%
    filter(dmgender == gen) %>%
    count(training_field, study_location, name = "n") %>%
    group_by(training_field) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()

  if (nrow(df_g2) == 0) next

  p_g2 <- ggplot(df_g2,
    aes(x = training_field, y = pct, fill = study_location)) +
    geom_col(width = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set2", drop = FALSE) +
    labs(title = paste0("Table-", table_count,
                        " Study location by training field — ", gen),
         x = NULL, y = "Share within field", fill = "Study location") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          panel.grid.major.x = element_blank())

  safe_gen <- gsub("[^A-Za-z0-9]+", "_", gen)
  ggsave(file.path(out_dir, paste0("field_location_gender_", safe_gen, ".png")),
         p_g2, width = 14, height = 7, dpi = 300)
  table_count <<- table_count + 1
}

# ── Importance activités statistiques par secteur ─────────────────
importance_palette <- c(
  "Not at all important" = "#c0392b",
  "Slightly important"   = "#e09060",
  "Moderately important" = "#b0b8c1",
  "Important"            = "#6dbe8d",
  "Very important"       = "#1a7a4a"
)

df_us_sec <- clean_data %>%
  mutate(id = row_number()) %>%
  select(id, ustime, plsector) %>%
  tidyr::unnest(ustime) %>%
  filter(!is.na(importance_code)) %>%
  mutate(theme      = factor(theme, levels = theme_levels),
         importance = factor(importance, levels = importance_levels))

plot_importance <- function(data, title) {
  data %>%
    count(theme, importance, name = "n") %>%
    group_by(theme) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    mutate(theme = forcats::fct_reorder(
      theme,
      ifelse(importance %in% c("Important", "Very important"), pct, 0), sum
    )) %>%
    ggplot(aes(x = pct, y = theme, fill = importance)) +
    geom_col(width = 0.7) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = importance_palette, drop = FALSE) +
    labs(title = paste("Table-", table_count, " ", title),
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1))
}

p_imp_overall <- plot_importance(df_us_sec,
  "Importance of statistical activities — overall")
print(p_imp_overall)
ggsave(file.path(out_dir, "ustime_importance_overall.png"),
       p_imp_overall, width = 12, height = 6, dpi = 300)
table_count <<- table_count + 1

for (sec in levels(df_us_sec$plsector)) {
  df_sec  <- df_us_sec %>% filter(plsector == sec)
  n_resp  <- n_distinct(df_sec$id)
  if (n_resp < 5) next

  p_sec <- plot_importance(df_sec,
    paste0("Importance of statistical activities — ", sec,
           " (n=", n_resp, ")"))
  safe_sec <- gsub("[^A-Za-z0-9]+", "_", sec)
  ggsave(file.path(out_dir, paste0("ustime_importance_sector_", safe_sec, ".png")),
         p_sec, width = 12, height = 6, dpi = 300)
  table_count <<- table_count + 1
}

# ── Expérience et séniorité par secteur ───────────────────────────
df_sector <- clean_data %>%
  mutate(plsector = as.character(plsector),
         plsenior = as.character(plsenior)) %>%
  filter(!is.na(plsector), plsector != "None", plsector != "NA",
         !is.na(plsenior), plsenior != "Never worked",
         !is.na(plyexp)) %>%
  mutate(plsector = factor(plsector),
         plsenior = factor(plsenior, levels = seniority_level_levels)) %>%
  droplevels()

sector_order <- df_sector %>%
  group_by(plsector) %>%
  summarise(med = median(plyexp, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(plsector)

p_exp_sector <- df_sector %>%
  mutate(plsector = factor(plsector, levels = sector_order)) %>%
  ggplot(aes(x = plyexp, y = plsector)) +
  geom_boxplot(fill = my_fill, color = "grey30",
               alpha = 0.7, outlier.size = 1.5) +
  labs(title = paste("Table-", table_count,
                     " Years of experience by sector"),
       x = "Years of experience", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

print(p_exp_sector)
ggsave(file.path(out_dir, "experience_by_sector.png"),
       p_exp_sector, width = 12, height = 10, dpi = 300)
table_count <<- table_count + 1

seniority_palette <- c(
  "Intern / Entry level position" = "#c0392b",
  "No managerial function"        = "#e09060",
  "Lower management"              = "#b0b8c1",
  "Middle management"             = "#6dbe8d",
  "Top management"                = "#1a7a4a",
  "Never worked"                  = "#cccccc"
)

sector_order_sen <- df_sector %>%
  count(plsector, plsenior, name = "n") %>%
  group_by(plsector) %>%
  mutate(pct = n / sum(n)) %>%
  filter(plsenior %in% c("Top management", "Middle management")) %>%
  summarise(senior_pct = sum(pct)) %>%
  arrange(senior_pct) %>%
  pull(plsector)

p_sen_sector <- df_sector %>%
  count(plsector, plsenior, name = "n") %>%
  group_by(plsector) %>%
  mutate(pct = n / sum(n),
         plsector = factor(plsector, levels = sector_order_sen)) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = plsector, fill = plsenior)) +
  geom_col(width = 0.8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_fill_manual(values = seniority_palette, drop = FALSE) +
  labs(title = paste("Table-", table_count,
                     " Seniority level by sector"),
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

print(p_sen_sector)
ggsave(file.path(out_dir, "seniority_by_sector.png"),
       p_sen_sector, width = 13, height = 10, dpi = 300)
table_count <<- table_count + 1

# ── Formation continue par secteur / séniorité / expérience ───────
cont_edu_palette <- c(
  "No"                                                 = "#cccccc",
  "MAS, DAS, CAS"                                      = "#2C7FB8",
  "Certified online training (Coursera, Edx, etc.)"    = "#6dbe8d",
  "Postgraduate in Business/Finance (MBA, EMBA, etc.)" = "#e6a817",
  "Post-Doc"                                           = "#e05252",
  "Further training with an employer"                  = "#1a4a7a"
)

save_donut_counts(clean_data, "continuous_education",
                  title    = paste("Table-", table_count,
                                   " Continuous education — overall"),
                  filename = "cont_edu_overall.png",
                  palette  = cont_edu_palette, drop_na = TRUE)
table_count <<- table_count + 1

# Par secteur
df_cont_sector <- clean_data %>%
  mutate(plsector = as.character(plsector)) %>%
  filter(!is.na(plsector), plsector != "None", plsector != "NA",
         !is.na(continuous_education)) %>%
  mutate(plsector = factor(plsector))

for (sec in levels(df_cont_sector$plsector)) {
  df_sec <- df_cont_sector %>% filter(plsector == sec)
  if (nrow(df_sec) < 5) next
  safe_sec <- gsub("[^A-Za-z0-9]+", "_", sec)
  save_donut_counts(df_sec, "continuous_education",
    title    = paste0("Table-", table_count,
                      " Continuous education — ", sec, " (n=", nrow(df_sec), ")"),
    filename = paste0("cont_edu_sector_", safe_sec, ".png"),
    palette  = cont_edu_palette, drop_na = TRUE)
  table_count <<- table_count + 1
}

# Par séniorité
df_cont_senior <- clean_data %>%
  mutate(plsenior = as.character(plsenior)) %>%
  filter(!is.na(plsenior), plsenior != "Never worked",
         !is.na(continuous_education)) %>%
  mutate(plsenior = factor(plsenior, levels = seniority_level_levels))

for (sen in levels(df_cont_senior$plsenior)) {
  df_sen <- df_cont_senior %>% filter(plsenior == sen)
  if (nrow(df_sen) < 5) next
  safe_sen <- gsub("[^A-Za-z0-9]+", "_", sen)
  save_donut_counts(df_sen, "continuous_education",
    title    = paste0("Table-", table_count,
                      " Continuous education — ", sen, " (n=", nrow(df_sen), ")"),
    filename = paste0("cont_edu_seniority_", safe_sen, ".png"),
    palette  = cont_edu_palette, drop_na = TRUE)
  table_count <<- table_count + 1
}

# Par tranche d'expérience
df_cont_exp <- clean_data %>%
  filter(!is.na(plyexp), !is.na(continuous_education)) %>%
  mutate(exp_group = cut(plyexp,
    breaks = c(0, 5, 10, 20, Inf),
    labels = c("0–5 years", "6–10 years", "11–20 years", "20+ years"),
    right = TRUE, include.lowest = TRUE))

for (grp in levels(df_cont_exp$exp_group)) {
  df_grp <- df_cont_exp %>% filter(exp_group == grp)
  if (nrow(df_grp) < 5) next
  safe_grp <- gsub("[^A-Za-z0-9]+", "_", grp)
  save_donut_counts(df_grp, "continuous_education",
    title    = paste0("Table-", table_count,
                      " Continuous education — ", grp, " (n=", nrow(df_grp), ")"),
    filename = paste0("cont_edu_exp_", safe_grp, ".png"),
    palette  = cont_edu_palette, drop_na = TRUE)
  table_count <<- table_count + 1
}

message("✔ 05_plots_advanced.R terminé")
