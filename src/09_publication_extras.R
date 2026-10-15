# ══════════════════════════════════════════════════════════════════
#  09_publication_extras.R — values and figures for the publication draft
#  (SSS_Publication_Draft v0.2). Sourced LAST from analysis.R.
#
#  Writes to publication_outputs/ :
#    publication_tables.xlsx             -> ONE Excel workbook, one sheet per table
#                                           (+ Figures sheet with Figures 13-14);
#                                           falls back to CSV if openxlsx is missing.
#  Underlying sheets (same names as the former CSV files):
#    tab_sample_flow.csv                 -> Table 2
#    tab_salary_sector_by_career_stage.csv, tab_salary_degree_by_career_stage.csv -> Table 6
#    tab_salary_table5_extended.csv      -> Table 5 (management, degree, gender, region)
#    tab_table5b_region_by_management.csv, tab_salary_region_by_management.csv
#                                        -> Section 7.4 (salary, region x management)
#    tab_gender_profile.csv              -> Table 7 (profile rows)
#    tab_management_by_gender.csv        -> Section 6.1 / 7.6 (seniority level x gender)
#    tab_gender_gap_by_career_stage.csv  -> Table 7 / Figure 13
#    tab_gender_gap_by_management.csv    -> Figure 14 / Section 7.6 (salary, management x gender)
#    fig13_salary_gender_career_stage.png, fig14_salary_gender_management.png
#    table6_like_with_like.png           -> Table 6, laid out as in the draft
#    table7_women_men.png (+ sheet table7_women_men) -> Table 7, laid out as in the draft
#
#  Rules: eligible sample only (done in 02_cleaning.R), work rate >= 50%,
#  medians and Q1-Q3 only (no means), groups with n < GROUP_MIN_N (10) hidden,
#  open-ended top band reported as ">= 200 000".
# ══════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

PUB_MIN_N <- GROUP_MIN_N
pub_dir <- "publication_outputs"
if (!dir.exists(pub_dir)) dir.create(pub_dir, recursive = TRUE)
message("── Publication extras → ", pub_dir, " ──")

# Tables are collected here and written at the end into a single Excel workbook.
pub_sheets <- list()
write_pub <- function(df, name) {
  sheet <- substr(sub("^tab_", "", tools::file_path_sans_ext(name)), 1, 31)
  pub_sheets[[sheet]] <<- df
  invisible(df)
}

# ── Table 2: sample flow (computed in 02_cleaning.R) ────────────────
if (exists("sample_flow")) write_pub(sample_flow, "tab_sample_flow.csv")

# ── Salary sample: eligible, work rate >= 50%, FTE salary available ─
sal <- clean_data |>
  filter(!is.na(salary_fte_mid), is.finite(salary_fte_mid)) |>
  mutate(
    gender = case_when(
      dmgender == "Woman" ~ "Women",
      dmgender == "Man"   ~ "Men",
      TRUE ~ NA_character_
    ),
    mgmt_level = case_when(
      plsenior == "No managerial function" ~ "No management",
      plsenior %in% c("Lower management", "Middle management") ~ "Lower / middle management",
      plsenior == "Top management" ~ "Top management",
      TRUE ~ NA_character_
    ),
    in_management = plsenior %in% c("Lower management", "Middle management", "Top management"),
    sector_pub = case_when(
      plsector == "Higher education / Research / Academia" ~ "Academia / research",
      plsector == "Government / Public administration"     ~ "Public administration",
      plsector == "Pharmaceuticals"                        ~ "Pharmaceuticals",
      !is.na(plsector)                                     ~ "Other sectors",
      TRUE ~ NA_character_
    ),
    degree_pub = case_when(
      trlvl == "PhD" ~ "Doctorate (PhD)",
      trlvl %in% c("University master", "Master of applied science") ~ "Master (university or UAS)",
      TRUE ~ NA_character_
    )
  )

# Median / Q1 / Q3 with the n >= PUB_MIN_N rule and top-band label
sal_stats <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      n      = n(),
      q1     = unname(quantile(salary_fte_mid, 0.25)),
      median = median(salary_fte_mid),
      q3     = unname(quantile(salary_fte_mid, 0.75)),
      .groups = "drop"
    ) |>
    mutate(
      shown        = n >= PUB_MIN_N,
      q1           = ifelse(shown, q1, NA_real_),
      median       = ifelse(shown, median, NA_real_),
      q3           = ifelse(shown, q3, NA_real_),
      median_label = ifelse(shown, salary_label(median), "–"),
      cell         = ifelse(shown, paste0(median_label, " (", n, ")"), "–")
    ) |>
    select(-shown)
}

# ── Table 6: like with like (within career stages) ─────────────────
sec_cs <- sal |>
  filter(!is.na(sector_pub), !is.na(career_stage)) |>
  sal_stats(group = sector_pub, career_stage)
write_pub(sec_cs, "tab_salary_sector_by_career_stage.csv")

deg_cs <- sal |>
  filter(!is.na(degree_pub), !is.na(career_stage)) |>
  sal_stats(group = degree_pub, career_stage)
write_pub(deg_cs, "tab_salary_degree_by_career_stage.csv")

# Table 6 as it appears in the report: rows = sector / degree (report order),
# columns = career stage, cells = "median (n)", "–" when n < PUB_MIN_N or empty.
table6_rows <- c("Academia / research", "Public administration", "Pharmaceuticals",
                 "Other sectors", "Master (university or UAS)", "Doctorate (PhD)")
table6_cols <- setNames(c("0–5 years", "6–15 years", "16+ years"),
                        levels(clean_data$career_stage))
table6_wide <- bind_rows(
  sec_cs |> mutate(block = "Sector"),
  deg_cs |> mutate(block = "Highest degree")
) |>
  mutate(career_stage = factor(table6_cols[as.character(career_stage)],
                               levels = unname(table6_cols))) |>
  select(block, group, career_stage, cell) |>
  complete(nesting(block, group), career_stage, fill = list(cell = "–")) |>
  pivot_wider(names_from = career_stage, values_from = cell) |>
  mutate(group = factor(group, levels = table6_rows)) |>
  arrange(group) |>
  mutate(group = as.character(group))
write_pub(table6_wide, "tab_table6_like_with_like.csv")

# Report-style table image (layout of Tables 6-7 in the draft): dark-blue
# header, striped rows, caption on top, source note below.
pub_source_note <- function(n, extra = "") paste0(
  "Source: SSS Work and Salary Survey 2026, n = ", n,
  ". FTE salary = band midpoint ÷ work rate × 100. Groups with n < ", PUB_MIN_N,
  " not shown.", extra)

save_pub_table <- function(gt_obj, caption, note, file, center_cols) {
  gt_obj <- gt_obj |>
    gt::tab_header(title = gt::md(caption)) |>
    gt::cols_align(align = "center", columns = dplyr::all_of(center_cols)) |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_row_groups()) |>
    gt::tab_source_note(note) |>
    gt::tab_options(
      table.font.size                 = gt::px(13),
      heading.align                   = "left",
      heading.title.font.size         = gt::px(13),
      column_labels.background.color  = "#1F3864",
      column_labels.font.weight       = "bold",
      row_group.background.color      = "#FFFFFF",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#F2F5F9",
      source_notes.font.size          = gt::px(10),
      data_row.padding                = gt::px(4)
    )
  gt::gtsave(gt_obj, file.path(pub_dir, file))
  invisible(gt_obj)
}

save_pub_table(
  table6_wide |> gt::gt(rowname_col = "group", groupname_col = "block"),
  "**Table 6.** *Median FTE gross annual salary (CHF) by sector and degree, within career stages (n in brackets)*",
  pub_source_note(sum(!is.na(sal$career_stage)),
                  paste0(" – = fewer than ", PUB_MIN_N, " respondents.")),
  "table6_like_with_like.png",
  center_cols = unname(table6_cols)
)

# ── Table 5: salary by management level, degree, gender and region ──
# Regions = Swiss large regions (NUTS-2) built from the main work canton.
# Regions with fewer than PUB_MIN_N salaries are merged into "Other regions".
# If the merged group is still below PUB_MIN_N it is shown as "\u2013" (n < 10 rule).
region_of_canton <- c(
  ZH = "Zurich",
  BS = "Northwestern Switzerland", BL = "Northwestern Switzerland", AG = "Northwestern Switzerland",
  BE = "Espace Mittelland", FR = "Espace Mittelland", SO = "Espace Mittelland",
  NE = "Espace Mittelland", JU = "Espace Mittelland",
  VD = "Lake Geneva region", VS = "Lake Geneva region", GE = "Lake Geneva region",
  GL = "Eastern Switzerland", SH = "Eastern Switzerland", AR = "Eastern Switzerland",
  AI = "Eastern Switzerland", SG = "Eastern Switzerland", GR = "Eastern Switzerland",
  TG = "Eastern Switzerland",
  LU = "Central Switzerland", UR = "Central Switzerland", SZ = "Central Switzerland",
  OW = "Central Switzerland", NW = "Central Switzerland", ZG = "Central Switzerland",
  TI = "Ticino"
)
sal$region <- unname(region_of_canton[as.character(sal$dmwork)])  # NA: abroad / no work

reg_n <- sort(table(sal$region[!is.na(sal$region)]))
merged <- names(reg_n)[reg_n < PUB_MIN_N]
# Labels: each region lists its cantons; a merged group lists the regions it contains.
cantons_of <- function(reg) paste(names(region_of_canton)[region_of_canton == reg], collapse = ", ")
other_label <- if (length(merged) > 0)
  paste0("Other regions (", paste(sort(merged), collapse = ", "), ")") else NA_character_
sal$region_pub <- ifelse(
  is.na(sal$region), NA_character_,
  ifelse(sal$region %in% merged, other_label,
         paste0(sal$region, " (", vapply(sal$region, cantons_of, ""), ")"))
)
region_note <- if (length(merged) > 0)
  paste0("Merged because fewer than ", PUB_MIN_N, " salaries each: ",
         paste(sort(merged), collapse = ", ")) else "No regions merged"
message("  Regions: ", region_note)

table5 <- bind_rows(
  sal |> filter(!is.na(mgmt_level)) |>
    sal_stats(group = mgmt_level) |> mutate(dimension = "Management level"),
  sal |> filter(!is.na(trlvl)) |>
    sal_stats(group = trlvl) |> mutate(dimension = "Highest degree"),
  sal |> filter(!is.na(gender)) |>
    sal_stats(group = gender) |> mutate(dimension = "Gender"),
  sal |> filter(!is.na(region_pub)) |>
    sal_stats(group = region_pub) |> mutate(dimension = "Work region")
) |>
  mutate(group = as.character(group),
         note  = ifelse(!is.na(other_label) & group == other_label, region_note, "")) |>
  select(dimension, group, n, q1, median, q3, median_label, note)
write_pub(table5, "tab_salary_table5_extended.csv")

# ── Table 5b: salary by work region and management level (Section 7.4) ──
# Rows = work region, columns = management level, cells = "median (n)";
# cells with n < PUB_MIN_N are shown as "–".
mgmt_order <- c("No management", "Lower / middle management", "Top management")
region_mgmt <- bind_rows(
  sal |> filter(!is.na(region_pub), !is.na(mgmt_level)) |>
    sal_stats(group = region_pub, mgmt_level),
  sal |> filter(!is.na(region_pub), !is.na(mgmt_level)) |>
    sal_stats(group = region_pub) |> mutate(mgmt_level = "All levels")
) |>
  mutate(mgmt_level = factor(mgmt_level, levels = c(mgmt_order, "All levels"))) |>
  arrange(group, mgmt_level)
write_pub(region_mgmt, "tab_salary_region_by_management.csv")
write_pub(region_mgmt |>
            select(group, mgmt_level, cell) |>
            pivot_wider(names_from = mgmt_level, values_from = cell, values_fill = "–"),
          "tab_table5b_region_by_management.csv")

# ── Table 7: women and men ──────────────────────────────────────────
gsal <- sal |> filter(!is.na(gender))

gender_profile <- gsal |>
  group_by(gender) |>
  summarise(
    n                  = n(),
    median_experience  = median(plyexp, na.rm = TRUE),
    # Same denominator as is_manager: known seniority level only
    pct_management     = 100 * mean(in_management[!is.na(plsenior) &
                                                    !plsenior %in% c("Never worked", "Other")]),
    pct_phd            = 100 * mean(trlvl == "PhD", na.rm = TRUE),
    pct_part_time      = 100 * mean(plrate < 100, na.rm = TRUE),
    median_salary_fte  = median(salary_fte_mid),
    .groups = "drop"
  ) |>
  mutate(median_salary_label = salary_label(median_salary_fte))
write_pub(gender_profile, "tab_gender_profile.csv")

# Management level by gender — whole eligible sample (not only salaries).
# % = share of women (resp. men) at each level; "Never worked" / "Other" /
# missing seniority are left out of the denominator.
mgmt_gender <- clean_data |>
  mutate(gender = case_when(dmgender == "Woman" ~ "Women",
                            dmgender == "Man"   ~ "Men",
                            TRUE ~ NA_character_)) |>
  filter(!is.na(gender), !is.na(plsenior),
         !plsenior %in% c("Never worked", "Other")) |>
  mutate(plsenior = droplevels(plsenior)) |>
  count(gender, seniority = plsenior, name = "n", .drop = FALSE) |>
  group_by(gender) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  pivot_wider(names_from = gender, values_from = c(n, pct), values_fill = 0)
write_pub(mgmt_gender, "tab_management_by_gender.csv")

gap_table <- function(stats_long, by) {
  # stats_long: output of sal_stats with `gender` and `by`
  stats_long |>
    select(all_of(c(by, "gender", "n", "q1", "median", "q3"))) |>
    pivot_wider(names_from = gender, values_from = c(n, q1, median, q3)) |>
    mutate(gap_pct = ifelse(!is.na(median_Women) & !is.na(median_Men),
                            100 * (median_Women - median_Men) / median_Men, NA_real_))
}

gender_cs   <- gsal |> filter(!is.na(career_stage)) |>
  sal_stats(gender, career_stage)
# All three management levels; groups with n < PUB_MIN_N stay "–" in the
# table and are dropped from Figure 14 (top management will usually be).
gender_mgmt <- gsal |> filter(!is.na(mgmt_level)) |>
  sal_stats(gender, mgmt_level) |>
  mutate(mgmt_level = factor(mgmt_level, levels = mgmt_order)) |>
  arrange(mgmt_level, gender)

# Overall gap (all career stages)
gender_all <- gsal |> sal_stats(gender)
gap_overall <- if (all(c("Women", "Men") %in% gender_all$gender)) {
  m <- setNames(gender_all$median, gender_all$gender)
  100 * (m[["Women"]] - m[["Men"]]) / m[["Men"]]
} else NA_real_
message("  Overall median gap women vs men: ", round(gap_overall, 1), "%")

gap_cs <- gap_table(gender_cs, "career_stage")
write_pub(gap_cs, "tab_gender_gap_by_career_stage.csv")
write_pub(gap_table(gender_mgmt, "mgmt_level"),   "tab_gender_gap_by_management.csv")

# ── Table 7 as it appears in the report (Women | Men | Difference) ──
fmt_gap <- function(g) ifelse(is.na(g), "",
                              paste0(ifelse(g < 0, "−", "+"), round(abs(g)), "%"))
prof <- function(col, g, digits = 1) {
  v <- gender_profile[[col]][gender_profile$gender == g]
  if (length(v) == 0 || is.na(v)) "–" else formatC(v, format = "f", digits = digits)
}
cs_cell <- function(stage, g) {
  v <- gender_cs$cell[gender_cs$gender == g & as.character(gender_cs$career_stage) == stage]
  if (length(v) == 0) "–" else v
}
med_all <- function(g) {
  v <- gender_profile$median_salary_label[gender_profile$gender == g]
  if (length(v) == 0) "–" else v
}
# Indented sub-rows (non-breaking spaces survive in the gt stub)
stage_rows <- paste0("   ", c("0–5 years of experience", "6–15 years", "16+ years"))

table7 <- data.frame(
  row   = c("n", "Median years of experience", "In a management position (%)",
            "PhD (%)", "Working part-time (%)", "Median FTE salary, all (CHF)",
            stage_rows),
  Women = c(prof("n", "Women", 0), prof("median_experience", "Women", 0),
            prof("pct_management", "Women"), prof("pct_phd", "Women"),
            prof("pct_part_time", "Women"), med_all("Women"),
            vapply(levels(clean_data$career_stage), cs_cell, "", g = "Women")),
  Men   = c(prof("n", "Men", 0), prof("median_experience", "Men", 0),
            prof("pct_management", "Men"), prof("pct_phd", "Men"),
            prof("pct_part_time", "Men"), med_all("Men"),
            vapply(levels(clean_data$career_stage), cs_cell, "", g = "Men")),
  Difference = c(rep("", 5), fmt_gap(gap_overall),
                 vapply(levels(clean_data$career_stage), function(s) {
                   fmt_gap(gap_cs$gap_pct[as.character(gap_cs$career_stage) == s][1])
                 }, "")),
  stringsAsFactors = FALSE, row.names = NULL
)
write_pub(table7, "tab_table7_women_men.csv")

save_pub_table(
  table7 |>
    gt::gt(rowname_col = "row") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = list(gt::cells_body(rows = 6), gt::cells_stub(rows = 6))),
  "**Table 7.** *Women and men compared*",
  pub_source_note(nrow(gsal), " Difference = (women − men) / men."),
  "table7_women_men.png",
  center_cols = c("Women", "Men", "Difference")
)

# ── Figures 13 and 14: median (dot) and Q1–Q3 (bar) by gender ──────
gender_cols <- c(Women = "#2b6cb0", Men = "#e8683f")

plot_gender_dots <- function(stats_long, ycol, title, subtitle, file) {
  dd <- stats_long |>
    filter(n >= PUB_MIN_N, !is.na(median)) |>
    mutate(
      gender = factor(gender, levels = c("Women", "Men")),
      lab    = paste0(gender, " (n=", n, ")")
    )
  if (nrow(dd) == 0) {
    message("  (no group with n >= ", PUB_MIN_N, " — ", file, " not created)")
    return(invisible(NULL))
  }
  # Explicit numeric y positions (men just above, women just below each group
  # line): position_dodge() does not shift geom_text vertically here, which made
  # the "(n=...)" labels of both genders overlap on the group line.
  dd[[ycol]] <- droplevels(factor(dd[[ycol]]))
  grp_levels <- levels(dd[[ycol]])
  dd$y_pos <- as.integer(dd[[ycol]]) + ifelse(dd$gender == "Men", 0.15, -0.15)
  p <- ggplot(dd, aes(y = y_pos, colour = gender)) +
    geom_linerange(aes(xmin = q1, xmax = q3),
                   linewidth = 3.2, alpha = 0.35, lineend = "round") +
    geom_point(aes(x = median), size = 3.6) +
    geom_text(aes(x = q3, label = lab), hjust = 0, nudge_x = 2500,
              size = 3, show.legend = FALSE, colour = "grey30") +
    scale_y_continuous(breaks = seq_along(grp_levels), labels = grp_levels,
                       expand = expansion(add = 0.4)) +
    scale_colour_manual(values = gender_cols, name = NULL) +
    scale_x_continuous(labels = scales::label_number(big.mark = "'"),
                       expand = expansion(mult = c(0.03, 0.22))) +
    labs(title = title, subtitle = subtitle, x = "FTE gross annual salary (CHF)", y = NULL,
         caption = paste0("Groups with n < ", PUB_MIN_N,
                          " not shown. Work rate ≥ 50%. FTE = band midpoint / work rate × 100.")) +
    theme_sss(horizontal = TRUE) +
    theme(legend.position = "top", legend.justification = "left")
  ggsave(file.path(pub_dir, file), p, width = 10, height = 5.5, dpi = 300, bg = sss_bg)
  invisible(p)
}

plot_gender_dots(
  gender_cs |> mutate(career_stage = factor(career_stage, levels = rev(levels(clean_data$career_stage)))),
  "career_stage",
  "Salary by gender and career stage",
  "Median FTE gross annual salary (dot) and interquartile range (bar), CHF",
  "fig13_salary_gender_career_stage.png"
)
plot_gender_dots(
  gender_mgmt |> mutate(mgmt_level = factor(mgmt_level, levels = rev(mgmt_order))),
  "mgmt_level",
  "Salary by gender and management level",
  "Median FTE gross annual salary (dot) and interquartile range (bar), CHF",
  "fig14_salary_gender_management.png"
)

# ══════════════════════════════════════════════════════════════════
#  Excel workbook — real .xlsx, with typed cells (no CSV-in-Excel)
#
#  Numbers are written as numbers and formatted by Excel (CHF, %, n),
#  so the tables can be sorted, filtered and charted natively.
#  Numeric columns leave hidden groups (n < PUB_MIN_N) empty so the column
#  stays numeric; the "–" convention is kept in the text label columns.
# ══════════════════════════════════════════════════════════════════

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  message("  openxlsx absent — installation en cours...")
  try(utils::install.packages("openxlsx", repos = "https://cloud.r-project.org"), silent = TRUE)
}

# Readable column headers for the workbook
pub_headers <- c(
  n = "n", q1 = "Q1 (CHF)", median = "Median (CHF)", q3 = "Q3 (CHF)",
  median_label = "Median (as published)", cell = "Median (n)",
  dimension = "Dimension", group = "Group", block = "Block",
  career_stage = "Career stage", mgmt_level = "Management level",
  gender = "Gender", note = "Note", Step = "Step",
  n_Women = "n (women)", n_Men = "n (men)",
  q1_Women = "Q1 women (CHF)", q1_Men = "Q1 men (CHF)",
  median_Women = "Median women (CHF)", median_Men = "Median men (CHF)",
  q3_Women = "Q3 women (CHF)", q3_Men = "Q3 men (CHF)",
  gap_pct = "Gap women vs men (%)",
  median_experience = "Median experience (years)",
  pct_management = "In management (%)", pct_phd = "PhD (%)",
  pct_part_time = "Part-time (%)",
  seniority = "Seniority level", row = "",
  pct_Women = "Women (%)", pct_Men = "Men (%)",
  median_salary_fte = "Median FTE salary (CHF)",
  median_salary_label = "Median salary (as published)"
)

# Which Excel number format applies to which column
fmt_for <- function(col) {
  if (grepl("^(n|n_)", col))                      return("#,##0")
  if (grepl("CHF|^(q1|q3|median)", col))          return("#,##0")
  if (grepl("pct|gap|%", col, ignore.case = TRUE)) return("0.0")
  if (grepl("experience", col))                   return("0.0")
  NULL
}

sheet_order <- c("sample_flow", "salary_table5_extended",
                 "table5b_region_by_management", "salary_region_by_management",
                 "table6_like_with_like",
                 "salary_sector_by_career_stage", "salary_degree_by_career_stage",
                 "table7_women_men", "gender_profile", "management_by_gender",
                 "gender_gap_by_career_stage", "gender_gap_by_management")
sheet_order <- c(intersect(sheet_order, names(pub_sheets)),
                 setdiff(names(pub_sheets), sheet_order))

xlsx_path <- file.path(pub_dir, "publication_tables.xlsx")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  st_header <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FFFFFF",
                                     fgFill = "#1F3864", halign = "center",
                                     valign = "center", wrapText = TRUE, border = "TopBottom")
  st_text   <- openxlsx::createStyle(halign = "left", valign = "center")

  for (nm in sheet_order) {
    df <- as.data.frame(pub_sheets[[nm]], stringsAsFactors = FALSE)

    # Round salary statistics to whole francs (they are midpoint estimates)
    for (cl in intersect(c("q1", "median", "q3", "median_salary_fte",
                           "q1_Women", "q1_Men", "median_Women", "median_Men",
                           "q3_Women", "q3_Men"), names(df))) {
      df[[cl]] <- round(as.numeric(df[[cl]]))
    }
    for (cl in intersect(c("gap_pct", "pct_management", "pct_phd", "pct_part_time",
                           "pct_Women", "pct_Men"),
                         names(df))) {
      df[[cl]] <- round(as.numeric(df[[cl]]), 1)
    }

    names(df) <- ifelse(names(df) %in% names(pub_headers),
                        pub_headers[names(df)], names(df))

    openxlsx::addWorksheet(wb, nm, gridLines = FALSE)
    openxlsx::writeData(wb, nm, df, headerStyle = st_header, na.string = "",
                        withFilter = nrow(df) > 3)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(df)), widths = "auto")

    if (nrow(df) > 0) {
      rows <- 2:(nrow(df) + 1)
      for (k in seq_along(df)) {
        if (is.numeric(df[[k]])) {
          f <- fmt_for(names(df)[k])
          if (!is.null(f)) {
            openxlsx::addStyle(wb, nm, openxlsx::createStyle(numFmt = f, halign = "right"),
                               rows = rows, cols = k, gridExpand = TRUE, stack = TRUE)
          }
        } else {
          openxlsx::addStyle(wb, nm, st_text, rows = rows, cols = k,
                             gridExpand = TRUE, stack = TRUE)
        }
      }
    }
  }

  # Figures 13-14 as native Excel bar charts would lose the Q1-Q3 range, so the
  # ggplot PNGs are inserted instead; their data live in the gender_gap_* sheets.
  figs <- file.path(pub_dir, c("fig13_salary_gender_career_stage.png",
                               "fig14_salary_gender_management.png"))
  figs <- figs[file.exists(figs)]
  if (length(figs) > 0) {
    openxlsx::addWorksheet(wb, "Figures", gridLines = FALSE)
    row <- 1
    for (f in figs) {
      openxlsx::insertImage(wb, "Figures", f, width = 8, height = 4.4,
                            startRow = row, startCol = 1, units = "in")
      row <- row + 25
    }
  }

  openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  # The CSV mirrors from a previous run would be mistaken for the deliverable.
  old_csv <- list.files(pub_dir, pattern = "^tab_.*\\.csv$", full.names = TRUE)
  if (length(old_csv) > 0) unlink(old_csv)
  message("  → ", xlsx_path, " (", length(sheet_order), " feuilles, données typées)")
} else {
  message("! openxlsx indisponible (installation échouée) — écriture en CSV. ",
          "Installez-le manuellement : install.packages(\"openxlsx\")")
  for (nm in sheet_order) {
    readr::write_csv(pub_sheets[[nm]], file.path(pub_dir, paste0("tab_", nm, ".csv")), na = "")
  }
}

message("✔ 09_publication_extras.R terminé — fichiers dans ", pub_dir, "/")
