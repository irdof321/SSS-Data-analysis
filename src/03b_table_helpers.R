# ══════════════════════════════════════════════════════════════════
#  03b_table_helpers.R — Fonctions de tableaux stylés (package gt)
#  Dépend de : 01_config.R (tab_dir, tab_accent, tab_accent_dk,
#                            tab_stripe, tab_border, tab_font, table_count)
# ══════════════════════════════════════════════════════════════════
#
#  Toutes les fonctions ci-dessous produisent un .png "haute couture" :
#  - bandeau de titre coloré (tab_accent_dk) avec sous-titre clair
#  - en-têtes de colonnes en majuscules, fond accent, texte blanc
#  - lignes zébrées très subtiles pour guider l'œil
#  - colonnes numériques alignées à droite, barres de proportion
#    intégrées (data bars) quand pertinent
#  - note de bas de tableau avec le N total
#
#  Tous les tableaux sont sauvegardés dans `tab_dir` (séparé de out_dir).
# ══════════════════════════════════════════════════════════════════

# ── Thème commun appliqué à la fin de chaque tableau ───────────────
apply_gt_theme <- function(gt_tbl, title, subtitle = NULL) {
  n_rows <- nrow(gt_tbl[["_data"]])
  stripe_rows <- if (n_rows >= 2) seq(2, n_rows, by = 2) else integer(0)
  
  gt_tbl %>%
    tab_header(title = md(paste0("**", title, "**")), subtitle = subtitle) %>%
    tab_style(
      style = list(
        cell_fill(color = tab_accent_dk),
        cell_text(color = "white", weight = "bold", size = px(18))
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = tab_accent_dk),
        cell_text(color = "#D9E8F5", size = px(12))
      ),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = tab_accent),
        cell_text(color = "white", weight = "bold", size = px(12),
                  transform = "uppercase")
      ),
      locations = cells_column_labels()
    ) %>%
    { if (length(stripe_rows) > 0)
      tab_style(.,
                style = cell_fill(color = tab_stripe),
                locations = cells_body(rows = stripe_rows)
      )
      else . } %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = 1)
    ) %>%
    opt_table_font(font = list(tab_font, gt::default_fonts())) %>%
    opt_row_striping(row_striping = FALSE) %>%   # on gère le stripe nous-mêmes
    tab_options(
      table.border.top.style    = "none",
      table.border.bottom.style = "none",
      column_labels.border.bottom.color = tab_accent_dk,
      column_labels.border.bottom.width = px(2),
      table_body.border.bottom.color    = tab_border,
      table_body.hlines.color           = tab_border,
      data_row.padding   = px(8),
      column_labels.padding = px(10),
      heading.padding    = px(14),
      table.font.size    = px(13),
      source_notes.font.size = px(10.5),
      source_notes.padding   = px(8)
    )
}

# ── Sauvegarde standard PNG dans tab_dir ───────────────────────────
save_gt_png <- function(gt_tbl, filename, vwidth = 1100, vheight = NULL) {
  gt::gtsave(
    gt_tbl,
    filename = file.path(tab_dir, filename),
    vwidth = vwidth,
    vheight = if (is.null(vheight)) 10 else vheight,  # auto height si NULL
    expand  = 10
  )
  table_count <<- table_count + 1
}

# ── Tableau de fréquences simple (1 variable catégorielle) ─────────
save_freq_table <- function(df, xvar, title, filename,
                            subtitle = NULL, drop_na = TRUE,
                            col_label = NULL) {
  dd <- df %>% mutate(.cat = as.character(.data[[xvar]]))
  if (drop_na) dd <- dd %>% filter(!is.na(.cat), .cat != "")
  
  n_total <- nrow(dd)
  
  tbl <- dd %>%
    count(.cat, name = "n") %>%
    arrange(desc(n)) %>%
    mutate(pct = n / sum(n)) %>%
    rename(!!(col_label %||% xvar) := .cat)
  
  gt_tbl <- tbl %>%
    gt() %>%
    cols_label(n = "N", pct = "Share") %>%
    fmt_percent(columns = pct, decimals = 1) %>%
    fmt_number(columns = n, decimals = 0, sep_mark = "'") %>%
    cols_align(align = "right", columns = c(n, pct)) %>%
    data_color(
      columns = pct,
      colors = scales::col_numeric(
        palette = c("white", tab_accent), domain = c(0, max(tbl$pct))
      )
    ) %>%
    apply_gt_theme(title = paste0("Table-", table_count, " — ", title),
                   subtitle = subtitle) %>%
    tab_source_note(source_note = md(paste0("*N = ", n_total, " respondents*")))
  
  save_gt_png(gt_tbl, filename)
}

# ── Tableau croisé 2 variables (counts + % en ligne) ────────────────
save_crosstab <- function(df, rowvar, colvar, title, filename,
                          subtitle = NULL, row_label = NULL,
                          show_pct = TRUE) {
  dd <- df %>%
    filter(!is.na(.data[[rowvar]]), !is.na(.data[[colvar]]))
  
  n_total <- nrow(dd)
  
  wide_n <- dd %>%
    count(.data[[rowvar]], .data[[colvar]], name = "n") %>%
    tidyr::pivot_wider(names_from = .data[[colvar]], values_from = n,
                       values_fill = 0)
  
  cat_col <- names(wide_n)[1]
  val_cols <- setdiff(names(wide_n), cat_col)
  
  if (show_pct) {
    wide_n <- wide_n %>%
      rowwise() %>%
      mutate(.rowtotal = sum(c_across(all_of(val_cols)))) %>%
      ungroup()
    
    wide_pct <- wide_n %>%
      mutate(across(all_of(val_cols), ~ . / .rowtotal)) %>%
      select(-.rowtotal)
  }
  
  display_tbl <- wide_n %>% select(-any_of(".rowtotal"))
  
  gt_tbl <- display_tbl %>%
    gt() %>%
    cols_label(.list = setNames(as.list(val_cols), val_cols)) %>%
    cols_label(!!cat_col := (row_label %||% rowvar)) %>%
    fmt_number(columns = all_of(val_cols), decimals = 0, sep_mark = "'") %>%
    cols_align(align = "right", columns = all_of(val_cols)) %>%
    apply_gt_theme(title = paste0("Table-", table_count, " — ", title),
                   subtitle = subtitle) %>%
    tab_source_note(source_note = md(paste0("*N = ", n_total, " respondents*")))
  
  if (show_pct) {
    for (vc in val_cols) {
      gt_tbl <- gt_tbl %>%
        data_color(
          columns = all_of(vc),
          colors = scales::col_numeric(
            palette = c("white", tab_accent),
            domain = c(0, max(wide_n[[vc]], na.rm = TRUE))
          )
        )
    }
  }
  
  gt_tbl <- gt_tbl %>%
    tab_spanner(label = (col_label_for_spanner <- colvar), columns = all_of(val_cols))
  
  save_gt_png(gt_tbl, filename, vwidth = 200 + 150 * length(val_cols))
}

# ── Tableau de statistiques descriptives (variable numérique) ──────
save_numeric_summary_table <- function(df, numvar, title, filename,
                                       subtitle = NULL, unit = "") {
  dd <- df %>% filter(!is.na(.data[[numvar]]))
  v  <- dd[[numvar]]
  
  tbl <- tibble::tibble(
    Statistic = c("N", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3"),
    Value = c(
      length(v),
      mean(v), median(v), sd(v), min(v), max(v),
      quantile(v, 0.25), quantile(v, 0.75)
    )
  )
  
  gt_tbl <- tbl %>%
    gt() %>%
    fmt_number(columns = Value, rows = Statistic != "N", decimals = 1) %>%
    fmt_number(columns = Value, rows = Statistic == "N", decimals = 0) %>%
    text_transform(
      locations = cells_body(columns = Value),
      fn = function(x) paste0(x, ifelse(unit == "", "", paste0(" ", unit)))
    ) %>%
    cols_align(align = "right", columns = Value) %>%
    apply_gt_theme(title = paste0("Table-", table_count, " — ", title),
                   subtitle = subtitle) %>%
    tab_source_note(source_note = md(paste0("*N = ", nrow(dd), " respondents*")))
  
  save_gt_png(gt_tbl, filename, vwidth = 500)
}

# ── Tableau "Top N + Other" (variable texte libre, ex: job_role) ───
save_topn_table <- function(df, xvar, n_top, title, filename,
                            subtitle = NULL, col_label = NULL) {
  dd <- df %>% filter(!is.na(.data[[xvar]]), .data[[xvar]] != "")
  n_total <- nrow(dd)
  
  tbl <- dd %>%
    count(.data[[xvar]], name = "n") %>%
    arrange(desc(n)) %>%
    mutate(rank = row_number()) %>%
    mutate(group = ifelse(rank <= n_top, as.character(.data[[xvar]]), "Other")) %>%
    count(group, wt = n, name = "n") %>%
    arrange(desc(n)) %>%
    mutate(pct = n / sum(n)) %>%
    rename(!!(col_label %||% xvar) := group)
  
  gt_tbl <- tbl %>%
    gt() %>%
    cols_label(n = "N", pct = "Share") %>%
    fmt_percent(columns = pct, decimals = 1) %>%
    fmt_number(columns = n, decimals = 0, sep_mark = "'") %>%
    cols_align(align = "right", columns = c(n, pct)) %>%
    data_color(
      columns = pct,
      colors = scales::col_numeric(
        palette = c("white", tab_accent), domain = c(0, max(tbl$pct))
      )
    ) %>%
    apply_gt_theme(title = paste0("Table-", table_count, " — ", title),
                   subtitle = subtitle) %>%
    tab_source_note(source_note = md(paste0("*N = ", n_total,
                                            " respondents — Top ", n_top, " shown*")))
  
  save_gt_png(gt_tbl, filename)
}

# ── Tableau pour variable multi-réponse (ex: training_fields_list) ─
save_multi_table <- function(df, listvar, title, filename,
                             subtitle = NULL, levels_order = NULL,
                             col_label = "Category") {
  n_total <- nrow(df)
  
  tbl <- df %>%
    tidyr::unnest_longer(!!sym(listvar), values_to = ".val") %>%
    mutate(.val = trimws(as.character(.val))) %>%
    filter(!is.na(.val), .val != "") %>%
    count(.val, name = "n") %>%
    arrange(desc(n)) %>%
    mutate(pct_of_respondents = n / n_total) %>%
    rename(!!col_label := .val)
  
  if (!is.null(levels_order)) {
    tbl <- tbl %>%
      mutate(.order = match(.data[[col_label]], levels_order)) %>%
      arrange(.order) %>%
      select(-.order)
  }
  
  gt_tbl <- tbl %>%
    gt() %>%
    cols_label(n = "N selections", pct_of_respondents = "Share of respondents") %>%
    fmt_percent(columns = pct_of_respondents, decimals = 1) %>%
    fmt_number(columns = n, decimals = 0, sep_mark = "'") %>%
    cols_align(align = "right", columns = c(n, pct_of_respondents)) %>%
    data_color(
      columns = pct_of_respondents,
      colors = scales::col_numeric(
        palette = c("white", tab_accent),
        domain = c(0, max(tbl$pct_of_respondents))
      )
    ) %>%
    apply_gt_theme(title = paste0("Table-", table_count, " — ", title),
                   subtitle = subtitle) %>%
    tab_source_note(source_note = md(paste0(
      "*N = ", n_total, " respondents — multiple answers allowed, ",
      "shares do not sum to 100%*"
    )))
  
  save_gt_png(gt_tbl, filename)
}

# `%||%` helper (au cas où rlang/purrr ne sont pas chargés)
`%||%` <- function(a, b) if (is.null(a)) b else a