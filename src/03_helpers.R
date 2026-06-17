# ══════════════════════════════════════════════════════════════════
#  03_helpers.R — Fonctions graphiques réutilisables
#  Dépend de : 01_config.R (my_fill, my_border, out_dir, table_count)
# ══════════════════════════════════════════════════════════════════

# ── Bar chart simple (counts) ──────────────────────────────────────
save_barplot_counts <- function(df, xvar, title, filename,
                                xlab = NULL, rotate_x = TRUE) {
  p <- ggplot(df, aes(x = .data[[xvar]])) +
    geom_bar(fill = my_fill, color = my_border) +
    labs(
      title = paste0("Table-", table_count, " ", title),
      x = xlab,
      y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )

  if (rotate_x)
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
  table_count <<- table_count + 1
}

# ── Histogram (counts) ─────────────────────────────────────────────
save_hist_counts <- function(df, numvar, title, filename,
                             binwidth = NULL, bins = 30, xlab = NULL) {
  tmp <- df %>% filter(!is.na(.data[[numvar]]))
  if (nrow(tmp) == 0) return(invisible(NULL))

  p <- ggplot(tmp, aes(x = .data[[numvar]])) +
    { if (!is.null(binwidth))
        geom_histogram(binwidth = binwidth, fill = my_fill, color = my_border)
      else
        geom_histogram(bins = bins, fill = my_fill, color = my_border) } +
    labs(
      title = paste0("Table-", table_count, " ", title),
      x = xlab, y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )

  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
  table_count <<- table_count + 1
}

# ── Bar chart avec bins numériques personnalisés ───────────────────
save_binned_counts <- function(df, numvar, breaks, title, filename,
                               xlab = NULL,
                               label_mode = c("default", "year", "k")) {
  label_mode <- match.arg(label_mode)

  if (label_mode == "year") {
    bin_labels <- paste0(breaks[-length(breaks)], "\u2013", breaks[-1] - 1)
  } else if (label_mode == "k") {
    lo <- breaks[-length(breaks)] / 1000
    hi <- (breaks[-1] - 1) / 1000
    bin_labels <- paste0(lo, "k\u2013", floor(hi), "k")
  } else {
    bin_labels <- format(breaks[-length(breaks)], scientific = FALSE, trim = TRUE)
  }

  tmp <- df %>%
    filter(!is.na(.data[[numvar]])) %>%
    mutate(bin = cut(.data[[numvar]], breaks = breaks,
                     right = FALSE, include.lowest = TRUE,
                     labels = bin_labels))

  p <- ggplot(tmp, aes(x = bin)) +
    geom_bar(fill = my_fill, color = my_border) +
    labs(
      title = paste0("Table-", table_count, " ", title),
      x = xlab, y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
  table_count <<- table_count + 1
}

# ── Top N + Other (horizontal) ─────────────────────────────────────
save_topn_barplot <- function(df, xvar, n_top, title, filename,
                              xlab = NULL) {
  tmp <- df %>%
    filter(!is.na(.data[[xvar]]), .data[[xvar]] != "") %>%
    count(.data[[xvar]], name = "n") %>%
    arrange(desc(n)) %>%
    mutate(
      rank  = row_number(),
      group = ifelse(rank <= n_top, as.character(.data[[xvar]]), "Other")
    ) %>%
    count(group, wt = n, name = "n") %>%
    mutate(group = forcats::fct_reorder(group, n))

  p <- ggplot(tmp, aes(x = group, y = n)) +
    geom_col(fill = my_fill, color = my_border) +
    coord_flip() +
    labs(
      title = paste0("Table-", table_count, " ", title),
      x = xlab, y = "N respondents"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 7, dpi = 300)
  table_count <<- table_count + 1
}

# ── Donut avec légende ─────────────────────────────────────────────
save_donut_counts <- function(df, xvar, title, filename,
                              palette = NULL, wrap_width = NULL,
                              drop_na = TRUE, legend_show_pct = TRUE) {
  dd <- df %>%
    mutate(cat = as.character(.data[[xvar]])) %>%
    { if (drop_na) dplyr::filter(., !is.na(cat), cat != "") else . } %>%
    count(cat, name = "n") %>%
    mutate(pct = n / sum(n))

  if (nrow(dd) == 0) return(invisible(NULL))

  cat_disp <- dd$cat
  if (!is.null(wrap_width))
    cat_disp <- stringr::str_wrap(cat_disp, width = wrap_width)

  dd$cat_lab <- if (legend_show_pct) {
    paste0(cat_disp, " — ", dd$n, " (", scales::percent(dd$pct, accuracy = 1), ")")
  } else {
    cat_disp
  }

  breaks_cat <- dd$cat
  labels_map <- setNames(dd$cat_lab, dd$cat)

  p <- ggplot(dd, aes(x = 2, y = n, fill = cat)) +
    geom_col(width = 0.9, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.6) +
    labs(title = paste0("Table-", table_count, " ", title), fill = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.text     = element_text(size = 10),
      legend.key      = element_rect(color = NA),
      legend.key.height = unit(5, "mm"),
      legend.key.width  = unit(5, "mm"),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    guides(fill = guide_legend(
      keyheight = unit(5, "mm"),
      keywidth  = unit(5, "mm"),
      override.aes = list(alpha = 1)
    ))

  if (!is.null(palette)) {
    p <- p + scale_fill_manual(
      values = palette,
      breaks = breaks_cat,
      labels = labels_map[breaks_cat],
      drop = FALSE
    )
  } else {
    p <- p + scale_fill_discrete(
      breaks = breaks_cat,
      labels = labels_map[breaks_cat],
      drop = FALSE
    )
  }

  print(p)
  ggsave(file.path(out_dir, filename), p, width = 12, height = 6, dpi = 300)
  table_count <<- table_count + 1
}
