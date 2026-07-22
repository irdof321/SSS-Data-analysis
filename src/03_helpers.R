# ══════════════════════════════════════════════════════════════════
#  03_helpers.R — Fonctions graphiques réutilisables (ggplot2)
#  Dépend de : 01_config.R (my_fill, my_border, out_dir)
#
#  Style "SSS" épuré : fond légèrement grisé, pas de bordures inutiles,
#  grille très fine, étiquettes (N + %) directement sur les barres,
#  hiérarchie typographique titre/sous-titre/source-note.
#
#  Toutes les fonctions écrivent dans `plots_dir`, réassignée par
#  generate_all_plots() pour chaque population (même logique que
#  `tables_dir` dans 06_tables.R).
# ══════════════════════════════════════════════════════════════════

plots_dir <- out_dir   # valeur par défaut, réassignée par generate_all_plots()

# ── Palette ──────────────────────────────────────────────────────
sss_blue      <- "#2C7FB8"
sss_blue_dark <- "#1a4a7a"
sss_grey_text <- "#5a6472"
sss_grey_line <- "#e7eaee"
sss_bg        <- "#FBFBFC"
sss_caption   <- "#9aa2ad"

# ── Thème commun ─────────────────────────────────────────────────
# Marge droite nettement plus généreuse (60pt) : c'est l'espace où
# vivent les labels de geom_text() en bout de barre horizontale.
theme_sss <- function(base_size = 12, horizontal = FALSE) {
  t <- theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = sss_bg, color = NA),
      panel.background  = element_rect(fill = sss_bg, color = NA),
      plot.title        = element_text(face = "bold", size = base_size + 6,
                                       color = "#20242c", margin = margin(b = 6),
                                       lineheight = 1.1),
      plot.subtitle     = element_text(color = sss_grey_text, size = base_size,
                                       face = "italic", margin = margin(b = 16),
                                       lineheight = 1.15),
      plot.caption      = element_text(color = sss_caption, size = base_size - 3.5,
                                       hjust = 0, margin = margin(t = 14)),
      axis.text         = element_text(color = sss_grey_text, size = base_size),
      axis.title        = element_blank(),
      axis.ticks        = element_blank(),
      panel.border      = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.spacing     = unit(1.4, "lines"),
      strip.text        = element_text(margin = margin(b = 6, t = 6)),
      plot.margin       = margin(18, 60, 12, 18)   # marge droite élargie : 22 -> 60
    )
  
  if (horizontal) {
    t <- t + theme(panel.grid.major.y = element_blank(),
                   panel.grid.major.x = element_line(color = sss_grey_line, linewidth = 0.4))
  } else {
    t <- t + theme(panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_line(color = sss_grey_line, linewidth = 0.4),
                   axis.line.x        = element_line(color = "#d8dce2", linewidth = 0.5),
                   plot.margin        = margin(18, 22, 12, 18))  # vertical bars : pas besoin de marge droite large
  }
  t
}

# ── Wrap automatique du titre/sous-titre — évite le débordement
# horizontal et le chevauchement avec le sous-titre/la légende ──────
# ── Wrap automatique du titre/sous-titre — évite le débordement
# horizontal et le chevauchement avec le sous-titre/la légende.
# `width_in` = largeur réelle du graphe en pouces (doit correspondre à
# ce qui est passé à save_plot) ; le nombre de caractères par ligne
# est dérivé de cette largeur, pour rester fiable même quand un graphe
# a une largeur non standard (heatmap, facettes, boxplot seul...).
sss_wrap_title <- function(x, width_in = 10) {
  if (is.null(x)) return(x)
  chars <- max(18, round(width_in * 5.2))
  stringr::str_wrap(x, width = chars)
}
sss_wrap_subtitle <- function(x, width_in = 10) {
  if (is.null(x)) return(x)
  chars <- max(24, round(width_in * 7.2))
  stringr::str_wrap(x, width = chars)
}

save_plot <- function(p, filename, width = 10, height = 6, dpi = 300) {
  print(p)
  ggsave(file.path(plots_dir, filename), p, width = width, height = height,
         dpi = dpi, bg = sss_bg)
}

sss_caption_note <- function(n, pop_label = "All respondents") {
  paste0("SSS Survey — ", pop_label, " — n = ", n)
}

# ══════════════════════════════════════════════════════════════════
#  BAR CHART — variable catégorielle (nominal ou ordonné)
#
#  order = "freq"  -> trié par fréquence décroissante
#  order = "level" -> garde l'ordre des levels du facteur (échelles, Likert)
#  horizontal      -> recommandé dès que > 4 catégories ou labels longs
#  top_n           -> regroupe le reste sous "Other"
# ══════════════════════════════════════════════════════════════════
save_bar_freq <- function(df, xvar, title, filename,
                          subtitle = NULL, order = c("freq", "level"),
                          horizontal = FALSE, top_n = NULL,
                          wrap_width = NULL, drop_na = TRUE,
                          pop_label = "All respondents") {
  order <- match.arg(order)
  
  dd <- df %>% mutate(.cat = as.character(.data[[xvar]]))
  if (drop_na) dd <- dd %>% filter(!is.na(.cat), .cat != "")
  if (nrow(dd) == 0) return(invisible(NULL))
  
  n_total <- nrow(dd)
  tab <- dd %>% count(.cat, name = "n")
  
  if (!is.null(top_n) && nrow(tab) > top_n) {
    tab <- tab %>%
      arrange(desc(n)) %>%
      mutate(rank = row_number(),
             .cat = ifelse(rank <= top_n, .cat, "Other")) %>%
      count(.cat, wt = n, name = "n")
  }
  
  tab <- tab %>% mutate(pct = n / sum(n))
  
  if (!is.null(wrap_width))
    tab <- tab %>% mutate(.cat = stringr::str_wrap(.cat, width = wrap_width))
  
  if (order == "freq") {
    tab <- tab %>% mutate(.cat = forcats::fct_reorder(.cat, n))
  } else {
    lvls <- levels(df[[xvar]])
    if (!is.null(lvls)) {
      if (!is.null(wrap_width)) lvls <- stringr::str_wrap(lvls, width = wrap_width)
      tab <- tab %>% mutate(.cat = factor(.cat, levels = lvls))
    }
  }
  
  is_top <- tab$n == max(tab$n)
  
  p <- ggplot(tab, aes(x = .cat, y = n)) +
    geom_col(aes(fill = is_top), width = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = c(`TRUE` = sss_blue_dark, `FALSE` = sss_blue)) +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(n_total, pop_label))
  
  if (horizontal) {
    p <- p +
      geom_text(aes(label = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")),
                hjust = -0.05, size = 3.4, color = sss_grey_text) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
      theme_sss(horizontal = TRUE)
  } else {
    p <- p +
      # N en gras, au-dessus ; % juste en dessous, en gris — bien séparés verticalement
      geom_text(aes(label = n), vjust = -1.7, size = 4.2, fontface = "bold", color = sss_blue_dark) +
      geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.4, size = 3.2, color = sss_grey_text) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
      theme_sss(horizontal = FALSE)
  }
  
  save_plot(p, filename, height = if (horizontal) max(4, 0.45 * nrow(tab) + 1.8) else 6)
}

# ══════════════════════════════════════════════════════════════════
#  HISTOGRAM — variable continue
# ══════════════════════════════════════════════════════════════════
save_hist <- function(df, numvar, title, filename,
                      subtitle = NULL, binwidth = NULL, bins = 30,
                      pop_label = "All respondents") {
  tmp <- df %>% filter(!is.na(.data[[numvar]]))
  if (nrow(tmp) == 0) return(invisible(NULL))
  m <- mean(tmp[[numvar]], na.rm = TRUE)
  
  p <- ggplot(tmp, aes(x = .data[[numvar]])) +
    { if (!is.null(binwidth))
      geom_histogram(binwidth = binwidth, fill = sss_blue, color = sss_bg, linewidth = 0.3)
      else
        geom_histogram(bins = bins, fill = sss_blue, color = sss_bg, linewidth = 0.3) } +
    geom_vline(xintercept = m, color = "#d62728", linetype = "dashed", linewidth = 0.7) +
    annotate("text", x = m, y = Inf, label = paste0("  mean = ", round(m, 1)),
             color = "#d62728", size = 3.2, hjust = 0, vjust = 1.6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(nrow(tmp), pop_label)) +
    theme_sss(horizontal = FALSE)
  
  save_plot(p, filename)
}

# ══════════════════════════════════════════════════════════════════
#  BOX PLOT — variable continue, seule ou croisée par groupe
# ══════════════════════════════════════════════════════════════════
save_boxplot <- function(df, numvar, title, filename,
                         groupvar = NULL, subtitle = NULL,
                         order_by_median = TRUE, drop_na = TRUE,
                         zoom = TRUE, zoom_factor = 1.8,
                         pop_label = "All respondents") {
  dd <- df %>% filter(!is.na(.data[[numvar]]))
  
  # ── Zoom : borne l'axe sur Q3 + zoom_factor*IQR (calculé sur
  # l'ensemble des données, pas groupe par groupe) pour que quelques
  # valeurs extrêmes n'écrasent pas la lecture des boîtes. Les points
  # au-delà restent dans les données (le boxplot les calcule toujours),
  # seule la fenêtre affichée est recadrée — une note l'indique.
  x_max <- max(dd[[numvar]], na.rm = TRUE)
  x_upper <- x_max
  n_hidden <- 0
  if (zoom) {
    q1  <- quantile(dd[[numvar]], 0.25, na.rm = TRUE)
    q3  <- quantile(dd[[numvar]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    x_upper <- as.numeric(q3 + zoom_factor * iqr)
    if (x_upper < x_max) {
      n_hidden <- sum(dd[[numvar]] > x_upper, na.rm = TRUE)
    } else {
      x_upper <- x_max  # pas d'outlier extrême : pas de recadrage nécessaire
    }
  }
  zoom_note <- if (n_hidden > 0)
    paste0(n_hidden, " extreme value(s) beyond this range not shown (axis zoomed for readability)")
  else NULL
  
  if (!is.null(groupvar)) {
    if (drop_na) dd <- dd %>% filter(!is.na(.data[[groupvar]]), .data[[groupvar]] != "")
    if (nrow(dd) == 0) return(invisible(NULL))
    
    if (order_by_median) {
      dd <- dd %>% mutate(.grp = forcats::fct_reorder(as.character(.data[[groupvar]]),
                                                      .data[[numvar]], median, .na_rm = TRUE))
    } else {
      dd <- dd %>% mutate(.grp = as.character(.data[[groupvar]]))
      lvls <- levels(df[[groupvar]])
      if (!is.null(lvls)) dd <- dd %>% mutate(.grp = factor(.grp, levels = lvls))
    }
    
    full_subtitle <- sss_wrap_subtitle(paste0(subtitle %||% "", if (!is.null(subtitle) && !is.null(zoom_note)) " — " else "", zoom_note %||% ""))
    
    p <- ggplot(dd, aes(x = .grp, y = .data[[numvar]])) +
      geom_boxplot(fill = sss_blue, alpha = 0.55, outlier.alpha = 0.35,
                   width = 0.55, color = sss_blue_dark, linewidth = 0.4) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#d62728") +
      coord_flip(ylim = c(NA, x_upper)) +
      labs(title = sss_wrap_title(title), subtitle = full_subtitle,
           caption = sss_caption_note(nrow(dd), pop_label)) +
      theme_sss(horizontal = TRUE)
    
    save_plot(p, filename, height = max(4, 0.5 * length(unique(dd$.grp)) + 1.8))
    
  } else {
    if (nrow(dd) == 0) return(invisible(NULL))
    full_subtitle <- sss_wrap_subtitle(paste0(subtitle %||% "", if (!is.null(subtitle) && !is.null(zoom_note)) " — " else "", zoom_note %||% ""), width_in = 4)
    p <- ggplot(dd, aes(x = "", y = .data[[numvar]])) +
      geom_boxplot(fill = sss_blue, alpha = 0.55, outlier.alpha = 0.35,
                   width = 0.35, color = sss_blue_dark, linewidth = 0.4) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#d62728") +
      coord_cartesian(ylim = c(NA, x_upper)) +
      labs(title = sss_wrap_title(title, width_in = 4), subtitle = full_subtitle,
           caption = sss_caption_note(nrow(dd), pop_label)) +
      theme_sss(horizontal = FALSE) +
      theme(axis.text.x = element_blank())
    save_plot(p, filename, width = 4, height = 6)
  }
}

# ══════════════════════════════════════════════════════════════════
#  MULTI-SELECT BAR — variable liste (jamais de donut : les % ne
#  somment pas à 100%)
# ══════════════════════════════════════════════════════════════════
save_bar_multi <- function(df, listvar, title, filename,
                           subtitle = NULL, levels_order = NULL,
                           wrap_width = NULL, pop_label = "All respondents") {
  n_total <- nrow(df)
  
  # Aplatit la colonne-liste avec du R de base (unlist), plutôt que
  # tidyr::unnest_longer() — contourne un conflit tidyr/dplyr observé
  # sur certaines données (message "Can't specify an argument named
  # `by`..."), indépendamment de la version des packages installée.
  vals <- unlist(df[[listvar]], use.names = FALSE)
  vals <- trimws(as.character(vals))
  vals <- vals[!is.na(vals) & vals != ""]
  
  if (length(vals) == 0) return(invisible(NULL))
  
  tab <- tibble::tibble(.val = vals) %>%
    count(.val, name = "n") %>%
    mutate(pct = n / n_total)
  
  if (nrow(tab) == 0) return(invisible(NULL))
  
  if (!is.null(wrap_width))
    tab <- tab %>% mutate(.val = stringr::str_wrap(.val, width = wrap_width))
  
  if (!is.null(levels_order)) {
    lvls <- if (!is.null(wrap_width)) stringr::str_wrap(levels_order, width = wrap_width) else levels_order
    tab <- tab %>% mutate(.val = factor(.val, levels = rev(lvls)))
  } else {
    tab <- tab %>% mutate(.val = forcats::fct_reorder(.val, n))
  }
  
  p <- ggplot(tab, aes(x = .val, y = n)) +
    geom_col(fill = sss_blue, width = 0.6) +
    geom_text(aes(label = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")),
              hjust = -0.05, size = 3.4, color = sss_grey_text) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    labs(title = sss_wrap_title(title),
         subtitle = sss_wrap_subtitle(paste0(subtitle %||% "", if (!is.null(subtitle)) " — " else "",
                                             "multiple selections allowed, shares do not sum to 100%")),
         caption = sss_caption_note(n_total, pop_label)) +
    theme_sss(horizontal = TRUE)
  
  save_plot(p, filename, height = max(4, 0.45 * nrow(tab) + 1.8))
}

# ══════════════════════════════════════════════════════════════════
#  HEATMAP — croisement de 2 variables catégorielles (counts)
#  Remplace le bar chart empilé dès que les 2 variables ont beaucoup
#  de catégories : plus lisible, et reprend l'esprit du data_color()
#  utilisé dans make_crosstab_table() côté tables.
# ══════════════════════════════════════════════════════════════════
save_heatmap_crosstab <- function(df, rowvar, colvar, title, filename,
                                  subtitle = NULL, wrap_width = NULL,
                                  col_wrap_width = NULL,
                                  pop_label = "All respondents") {
  dd <- df %>% filter(!is.na(.data[[rowvar]]), !is.na(.data[[colvar]]))
  if (nrow(dd) == 0) return(invisible(NULL))
  
  tab <- dd %>% count(.data[[rowvar]], .data[[colvar]], name = "N")
  names(tab)[1:2] <- c(".row", ".col")
  
  if (!is.null(wrap_width)) {
    tab <- tab %>% mutate(.row = stringr::str_wrap(as.character(.row), width = wrap_width))
  }
  if (!is.null(col_wrap_width)) {
    tab <- tab %>% mutate(.col = stringr::str_wrap(as.character(.col), width = col_wrap_width))
  }
  
  # tri des lignes par total décroissant (les + gros effectifs en haut)
  row_order <- tab %>% summarise(tot = sum(N), .by = .row) %>% arrange(tot) %>% pull(.row)
  tab <- tab %>% mutate(.row = factor(.row, levels = row_order))
  
  n_cols <- length(unique(tab$.col))
  # largeur dynamique : évite les colonnes écrasées quand il y a beaucoup
  # de catégories (ex: sectors, job titles) — calculée AVANT le titre pour
  # que le wrap du texte corresponde à la largeur réelle du graphe.
  plot_width <- max(8.5, 1.5 * n_cols + 3)
  
  p <- ggplot(tab, aes(x = .col, y = .row, fill = N)) +
    geom_tile(color = sss_bg, linewidth = 1.5) +
    geom_text(aes(label = N,
                  color = N > max(tab$N) * 0.55),
              size = 3.3, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = sss_blue_dark)) +
    scale_fill_gradient(low = "#eaf2fa", high = sss_blue_dark, guide = "none") +
    labs(title = sss_wrap_title(title, width_in = plot_width),
         subtitle = sss_wrap_subtitle(subtitle, width_in = plot_width),
         caption = sss_caption_note(nrow(dd), pop_label)) +
    theme_sss(horizontal = TRUE) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
          plot.margin = margin(18, 60, 30, 18))  # marge basse élargie pour les labels x en biais
  
  save_plot(p, filename, width = plot_width, height = max(4, 0.5 * length(row_order) + 1.8))
}

# ══════════════════════════════════════════════════════════════════
#  RATE BAR — % "vrai" par groupe (ex: taux de managers par diplôme)
# ══════════════════════════════════════════════════════════════════
save_rate_bar <- function(df, groupvar, boolvar, title, filename,
                          subtitle = NULL, group_order = NULL,
                          sort_by = c("rate", "order", "freq"),
                          min_n = 5, pop_label = "All respondents") {
  sort_by <- match.arg(sort_by)
  
  tab <- df %>%
    filter(!is.na(.data[[groupvar]])) %>%
    summarise(N = n(), rate = mean(.data[[boolvar]], na.rm = TRUE), .by = all_of(groupvar)) %>%
    filter(N >= min_n)
  names(tab)[1] <- ".grp"
  if (nrow(tab) == 0) return(invisible(NULL))
  
  if (!is.null(group_order)) {
    tab <- tab %>% mutate(.grp = factor(.grp, levels = group_order))
  } else if (sort_by == "rate") {
    tab <- tab %>% mutate(.grp = forcats::fct_reorder(.grp, rate))
  } else if (sort_by == "freq") {
    tab <- tab %>% mutate(.grp = forcats::fct_reorder(.grp, N))
  }
  
  p <- ggplot(tab, aes(x = .grp, y = rate)) +
    geom_col(fill = sss_blue, width = 0.6) +
    geom_text(aes(label = paste0(scales::percent(rate, accuracy = 0.1), "  (n=", N, ")")),
              hjust = -0.05, size = 3.3, color = sss_grey_text) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(tab$rate) * 1.45),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(sum(tab$N), pop_label)) +
    theme_sss(horizontal = TRUE)
  
  save_plot(p, filename, height = max(4, 0.45 * nrow(tab) + 1.8))
}

# ══════════════════════════════════════════════════════════════════
#  DIVERGING BAR — satisfaction par groupe (% satisfait vs % insatisfait,
#  de part et d'autre de zéro)
# ══════════════════════════════════════════════════════════════════
save_diverging_satisfaction <- function(df, groupvar, title, filename,
                                        subtitle = NULL, group_order = NULL,
                                        min_n = 5, pop_label = "All respondents") {
  tab <- df %>%
    filter(!is.na(.data[[groupvar]]), !is.na(worksatisfction)) %>%
    summarise(
      N              = n(),
      pct_satisfied  = mean(worksatisfction %in% c("Very satisfied", "Quite satisfied"), na.rm = TRUE),
      pct_dissatisfied = -mean(worksatisfction %in% c("Not quite satisfied", "Not at all satisfied"), na.rm = TRUE),
      .by = all_of(groupvar)
    ) %>%
    filter(N >= min_n)
  names(tab)[1] <- ".grp"
  if (nrow(tab) == 0) return(invisible(NULL))
  
  if (!is.null(group_order)) {
    tab <- tab %>% mutate(.grp = factor(.grp, levels = group_order))
  } else {
    tab <- tab %>% mutate(.grp = forcats::fct_reorder(.grp, pct_satisfied))
  }
  
  tab_long <- tab %>%
    tidyr::pivot_longer(cols = c(pct_satisfied, pct_dissatisfied),
                        names_to = "type", values_to = "val")
  
  p <- ggplot(tab_long, aes(x = .grp, y = val, fill = type)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, color = "#d8dce2", linewidth = 0.5) +
    scale_fill_manual(values = c(pct_satisfied = "#1a7a4a", pct_dissatisfied = "#c0392b"),
                      labels = c(pct_satisfied = "% Satisfied", pct_dissatisfied = "% Dissatisfied"),
                      name = NULL) +
    scale_y_continuous(labels = function(x) scales::percent(abs(x))) +
    coord_flip() +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(sum(tab$N), pop_label)) +
    theme_sss(horizontal = TRUE) +
    theme(legend.position = "top", legend.justification = "left")
  
  save_plot(p, filename, height = max(4, 0.45 * nrow(tab) + 1.8))
}

# ══════════════════════════════════════════════════════════════════
#  FACETED TOP-N — top-N éléments par groupe (petits bar charts côte à
#  côte, ex: job titles les plus fréquents par profil)
# ══════════════════════════════════════════════════════════════════
save_facet_topn <- function(df, groupvar, itemvar, n_top, title, filename,
                            subtitle = NULL, group_order = NULL,
                            wrap_width = 28, pop_label = "All respondents") {
  dd <- df %>% filter(!is.na(.data[[groupvar]]), !is.na(.data[[itemvar]]))
  if (nrow(dd) == 0) return(invisible(NULL))
  
  tab <- dd %>%
    count(.data[[groupvar]], .data[[itemvar]], name = "N")
  names(tab)[1:2] <- c(".grp", ".item")
  
  tab <- tab %>%
    group_by(.grp) %>%
    slice_max(N, n = n_top, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(.item = stringr::str_wrap(as.character(.item), width = wrap_width))
  
  if (!is.null(group_order)) tab <- tab %>% mutate(.grp = factor(.grp, levels = group_order))
  
  # ordre local par groupe (sans dépendance à tidytext)
  tab <- tab %>%
    mutate(.item_ord = paste(.grp, .item, sep = "___")) %>%
    mutate(.item_ord = forcats::fct_reorder(.item_ord, N))
  
  p <- ggplot(tab, aes(x = .item_ord, y = N)) +
    geom_col(fill = sss_blue, width = 0.65) +
    geom_text(aes(label = N), hjust = -0.3, size = 3, color = sss_grey_text) +
    coord_flip(clip = "off") +
    scale_x_discrete(labels = function(x) sub("___.*$", "", gsub("^[^_]*___", "", x))) +
    facet_wrap(vars(.grp), scales = "free_y", ncol = 2,
               labeller = ggplot2::label_wrap_gen(width = 20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(nrow(dd), pop_label)) +
    theme_sss(horizontal = TRUE) +
    theme(strip.background = element_rect(fill = sss_blue_dark, color = NA),
          strip.text = element_text(color = "white", face = "bold", size = 9))
  
  n_facets <- length(unique(tab$.grp))
  save_plot(p, filename, width = 10, height = max(5, ceiling(n_facets/2) * 3))
}

# ══════════════════════════════════════════════════════════════════
#  STACKED 100% BAR — composition d'une variable catégorielle par
#  groupe (ex: continuous education par secteur) — plus parlant qu'un
#  heatmap pour montrer une répartition qui somme à 100% par groupe
# ══════════════════════════════════════════════════════════════════
save_stacked_100_bar <- function(df, groupvar, catvar, title, filename,
                                 subtitle = NULL, group_order = NULL,
                                 cat_order = NULL, min_n = 5,
                                 pop_label = "All respondents") {
  dd <- df %>% filter(!is.na(.data[[groupvar]]), !is.na(.data[[catvar]]))
  if (nrow(dd) == 0) return(invisible(NULL))
  
  tab <- dd %>% count(.data[[groupvar]], .data[[catvar]], name = "n")
  names(tab)[1:2] <- c(".grp", ".cat")
  
  grp_n <- tab %>% summarise(N = sum(n), .by = .grp)
  tab <- tab %>% left_join(grp_n, by = ".grp") %>% filter(N >= min_n) %>%
    mutate(pct = n / N)
  
  if (nrow(tab) == 0) return(invisible(NULL))
  
  if (!is.null(group_order)) tab <- tab %>% mutate(.grp = factor(.grp, levels = group_order))
  if (!is.null(cat_order))   tab <- tab %>% mutate(.cat = factor(.cat, levels = cat_order))
  
  # Légende : labels courts (wrap) + toujours plusieurs lignes plutôt
  # qu'une seule ligne qui déborde hors du graphe.
  cat_labels <- setNames(
    stringr::str_wrap(levels(factor(tab$.cat)), width = 22),
    levels(factor(tab$.cat))
  )
  n_cat <- length(cat_labels)
  
  p <- ggplot(tab, aes(x = .grp, y = pct, fill = .cat)) +
    geom_col(width = 0.65, color = sss_bg, linewidth = 0.4) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_brewer(palette = "Blues", direction = -1, name = NULL, labels = cat_labels) +
    labs(title = sss_wrap_title(title), subtitle = sss_wrap_subtitle(subtitle),
         caption = sss_caption_note(sum(grp_n$N), pop_label)) +
    theme_sss(horizontal = TRUE) +
    theme(legend.position = "bottom", legend.text = element_text(size = 8.5),
          legend.key.size = unit(3.5, "mm")) +
    guides(fill = guide_legend(nrow = ceiling(n_cat / 3), byrow = TRUE))
  
  save_plot(p, filename, height = max(4.5, 0.5 * length(unique(tab$.grp)) + 0.35 * ceiling(n_cat / 3) + 2.2))
}

`%||%` <- function(a, b) if (is.null(a)) b else a