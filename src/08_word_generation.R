# ══════════════════════════════════════════════════════════════════
#  08_word_generation.R — Automatic Word report generation
# ══════════════════════════════════════════════════════════════════
# Requires:
#   install.packages(c("officer", "flextable"))
#
# This script is sourced LAST, after tables and plots have been generated.
# It builds a .docx containing:
#   - title page and table of contents
#   - all result sections
#   - editable Word tables from csv_files/
#   - PNG figures from descriptives_plots/
#   - an explicit salary-methods section documenting the band -> FTE conversion
#
# If report_reference.docx exists, it is used as the Word reference document.
# Otherwise a blank Word document is created.
# ══════════════════════════════════════════════════════════════════

if (!exists("GENERATE_WORD_REPORT")) GENERATE_WORD_REPORT <- TRUE
if (!GENERATE_WORD_REPORT) {
  message("↷ Word report generation disabled (GENERATE_WORD_REPORT = FALSE)")
} else {

  required_pkgs <- c("officer", "flextable")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                         FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "Word report generation requires: ",
      paste(missing_pkgs, collapse = ", "),
      ". Install once with install.packages(c(\"officer\", \"flextable\"))."
    )
  }

  library(officer)
  library(flextable)

  # ── Word style helpers ──────────────────────────────────────────
  # Reference documents do not necessarily contain Word's built-in
  # "Title", "Subtitle", "Caption" or "Intense Quote" styles.
  # The report therefore relies only on styles that are actually present
  # and falls back to Normal when needed.
  style_names <- function(doc) unique(officer::styles_info(doc)$style_name)

  pick_style <- function(doc, candidates, fallback = "Normal") {
    available <- style_names(doc)
    hit <- candidates[candidates %in% available]
    if (length(hit) > 0) return(hit[[1]])
    if (fallback %in% available) return(fallback)
    available[[1]]
  }

  add_report_title <- function(doc, text) {
    p <- fpar(
      ftext(text, prop = fp_text(font.family = "Arial", font.size = 24, bold = TRUE)),
      fp_p = fp_par(text.align = "center")
    )
    body_add_fpar(doc, p, style = pick_style(doc, c("Title", "graphic title", "heading 1")))
  }

  add_report_subtitle <- function(doc, text) {
    p <- fpar(
      ftext(text, prop = fp_text(font.family = "Arial", font.size = 13, italic = TRUE, color = "#5B6573")),
      fp_p = fp_par(text.align = "center")
    )
    body_add_fpar(doc, p, style = pick_style(doc, c("Subtitle", "centered", "Normal")))
  }

  add_formula_par <- function(doc, text) {
    p <- fpar(
      ftext(text, prop = fp_text(font.family = "Arial", font.size = 10, italic = TRUE)),
      fp_p = fp_par(text.align = "center")
    )
    body_add_fpar(doc, p, style = pick_style(doc, c("Intense Quote", "centered", "Normal")))
  }

  # ── Defaults (can be overridden in 01_config.R) ─────────────────
  if (!exists("WORD_REPORT_FILE"))
    WORD_REPORT_FILE <- "SSS_Survey_Report.docx"
  if (!exists("WORD_REPORT_TEMPLATE"))
    WORD_REPORT_TEMPLATE <- "report_reference.docx"
  if (!exists("WORD_REPORT_INCLUDE_SSS_MEMBERS"))
    WORD_REPORT_INCLUDE_SSS_MEMBERS <- TRUE
  if (!exists("WORD_REPORT_MAX_FIGURE_WIDTH"))
    WORD_REPORT_MAX_FIGURE_WIDTH <- 6.35
  if (!exists("WORD_REPORT_MAX_FIGURE_HEIGHT"))
    WORD_REPORT_MAX_FIGURE_HEIGHT <- 7.25
  if (!exists("WORD_REPORT_TABLE_FONT_SIZE"))
    WORD_REPORT_TABLE_FONT_SIZE <- 8.5
  if (!exists("WORD_REPORT_STATIC_TOC"))
    WORD_REPORT_STATIC_TOC <- TRUE
  if (!exists("WORD_REPORT_DYNAMIC_TOC"))
    WORD_REPORT_DYNAMIC_TOC <- TRUE
  if (!exists("WORD_REPORT_INCLUDE_APPENDIX_A"))
    WORD_REPORT_INCLUDE_APPENDIX_A <- TRUE

  # ── Utility helpers ──────────────────────────────────────────────
  humanize_name <- function(x) {
    x <- sub("\\.[Pp][Nn][Gg]$", "", x)
    x <- sub("\\.[Cc][Ss][Vv]$", "", x)
    x <- gsub("[_-]+", " ", x)
    x <- trimws(x)
    tools::toTitleCase(x)
  }

  pretty_titles <- c(
    "freq_gender" = "Gender",
    "desc_age" = "Age",
    "freq_origin" = "Origin",
    "freq_residency" = "Place of residence",
    "freq_work_location" = "Place of work (canton)",
    "freq_sss_awareness" = "Awareness of the Swiss Statistical Society",
    "freq_sss_membership" = "SSS membership",
    "freq_sss_involvement" = "SSS involvement",
    "freq_sss_duration" = "Duration of SSS membership",
    "freq_education_level" = "Highest degree obtained",
    "multi_training_fields" = "Fields of study",
    "desc_graduation_year" = "Graduation year",
    "freq_study_location" = "Place of study",
    "freq_continuous_education" = "Continuous education",
    "freq_employment_status" = "Employment status",
    "freq_employed" = "Current employment",
    "freq_sector" = "Sector of employment",
    "freq_job_role" = "Work position (job title)",
    "desc_experience" = "Years of professional experience",
    "freq_seniority" = "Seniority level",
    "freq_career_stage" = "Career stage",
    "ustime_summary" = "Use of statistics in professional activity",
    "freq_salary_band" = "Reported gross annual salary bands",
    "desc_salary_raw" = "Reported salary midpoint summary",
    "desc_salary_fte" = "Approximate full-time equivalent salary",
    "freq_work_satisfaction" = "Overall job satisfaction",
    "satisfaction_detail" = "Detailed job satisfaction",
    "salary_by_sector" = "Salary levels, by sector of employment",
    "salary_by_jobrole" = "Salary levels, by work position (job title)",
    "salary_by_degree" = "Salary levels, by highest degree obtained",
    "salary_by_experience" = "Salary levels, by years of professional experience",
    "salary_by_seniority" = "Salary levels, by seniority level",
    "salary_by_age_group" = "Salary levels, by age group",
    "salary_by_work_region" = "Salary levels, by main work canton",
    "salary_by_gender" = "Salary levels, by gender",
    "satisfaction_by_salary" = "Job satisfaction, by salary level",
    "labour_activities_by_sector" = "Mean perceived importance of statistical activities, by sector",
    "labour_manager_rate_by_sector" = "Managerial responsibility, by sector",
    "labour_experience_by_sector" = "Years of professional experience, by sector",
    "hidden_skills_by_rolegroup" = "Work-related skills, by role group",
    "hidden_activities_by_rolegroup" = "Statistical activities, by role group"
  )

  report_title_for <- function(stem) {
    if (stem %in% names(pretty_titles)) unname(pretty_titles[[stem]]) else humanize_name(stem)
  }

  png_dimensions <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    raw <- readBin(con, what = "raw", n = 24)
    if (length(raw) < 24) return(c(width = 1600, height = 1000))
    b2i <- function(z) sum(as.integer(z) * 256^(3:0))
    c(width = b2i(raw[17:20]), height = b2i(raw[21:24]))
  }

  fitted_png_size <- function(path,
                              max_width = WORD_REPORT_MAX_FIGURE_WIDTH,
                              max_height = WORD_REPORT_MAX_FIGURE_HEIGHT) {
    d <- png_dimensions(path)
    ratio <- d[["width"]] / d[["height"]]
    width <- min(max_width, max_height * ratio)
    height <- width / ratio
    c(width = width, height = height)
  }

  add_caption <- function(doc, type = c("figure", "table"), title) {
    type <- match.arg(type)
    seq_id <- if (type == "figure") "fig" else "tab"
    label <- if (type == "figure") "Figure " else "Table "
    caption_style <- if (type == "figure") {
      pick_style(doc, c("Image Caption", "Caption", "graphic title", "Normal"))
    } else {
      pick_style(doc, c("Table Caption", "Caption", "table title", "Normal"))
    }
    p <- fpar(
      run_autonum(seq_id = seq_id, pre_label = label, post_label = ". "),
      ftext(title, prop = fp_text(font.size = 9, bold = FALSE))
    )
    body_add_fpar(doc, p, style = caption_style)
  }

  make_word_table <- function(csv_path) {
    dat <- readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)

    # Convert proportions into human-readable percentages when the column name
    # clearly indicates a percentage. The source CSV stores the underlying values.
    pct_cols <- names(dat)[grepl("(^%$|%|share|proportion|rate)", names(dat), ignore.case = TRUE)]
    for (nm in pct_cols) {
      if (is.numeric(dat[[nm]])) {
        finite_vals <- dat[[nm]][is.finite(dat[[nm]])]
        if (length(finite_vals) > 0 && max(abs(finite_vals), na.rm = TRUE) <= 1.000001) {
          dat[[nm]] <- ifelse(is.na(dat[[nm]]), NA_character_,
                              sprintf("%.1f%%", 100 * dat[[nm]]))
        }
      }
    }

    # Keep generic numeric formatting readable in Word.
    for (nm in names(dat)) {
      if (is.numeric(dat[[nm]])) {
        vals <- dat[[nm]]
        integer_like <- all(is.na(vals) | abs(vals - round(vals)) < 1e-9)
        if (integer_like) {
          dat[[nm]] <- ifelse(is.na(vals), NA_character_,
                              format(round(vals), big.mark = "'", scientific = FALSE, trim = TRUE))
        } else {
          dat[[nm]] <- ifelse(is.na(vals), NA_character_,
                              format(round(vals, 2), nsmall = 2, big.mark = "'",
                                     scientific = FALSE, trim = TRUE))
        }
      }
    }

    ft <- flextable(dat)
    ft <- theme_booktabs(ft)
    ft <- bold(ft, part = "header")
    ft <- bg(ft, part = "header", bg = "#1a4a7a")
    ft <- color(ft, part = "header", color = "white")
    ft <- font(ft, fontname = "Arial", part = "all")
    ft <- fontsize(ft, size = WORD_REPORT_TABLE_FONT_SIZE, part = "all")
    ft <- valign(ft, valign = "top", part = "all")
    ft <- autofit(ft)
    ft <- set_table_properties(ft, layout = "autofit", width = 1)
    ft
  }

  add_csv_table <- function(doc, csv_path, title) {
    if (!file.exists(csv_path)) return(doc)
    dat <- tryCatch(
      readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
    if (is.null(dat) || nrow(dat) == 0 || ncol(dat) == 0) return(doc)

    doc <- add_caption(doc, "table", title)
    ft <- make_word_table(csv_path)
    doc <- body_add_flextable(doc, value = ft)
    doc <- body_add_par(doc, "", style = "Normal")
    doc
  }

  add_png_figure <- function(doc, png_path, title) {
    if (!file.exists(png_path)) return(doc)
    doc <- add_caption(doc, "figure", title)
    sz <- fitted_png_size(png_path)
    img <- external_img(
      src = png_path,
      width = unname(sz[["width"]]),
      height = unname(sz[["height"]])
    )
    doc <- body_add_fpar(
      doc,
      fpar(img, fp_p = fp_par(text.align = "center")),
      style = "Normal"
    )
    doc <- body_add_par(doc, "", style = "Normal")
    doc
  }

  # ── Visible TOC + Appendix helpers ──────────────────────────────
  # officer::body_add_toc() inserts a native Word TOC field. Its entries and
  # page numbers are computed by Word, so a newly generated DOCX can look as
  # if the TOC were empty until fields are refreshed. To avoid that ambiguity,
  # the report contains a visible R-generated contents list as well.
  add_visible_toc <- function(doc, sections, include_appendix = TRUE) {
    entries <- c("About this report", vapply(sections[ vapply(sections, function(x) isTRUE(x$include), logical(1)) ],
                                              function(x) x$title, character(1)))
    nums <- seq_along(entries)
    for (i in seq_along(entries)) {
      p <- fpar(
        ftext(paste0(nums[[i]], ".  "), prop = fp_text(font.family = "Arial", font.size = 10, bold = TRUE)),
        ftext(entries[[i]], prop = fp_text(font.family = "Arial", font.size = 10))
      )
      doc <- body_add_fpar(doc, p, style = "Normal")
    }
    if (isTRUE(include_appendix)) {
      p <- fpar(
        ftext("Appendix A  ", prop = fp_text(font.family = "Arial", font.size = 10, bold = TRUE)),
        ftext("Technical methodology and output notes", prop = fp_text(font.family = "Arial", font.size = 10))
      )
      doc <- body_add_fpar(doc, p, style = "Normal")
    }
    doc
  }

  add_appendix_a <- function(doc) {
    doc <- body_add_break(doc)
    doc <- body_add_par(
      doc,
      "Appendix A — Technical methodology and output notes",
      style = pick_style(doc, c("heading 1", "Heading 1", "Normal"))
    )

    doc <- body_add_par(
      doc,
      "A.1 Salary transformation to full-time equivalent",
      style = pick_style(doc, c("heading 2", "Heading 2", "Normal"))
    )
    doc <- body_add_par(
      doc,
      paste0(
        "Salary is collected in bands. Continuous salary analyses use the midpoint of the reported band as an approximate point estimate. ",
        "The midpoint and, where available, the lower and upper band limits are divided by the respondent's work rate and multiplied by 100 to obtain FTE-compatible values."
      ),
      style = "Normal"
    )
    doc <- add_formula_par(doc, "salary_fte_low = salary_band_low / work rate × 100")
    doc <- add_formula_par(doc, "salary_fte_mid = salary_midpoint / work rate × 100")
    doc <- add_formula_par(doc, "salary_fte_high = salary_band_high / work rate × 100")
    doc <- body_add_par(
      doc,
      paste0(
        "The open-ended categories retain an undefined outer bound. For midpoint-based point estimates only, the working values are CHF ",
        format(salary_band_midpoints[[1]], big.mark = "'", scientific = FALSE), " for the lowest band and CHF ",
        format(salary_band_midpoints[[length(salary_band_midpoints)]], big.mark = "'", scientific = FALSE), " for the highest band."
      ),
      style = "Normal"
    )

    salary_dictionary <- data.frame(
      Variable = c(
        "salary_band", "salary_band_low", "salary_midpoint", "salary_band_high",
        "salary_fte_low", "salary_fte_mid", "salary_fte_high"
      ),
      Meaning = c(
        "Original salary category reported by the respondent",
        "Known lower limit of the reported category (NA for the lowest open-ended band)",
        "Midpoint used as an approximate point estimate",
        "Known upper limit of the reported category (NA for the highest open-ended band)",
        "Lower FTE-compatible salary bound, when defined",
        "Approximate FTE salary point estimate used in continuous analyses",
        "Upper FTE-compatible salary bound, when defined"
      ),
      stringsAsFactors = FALSE
    )
    doc <- add_caption(doc, "table", "Salary variables used in the analysis")
    ft <- flextable(salary_dictionary)
    ft <- theme_booktabs(ft)
    ft <- bold(ft, part = "header")
    ft <- bg(ft, part = "header", bg = "#1a4a7a")
    ft <- color(ft, part = "header", color = "white")
    ft <- font(ft, fontname = "Arial", part = "all")
    ft <- fontsize(ft, size = 9, part = "all")
    ft <- width(ft, j = 1, width = 1.5)
    ft <- width(ft, j = 2, width = 5.0)
    ft <- set_table_properties(ft, layout = "fixed", width = 1)
    doc <- body_add_flextable(doc, ft)

    doc <- body_add_par(
      doc,
      "A.2 Minimum group-size rules",
      style = pick_style(doc, c("heading 2", "Heading 2", "Normal"))
    )
    general_n <- if (exists("GROUP_MIN_N")) GROUP_MIN_N else 5
    salary_n <- if (exists("SALARY_BOXPLOT_MIN_N")) SALARY_BOXPLOT_MIN_N else general_n
    job_n <- if (exists("SALARY_JOBROLE_MIN_N")) SALARY_JOBROLE_MIN_N else 3
    doc <- body_add_par(
      doc,
      paste0(
        "Grouped analyses suppress groups with too few usable observations. Current settings: general grouped analyses n ≥ ", general_n,
        "; salary boxplots n ≥ ", salary_n, "; salary-by-job-title boxplots n ≥ ", job_n,
        ". For a salary analysis, eligibility is based on usable observations for that salary analysis, not merely on the number of respondents in the corresponding descriptive category."
      ),
      style = "Normal"
    )

    doc <- body_add_par(
      doc,
      "A.3 Reproducible report generation",
      style = pick_style(doc, c("heading 2", "Heading 2", "Normal"))
    )
    doc <- body_add_par(
      doc,
      paste0(
        "The report is generated at the end of analysis.R by src/08_word_generation.R. Figures are read from ", out_dir,
        "/ and editable Word tables are reconstructed from the CSV mirrors in ", csv_dir,
        "/. Re-running the full pipeline refreshes these outputs and rebuilds the DOCX."
      ),
      style = "Normal"
    )
    doc
  }

  add_salary_methodology <- function(doc) {
    doc <- body_add_par(doc, "Salary methodology", style = pick_style(doc, c("heading 2", "Heading 2", "Normal")))

    doc <- body_add_par(
      doc,
      paste0(
        "The questionnaire collects gross annual salary in predefined bands rather than as an exact amount. ",
        "For descriptive displays of the observed responses, these original salary bands are retained. ",
        "For analyses that require a continuous salary variable, each band is represented by its midpoint."
      ),
      style = "Normal"
    )

    doc <- body_add_par(
      doc,
      paste0(
        "Because respondents may work at different employment rates, salary comparisons are standardized to a ",
        "full-time equivalent (FTE; 100% workload). If L and U are the lower and upper limits of the reported band, ",
        "M is its midpoint, and r is the reported work rate in percent, the derived quantities are:"
      ),
      style = "Normal"
    )

    doc <- add_formula_par(doc, "FTE lower bound = L / r × 100")
    doc <- add_formula_par(doc, "FTE midpoint estimate = M / r × 100")
    doc <- add_formula_par(doc, "FTE upper bound = U / r × 100")

    doc <- body_add_par(
      doc,
      paste0(
        "For example, a respondent reporting CHF 80'000–89'999 at a 50% work rate is compatible with an FTE salary ",
        "of approximately CHF 160'000–179'998, with a midpoint estimate of about CHF 170'000."
      ),
      style = "Normal"
    )

    low_mid <- format(salary_band_midpoints[[1]], big.mark = "'", scientific = FALSE)
    high_mid <- format(salary_band_midpoints[[length(salary_band_midpoints)]], big.mark = "'", scientific = FALSE)
    doc <- body_add_par(
      doc,
      paste0(
        "The lowest (< CHF 60'000) and highest (≥ CHF 200'000) categories are open-ended. Their unknown outer bound ",
        "remains undefined. For midpoint-based point estimates only, the analysis uses CHF ", low_mid,
        " and CHF ", high_mid, " respectively. These are explicit working assumptions and not observed exact salaries."
      ),
      style = "Normal"
    )

    doc <- body_add_par(
      doc,
      paste0(
        "Accordingly, salary_fte_mid is the point estimate used for means, medians, quartiles, boxplots and subgroup ",
        "comparisons. salary_fte_low and salary_fte_high retain the compatible FTE interval whenever both corresponding ",
        "band limits are known. Results based on salary_fte_mid should be interpreted as approximate rather than exact ",
        "salary estimates."
      ),
      style = "Normal"
    )

    salary_dictionary <- data.frame(
      Variable = c(
        "salary_band", "salary_band_low", "salary_midpoint", "salary_band_high",
        "salary_fte_low", "salary_fte_mid", "salary_fte_high"
      ),
      Meaning = c(
        "Original salary category reported by the respondent",
        "Known lower limit of the reported category (NA for the lowest open-ended band)",
        "Midpoint used as an approximate point estimate",
        "Known upper limit of the reported category (NA for the highest open-ended band)",
        "Lower FTE-compatible salary bound, when defined",
        "Approximate FTE salary point estimate used in continuous analyses",
        "Upper FTE-compatible salary bound, when defined"
      ),
      stringsAsFactors = FALSE
    )

    doc <- add_caption(doc, "table", "Derived salary variables used in the analysis")
    ft <- flextable(salary_dictionary)
    ft <- theme_booktabs(ft)
    ft <- bold(ft, part = "header")
    ft <- bg(ft, part = "header", bg = "#1a4a7a")
    ft <- color(ft, part = "header", color = "white")
    ft <- font(ft, fontname = "Arial", part = "all")
    ft <- fontsize(ft, size = 9, part = "all")
    ft <- width(ft, j = 1, width = 1.5)
    ft <- width(ft, j = 2, width = 5.0)
    ft <- set_table_properties(ft, layout = "fixed", width = 1)
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, "", style = "Normal")
    doc
  }

  section_definitions <- list(
    list(title = "Descriptive results — all respondents", folder = "full_population", include = TRUE),
    list(title = "Descriptive results — SSS members", folder = "sss_members",
         include = isTRUE(WORD_REPORT_INCLUDE_SSS_MEMBERS)),
    list(title = "Career pathways", folder = "career_pathways", include = TRUE),
    list(title = "Labour market", folder = "labour_market", include = TRUE),
    list(title = "Salary and employment conditions", folder = "salary_and_conditions", include = TRUE),
    list(title = "Future research questions", folder = "future_research", include = TRUE),
    list(title = "Hidden statistical roles", folder = "hidden_statistical_roles", include = TRUE)
  )

  # ── Create Word document ─────────────────────────────────────────
  if (file.exists(WORD_REPORT_TEMPLATE)) {
    doc <- read_docx(path = WORD_REPORT_TEMPLATE)
  } else {
    doc <- read_docx()
  }

  # Title page
  doc <- add_report_title(doc, "Swiss Statistical Society Survey")
  doc <- add_report_subtitle(doc, "Statistical professions in Switzerland")
  doc <- add_report_subtitle(doc, "Automatically generated analysis report")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, paste0("Respondents in current data set: n = ", nrow(clean_data)), style = "Normal")
  doc <- body_add_par(doc, paste0("Generated: ", format(Sys.Date(), "%d %B %Y")), style = "Normal")
  doc <- body_add_break(doc)

  # Table of contents
  # Keep a visible R-generated contents list because native Word TOC fields do
  # not contain computed entries/page numbers until Word refreshes the field.
  toc_sections <- list(
    list(title = "Descriptive results — all respondents", include = TRUE),
    list(title = "Descriptive results — SSS members", include = isTRUE(WORD_REPORT_INCLUDE_SSS_MEMBERS)),
    list(title = "Career pathways", include = TRUE),
    list(title = "Labour market", include = TRUE),
    list(title = "Salary and employment conditions", include = TRUE),
    list(title = "Future research questions", include = TRUE),
    list(title = "Hidden statistical roles", include = TRUE)
  )
  doc <- body_add_par(doc, "Table of contents", style = pick_style(doc, c("heading 1", "Heading 1", "Normal")))
  if (isTRUE(WORD_REPORT_STATIC_TOC)) {
    doc <- add_visible_toc(doc, toc_sections, include_appendix = WORD_REPORT_INCLUDE_APPENDIX_A)
  }
  if (isTRUE(WORD_REPORT_DYNAMIC_TOC)) {
    doc <- body_add_par(doc, "Word table of contents with page numbers",
                        style = pick_style(doc, c("heading 2", "Heading 2", "Normal")))
    doc <- body_add_toc(doc, level = 3)
    doc <- body_add_par(
      doc,
      "The page-numbered table above is a native Word field. If it is blank or outdated, press Ctrl+A then F9 (or right-click the field and choose Update Field). The visible contents list remains available even before fields are refreshed.",
      style = "Normal"
    )
  }
  doc <- body_add_break(doc)

  # Brief report note
  doc <- body_add_par(doc, "About this report", style = pick_style(doc, c("heading 1", "Heading 1", "Normal")))
  doc <- body_add_par(
    doc,
    paste0(
      "This document is generated directly from the R analysis pipeline. Figures are inserted from ",
      "descriptives_plots/, while tables are recreated as editable Word tables from the CSV mirrors in csv_files/. ",
      "Re-running analysis.R therefore refreshes the analysis outputs and rebuilds this Word report."
    ),
    style = "Normal"
  )

  # ── Result sections ──────────────────────────────────────────────
  for (sec in section_definitions) {
    if (!isTRUE(sec$include)) next

    doc <- body_add_break(doc)
    doc <- body_add_par(doc, sec$title, style = pick_style(doc, c("heading 1", "Heading 1", "Normal")))

    if (identical(sec$folder, "salary_and_conditions")) {
      doc <- add_salary_methodology(doc)
    }

    csv_folder <- file.path(csv_dir, sec$folder)
    png_folder <- file.path(out_dir, sec$folder)

    csv_files <- if (dir.exists(csv_folder)) {
      list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)
    } else character(0)

    png_files <- if (dir.exists(png_folder)) {
      list.files(png_folder, pattern = "\\.png$", full.names = TRUE)
    } else character(0)

    csv_stems <- tools::file_path_sans_ext(basename(csv_files))
    png_stems <- tools::file_path_sans_ext(basename(png_files))
    stems <- unique(c(csv_stems, png_stems))

    if (length(stems) == 0) {
      doc <- body_add_par(doc, "No generated outputs were found for this section.", style = "Normal")
      next
    }

    # Preserve the order generated on disk instead of alphabetically mixing
    # tables and figures. CSV order is used first, then plot-only outputs.
    stems <- c(csv_stems, setdiff(png_stems, csv_stems))
    stems <- unique(stems)

    for (stem in stems) {
      title <- report_title_for(stem)
      doc <- body_add_par(doc, title, style = pick_style(doc, c("heading 2", "Heading 2", "Normal")))

      csv_path <- file.path(csv_folder, paste0(stem, ".csv"))
      png_path <- file.path(png_folder, paste0(stem, ".png"))

      if (file.exists(csv_path)) doc <- add_csv_table(doc, csv_path, title)
      if (file.exists(png_path)) doc <- add_png_figure(doc, png_path, title)
    }
  }

  # ── Appendix A ───────────────────────────────────────────────────
  if (isTRUE(WORD_REPORT_INCLUDE_APPENDIX_A)) {
    doc <- add_appendix_a(doc)
  }

  # ── Save ─────────────────────────────────────────────────────────
  print(doc, target = WORD_REPORT_FILE)
  message("✔ Word report generated → ", normalizePath(WORD_REPORT_FILE, winslash = "/", mustWork = FALSE))
}
