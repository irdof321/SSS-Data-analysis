# SSS Data Analysis

Analysis pipeline for the **Swiss Statistical Society (SSS) survey**: *"The professional
landscape of Statisticians in Switzerland: Education, Careers, and Job Satisfaction."*

## About the study

This project analyses data from a nationwide, cross-sectional online survey assessing the
education, professional activities, salaries, and working conditions of statisticians and
statistics-related professionals in Switzerland. The survey was commissioned by the Swiss
Statistical Society and distributed to both SSS members and non-members (via academic
institutions, federal offices, and private-sector organizations) to capture a broad view of
the field.

The full research protocol — background, eligibility criteria, questionnaire structure, data
cleaning rules, derived variables, and planned analyses — is available in
[`Protocol__The_professional_landscape_of_Statisticians_in_Switzerland.pdf`](./Protocol__The_professional_landscape_of_Statisticians_in_Switzerland.pdf).

All analyses are produced twice: once for the **full respondent population**, and once
restricted to **SSS members only**, in order to compare the general field to the Society's
membership.

## Repository structure

```
.
├── analysis.R                          # entry point — sources everything in order
├── src/
│   ├── 01_config.R                     # libraries, global constants, style, output paths
│   ├── 02_cleaning.R                   # raw survey import -> clean_data (+ derived variables)
│   ├── 03_helpers.R                    # reusable ggplot2 helpers (bar charts, histograms, ...)
│   ├── 03b_table_helpers.R             # reusable gt table helpers (styling, saving)
│   ├── 04_plots_basic.R                # descriptive plots (protocol §5.4.0) — currently disabled
│   ├── 05_plots_advanced.R             # cross-tabulated plots (protocol §5.4.1) — currently disabled
│   ├── 06_tables.R                     # styled descriptive tables (mirrors the plots)
│   └── 07_tables_derived_variables.R   # tables on derived variables (§5.3.1 & §5.3.2 of the protocol)
├── Protocol__...pdf                    # full research protocol
└── README.md
```

## Pipeline

Running `analysis.R` sources the following scripts, in order:

1. **`01_config.R`** — loads required libraries, defines global constants (reference year,
   output directories, color palette, factor level orderings) and the `generate_csv_files`
   switch (see below).
2. **`02_cleaning.R`** — imports the raw LimeSurvey export (`data_file`, semicolon-separated
   CSV) and builds `clean_data`: recoded factors, numeric conversions, multi-select fields
   (training fields, skills, continuous education, etc.), and derived variables such as
   `age`, `salary` (full-time-equivalent standardised, per protocol §5.2.2.1), `career_stage`
   (per protocol §5.2.2.2), `age_group`, `exp_group`, and `is_sss_member`. A copy is also
   exported to `my_df.csv`.
3. **`03_helpers.R`** / **`03b_table_helpers.R`** — reusable plotting (ggplot2) and table
   (gt) helper functions used by the scripts below.
4. **`04_plots_basic.R`** / **`05_plots_advanced.R`** — descriptive and cross-tabulated
   plots. *Currently commented out in `analysis.R`*: the project has moved to styled `gt`
   tables (see `06_tables.R` and `07_tables_derived_variables.R`) as the primary output
   format.
5. **`06_tables.R`** — generates all descriptive tables required by protocol §5.1 (frequency
   tables for categorical variables, summary tables for continuous variables, multi-select
   tables), for both the full population and the SSS-members-only subset.
6. **`07_tables_derived_variables.R`** — generates tables addressing protocol §5.3.1 (career
   information for students) and §5.3.2 (labour market needs: skills, sectors, career paths),
   built on the derived variables from `02_cleaning.R`.

## Outputs

| Directory              | Content                                                        |
|------------------------|-----------------------------------------------------------------|
| `descriptives_plots/`  | ggplot2 plots (when `04_plots_basic.R` / `05_plots_advanced.R` are enabled) |
| `descriptives_tables/` | Styled `.png` tables (via `gt`), split into subfolders per population/topic: `full_population/`, `sss_members/`, `career_pathways/`, `labour_market/`, `salary_and_conditions/`, `future_research/`, `hidden_statistical_roles/` |
| `csv_files/`           | Optional plain-`.csv` mirror of every table in `descriptives_tables/`, with the same subfolder structure — useful for further analysis outside R (e.g. Python) |

### CSV export switch

Set in `01_config.R`:

```r
generate_csv_files <- TRUE   # set to FALSE to only produce the styled .png tables
```

When enabled, every call to `save_gt()` (used throughout `06_tables.R` and
`07_tables_derived_variables.R`) writes a `.csv` version of the underlying table data
alongside its `.png`, in `csv_files/`, mirroring the subfolder structure of
`descriptives_tables/`.

## R environment

### Installing a package

To install a package in your R environment, enter the following in the console:

```r
install.packages("my_pckg_name")
```

### Update / freeze the environment (lockfile)

After adding, removing, or upgrading packages, update the lockfile:

```r
renv::snapshot()
```

### Restore the environment (on a new machine / fresh clone)

After cloning the repo, open the project in RStudio and run:

```r
renv::restore()
```

## Running the pipeline

1. Place the raw survey export CSV (set via `data_file` in `01_config.R`) at the project root.
2. Open the project in RStudio and run `renv::restore()` to install the exact package versions.
3. Run `source("analysis.R")`.
4. Outputs are written to `descriptives_plots/`, `descriptives_tables/`, and (if enabled)
   `csv_files/`.