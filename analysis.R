# ══════════════════════════════════════════════════════════════════
#  MAIN — point d'entrée : sourcer dans l'ordre
# ══════════════════════════════════════════════════════════════════

source("src/01_config.R")
source("src/02_cleaning.R")
source("src/03_helpers.R")
source("src/03b_table_helpers.R")
source("src/06_tables.R")
source("src/07_tables_derived_variables.R")   # crée is_manager, job_profile, profile_order...
source("src/04_plots_basic.R")                # peut rester ici ou avant, n'en dépend pas
source("src/05_plots_advanced.R")             # doit venir APRÈS le 07