# ══════════════════════════════════════════════════════════════════
#  MAIN — point d'entrée : sourcer dans l'ordre
# ══════════════════════════════════════════════════════════════════

source("src/01_config.R")    # librairies + constantes
source("src/02_cleaning.R")  # import + nettoyage -> clean_data
source("src/03_helpers.R")   # fonctions graphiques réutilisables
source("src/03b_table_helpers.R") # fonctions de tableaux stylés (gt)
source("src/04_plots_basic.R")    # section 5.4.0 — descriptives (plots)
source("src/05_plots_advanced.R") # section 5.4.1 — analyses croisées (plots)
source("src/06_tables.R")         # tableaux stylés .png (miroir des plots)