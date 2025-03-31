# ============================================================================
# Script : exemple_utilisation.R
# Rôle   : Démonstration simple de l’utilisation des fonctions métier AquaPop
#          hors de l'application Shiny
# ============================================================================

# 1. Charger les dépendances et les fonctions --------------------------------
source("R/load_packages.R")

path <- "data/Extract IFA_R04_AquaPop.xlsx"

# (À terme : source ici les fichiers contenant les fonctions que tu veux tester)
source("R/import_data.R")        # À créer bientôt
source("R/biomasse_table.R")     # Supposé déjà fonctionnel

# 2. Charger les données d’exemple ------------------------------------------
# (Données extraites de la base IFA au bon format)
donnees_capture <- readxl::read_excel("data/exempledata.xlsx", sheet = "capture")
donnees_specimen <- readxl::read_excel("data/exempledata.xlsx", sheet = "specimen")
donnees_station <- readxl::read_excel("data/exempledata.xlsx", sheet = "station")

# 3. Calculer un indicateur simple ------------------------------------------
# (ici on teste seulement biomasse_table)
table_biomasse <- biomasse_table(
  data_capture = donnees_capture,
  data_specimen = donnees_specimen,
  data_station = donnees_station
)

# 4. Exporter les résultats --------------------------------------------------
writexl::write_xlsx(table_biomasse, "resultats/biomasse_demo.xlsx")

message("✅ Exemple d'utilisation complété. Résultat dans : resultats/biomasse_demo.xlsx")
