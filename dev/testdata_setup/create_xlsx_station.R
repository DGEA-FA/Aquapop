# Ce script génère tous les fichiers de test de la fonction load_station()
# À exécuter manuellement ou via testthat::source_test_helpers()

# tests/testthat/helper_create_xlsx_station.R

dir.create(testthat::test_path("testdata"), showWarnings = FALSE, recursive = TRUE)
library(writexl)


# Cas nominal : toutes les colonnes présentes
station_complet <- data.frame(
  "Numéro LCE"     = "001",
  "Type de pêche"  = "PE",
  "Année"          = "2022",
  "No station"     = "ST01",
  "Latitude"       = "46.123",
  "Longitude"      = "-73.456",
  "Date de levée"  = as.character(44927),  # équiv. 2023-01-01
  "Heure de pose"  = "6",
  "Minute pose"    = "30",
  "Heure de levée" = "7",
  "Minute levée"   = "45",
  "Hasard"         = "O",
  "Valide"         = "O",
  "Profondeur début" = "2.5",
  "Profondeur fin"   = "3.1",
  "Type maillage"    = "M1",
  "Commentaires"     = "RAS"
)

# Variante avec colonnes en désordre
set.seed(1)
station_desordonnee <- station_complet[, sample(names(station_complet))]

# Variante avec colonnes supplémentaires
station_colonnes_sup <- station_complet
station_colonnes_sup$extra1 <- "inutile"
station_colonnes_sup$extra2 <- "à ignorer"

# Variante sans colonnes optionnelles
station_sans_optionnelles <- station_complet[, 1:13]

# Variante avec colonnes obligatoires manquantes
# Variante robuste : toutes les colonnes obligatoires renommées de façon à être méconnaissables
station_manque_obligatoire <- station_complet
noms_modifies <- c(
  "Numéro LCE"     = "CODE LAC BIDON",
  "Type de pêche"  = "PÊCHE",
  "Année"          = "année (modifiée)",
  "No station"     = "identifiant station inconnu",
  "Valide"         = "statut V",
  "Hasard"         = "méthode tirage"
)

# Renommer les colonnes spécifiées
# Renommer uniquement les colonnes existantes parmi les noms à modifier
for (nom_original in names(noms_modifies)) {
  if (nom_original %in% names(station_manque_obligatoire)) {
    names(station_manque_obligatoire)[names(station_manque_obligatoire) == nom_original] <- noms_modifies[[nom_original]]
  }
}

# Variante avec IND et NA dans st_valide / st_hasard
station_statuts_ind <- station_complet
station_statuts_ind$Valide <- NA
station_statuts_ind$Hasard <- "IND"

# Variante avec année Excel et dates correctes
station_excel_annee <- station_complet
station_excel_annee$Année <- as.character(44927)  # Excel numeric date = 2023-01-01

# Sauvegarde dans le dossier testdata/ avec chemins robustes
write_xlsx(list("Stations" = station_complet),            testthat::test_path("testdata", "station_complet.xlsx"))
write_xlsx(list("Stations" = station_desordonnee),        testthat::test_path("testdata", "station_desordonnee.xlsx"))
write_xlsx(list("Stations" = station_colonnes_sup),       testthat::test_path("testdata", "station_colonnes_sup.xlsx"))
write_xlsx(list("Stations" = station_sans_optionnelles),  testthat::test_path("testdata", "station_sans_optionnelles.xlsx"))
write_xlsx(list("Stations" = station_manque_obligatoire), testthat::test_path("testdata", "station_manque_obligatoire.xlsx"))
write_xlsx(list("Stations" = station_statuts_ind),        testthat::test_path("testdata", "station_statuts_ind.xlsx"))
write_xlsx(list("Stations" = station_excel_annee),        testthat::test_path("testdata", "station_excel_annee.xlsx"))