dir.create("tests/testthat/testdata", showWarnings = FALSE, recursive = TRUE)

library(writexl)

# Jeu de base avec colonnes synonymes
lac_complet <- data.frame(
  "Numéro LCE"     = "12345",
  "Nom du lac"     = "Lac Complet",
  "Type pêche"     = "PE",
  "Année"          = "2022",
  "Code méthode"   = "EP",
  "Longitude"      = "-75.1234",
  "Latitude"       = "46.9876",
  "Territoire"     = "01",
  "Zone pêche"     = "2A",
  "Surface_ha"     = "45.6",
  "Périmètre"      = "3.2",
  "Profondeur_max" = "8",
  "Profondeur_moy" = "4.5",
  "Observations"   = "Données complètes"
)

# Variante sans comments
lac_sans_comments <- lac_complet[, !names(lac_complet) %in% "Observations"]

# Variante avec colonnes supplémentaires
lac_avec_colonnes_sup <- lac_complet
lac_avec_colonnes_sup$extra1 <- "inutilisée"
lac_avec_colonnes_sup$extra2 <- "à ignorer"

# Variante avec colonne obligatoire manquante (ex. : "Nom du lac")
lac_manque_colonne <- data.frame(
  "Numéro LCE" = "123",
  "Type pêche" = "PE",
  "Année" = "2023"
)
# Variante avec colonnes dans un ordre aléatoire
set.seed(42)
lac_colonnes_desordonnees <- lac_complet[, sample(names(lac_complet))]

# Sauvegarde avec nom du feuillet = "Lac"
write_xlsx(list("Lac" = lac_complet),                  "tests/testthat/testdata/lac_complet.xlsx")
write_xlsx(list("Lac" = lac_sans_comments),            "tests/testthat/testdata/lac_sans_comments.xlsx")
write_xlsx(list("Lac" = lac_avec_colonnes_sup),        "tests/testthat/testdata/lac_avec_colonnes_sup.xlsx")
write_xlsx(list("Lac" = lac_manque_colonne), "tests/testthat/testdata/lac_manque_colonne.xlsx")
write_xlsx(list("Lac" = lac_colonnes_desordonnees),    "tests/testthat/testdata/lac_colonnes_desordonnees.xlsx")
