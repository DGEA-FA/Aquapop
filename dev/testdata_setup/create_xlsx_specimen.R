library(writexl)
dir.create(testthat::test_path("testdata"), showWarnings = FALSE, recursive = TRUE)

# Cas nominal avec toutes les colonnes
specimen_nominal <- data.frame(
  "Numéro LCE"   = "123",
  "Type pêche"   = "PE",
  "Station"      = "ST01",
  "Spécimen"     = "001",
  "Espèce"       = "SANA",
  "LT"           = "110",
  "Poids"        = "15.3",
  "Âge"          = "2",
  "Sexe"         = "F",
  "Maturité"     = "O",
  "Année"        = "2022",
  "Long_fourche" = "105",
  "Statut marquage" = "MA",
  "Indice insecte"  = "0",
  "Indice benthos"  = "1",
  "Indice plancton" = "0",
  "Indice chyme"    = "0",
  "Indice vide"     = "0",
  "Indice poisson"  = "0",
  "Poisson 1"       = "SAFO",
  "Poisson 2"       = "PECA",
  "Commentaires"    = "Individu complet"
)

specimen_minimal <- data.frame(
  no_lac      = "123",
  typ_pech    = "PE",
  no_station  = "ST01",
  no_specimen = "001",
  sp          = "SANA",
  ltm         = "110",
  masse       = "15.3",
  age         = "2",
  sexe        = "F",
  maturite    = "O",
  annee       = "2022"
)

specimen_colonnes_sup <- specimen_minimal
specimen_colonnes_sup$extra1 <- "inutile"
specimen_colonnes_sup$extra2 <- "à ignorer"

specimen_colonne_manquante <- specimen_minimal[, !names(specimen_minimal) %in% "no_lac"]

set.seed(42)
specimen_colonnes_desordonnees <- specimen_nominal[, sample(names(specimen_nominal))]

# Sauvegardes
write_xlsx(list("Specimens" = specimen_nominal),
           testthat::test_path("testdata/specimen_nominal.xlsx"))

write_xlsx(list("Specimens" = specimen_minimal),
           testthat::test_path("testdata/specimen_minimal.xlsx"))

write_xlsx(list("Specimens" = specimen_colonnes_sup),
           testthat::test_path("testdata/specimen_colonnes_sup.xlsx"))

write_xlsx(list("Specimens" = specimen_colonne_manquante),
           testthat::test_path("testdata/specimen_colonne_manquante.xlsx"))

write_xlsx(list("Specimens" = specimen_colonnes_desordonnees),
           testthat::test_path("testdata/specimen_colonnes_desordonnees.xlsx"))
