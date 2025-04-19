# tests/testthat/helper_create_xlsx_get_analysis_data.R

dir.create("tests/testthat/testdata", showWarnings = FALSE, recursive = TRUE)

library(writexl)

# Stations ----
stations <- data.frame(
  "Numéro LCE"     = c("00045", "00045", "00045"),
  "Type de pêche"  = c("PENT", "PENT", "PENDJ"),
  "Année"          = c(2022, 2022, 2022),
  "No station"     = c("ST01", "ST02", "ST03"),
  "Valide"         = c("O", "O", "N"),
  "Hasard"         = c("O", "N", "O")
)

# Spécimens ----
specimens <- data.frame(
  "Numéro LCE"     = c("00045", "00045"),
  "Type de pêche"  = c("PENT", "PENT"),
  "Année"          = c(2022, 2022),
  "No station"     = c("ST01", "ST02"),
  "No spécimen"    = c("SP01", "SP02"),   # <- colonne obligatoire : no_specimen
  "sp"             = c("SAFO", "SAFO"),
  "ltm"            = c(250, 300),
  "masse"          = c(150, 200),
  "age"            = c(3, 4),             # <- colonne obligatoire
  "sexe"           = c("M", "F"),         # <- colonne obligatoire
  "maturite"       = c("IM", "MA")        # <- colonne obligatoire
)

# Récolte ----
recolte <- data.frame(
  "Numéro LCE"     = c("00045", "00045", "00045"),
  "Type de pêche"  = c("PENT", "PENT", "PENT"),
  "Année"          = c(2022, 2022, 2022),
  "No station"     = c("ST01", "ST02", "ST99"),
  "sp"             = c("SAFO", "SAFO", "SAFO"),
  "nb_capture"     = c(5, NA, 3),
  "nb_pese"        = c(2, 1, NA)
)

# Export ----
jeu_test <- list(
  Stations  = stations,
  Specimens = specimens,
  Recolte   = recolte
)

write_xlsx(jeu_test, "tests/testthat/testdata/jeu_test_get_analysis_data.xlsx")
