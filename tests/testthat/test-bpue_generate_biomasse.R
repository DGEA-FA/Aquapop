# test-bpue_generate_biomasse.R
# Tests unitaires – Fonction bpue_generate_biomasse()
# AquaPop – Calcul de la biomasse et BPUE

# --- Jeux de données simulés ----

# Cas nominal : tous les groupes présents
data_station_nominal <- tibble::tibble(no_station = 1:3)
data_specimen_nominal <- tibble::tibble(
  no_station = c(1, 1, 2, 2, 3, 3),
  masse = c(100, 200, 300, 400, 500, 600),
  sexe = c("F", "M", "F", "M", "IND", "F"),
  maturite = c("O", "N", "O", "N", "IND", "O")
)

# Aucun spécimen
data_specimen_empty <- tibble::tibble(
  no_station = integer(),
  masse = numeric(),
  sexe = character(),
  maturite = character()
)
data_station_with_empty <- tibble::tibble(no_station = 1:2)

# Cas sans femelles matures
data_specimen_no_fem_mature <- tibble::tibble(
  no_station = c(1, 2),
  masse = c(200, 300),
  sexe = c("F", "F"),
  maturite = c("N", "N")
)

# --- Début des tests ----

test_that("bpue_generate_biomasse() retourne une liste contenant data et flextable", {
  res <- bpue_generate_biomasse(data_specimen_nominal, data_station_nominal)
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
})

test_that("les colonnes de data sont correctes et arrondies", {
  res <- bpue_generate_biomasse(data_specimen_nominal, data_station_nominal)$data
  expect_equal(colnames(res), c("groupe", "biomasse", "percent", "bpue", "ic95"))
  expect_type(res$biomasse, "double")
  expect_type(res$percent, "double")
  expect_type(res$bpue, "double")
  expect_type(res$ic95, "character")
  expect_true(all(round(res$biomasse, 1) == res$biomasse, na.rm = TRUE))
  expect_true(all(round(res$bpue, 1) == res$bpue, na.rm = TRUE))
  expect_true(all(round(res$percent, 0) == res$percent, na.rm = TRUE))
})



test_that("la fonction gère bien l'absence de groupes (ex: aucune femelle mature)", {
  res <- bpue_generate_biomasse(data_specimen_no_fem_mature, data_station_nominal)
  expect_s3_class(res$data, "data.frame")
  expect_true("Repro. actifs femelles" %in% res$data$groupe)
  expect_true(all(!is.na(res$data$biomasse)))
})

test_that("les stations sans capture sont bien traitées (biomasse = 0)", {
  res <- bpue_generate_biomasse(data_specimen_empty, data_station_with_empty)
  expect_true(all(res$data$biomasse >= 0))
  expect_equal(res$data$biomasse[res$data$groupe == "Tous"], 0)
})

test_that("les IC sont calculés seulement pour Tous et Femelles matures", {
  res <- bpue_generate_biomasse(data_specimen_nominal, data_station_nominal)$data
  groupes_ic <- res$groupe[res$ic95 != ""]
  expect_setequal(groupes_ic, c("Tous", "Repro. actifs femelles"))
})
