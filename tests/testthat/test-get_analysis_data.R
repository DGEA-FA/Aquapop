# tests/testthat/test-get_analysis_data.R

test_that("get_analysis_data retourne les objets attendus", {
  res <- get_analysis_data(
    path = "testdata/jeu_test_get_analysis_data.xlsx",
    typ_pech = "PENT",
    no_lac   = "00045",
    annee    = 2022,
    verbose  = FALSE
  )
  
  # Structure générale
  expect_type(res, "list")
  expect_named(res, c(
    "data_station", "station_valide", "station_hasard_valide",
    "specimen_tous", "specimen_valide","specimen_hasard_valide", "capture"
  ))
  
  # Filtrage des spécimens par espèce cible
  expect_true(all(res$specimen_tous$sp == "SAFO"))
  expect_true(all(res$specimen_valide$sp == "SAFO"))
  
  # Vérifie que les captures contiennent les colonnes obligatoires
  expect_true("nb_capture" %in% colnames(res$capture))
  expect_true("nb_pese" %in% colnames(res$capture))
  expect_type(res$capture$nb_capture, "double")
  expect_type(res$capture$nb_pese, "double")
  expect_false(any(is.na(res$capture$nb_capture)))
  expect_false(any(is.na(res$capture$nb_pese)))
  
  # Vérifie qu’il n’y a pas de doublons
  expect_equal(nrow(res$specimen_tous), nrow(dplyr::distinct(res$specimen_tous)))
  expect_equal(nrow(res$capture), nrow(dplyr::distinct(res$capture)))
  
  # Présence des stations sans capture
  stations_sans_capture <- setdiff(res$station_hasard_valide$no_station, res$data_station$no_station[res$data_station$no_station %in% res$capture$no_station])
  expect_true(length(stations_sans_capture) == 0)
})

test_that("get_analysis_data échoue si typ_pech inconnu", {
  expect_error(
    get_analysis_data(
      path = "testdata/jeu_test_get_analysis_data.xlsx",
      typ_pech = "XYZ",
      no_lac = "00045",
      annee = 2022,
      verbose = FALSE
    ),
    "Type de pêche inconnu"
  )
})
