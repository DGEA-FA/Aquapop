test_that("masse_longueur_fit() retourne une liste bien structurée", {
  # --- Données fictives valides ---
  data_valide <- tibble::tibble(
    ltm = c(100, 120, 140, 160, 180),
    masse = c(10, 15, 25, 35, 50),
    sp = rep("SAFO", 5),
    no_specimen = 1:5
  )
  
  resultat <- masse_longueur_fit(data_valide)
  
  # --- Structure de sortie ---
  expect_type(resultat, "list")
  expect_named(resultat, c("data", "flextable", "plot"))
  
  # --- Types attendus ---
  expect_s3_class(resultat$data, "data.frame")
  expect_s3_class(resultat$plot, "ggplot")
  expect_s3_class(resultat$flextable, "flextable")
  
  # --- Colonnes attendues ---
  expect_true(all(c("coefficient", "estimation", "erreur_standard", "ic95") %in% names(resultat$data)))
})

test_that("masse_longueur_fit() échoue si plus d'une espèce est présente", {
  data_multi_especes <- tibble::tibble(
    ltm = c(100, 120, 140),
    masse = c(10, 12, 14),
    sp = c("SAFO", "SAVI", "SAFO"),
    no_specimen = 1:3
  )
  
  expect_error(masse_longueur_fit(data_multi_especes), "une seule espèce")
})

test_that("masse_longueur_fit() échoue si les données sont vides", {
  data_vide <- tibble::tibble(
    ltm = numeric(0),
    masse = numeric(0),
    sp = character(0),
    no_specimen = integer(0)
  )
  
  expect_error(masse_longueur_fit(data_vide))
})

test_that("masse_longueur_fit() gère les NA en les filtrant", {
  data_na <- tibble::tibble(
    ltm = c(100, 120, NA, 160, 180),
    masse = c(10, NA, 22, 30, 40),
    sp = rep("SAFO", 5),
    no_specimen = 1:5
  )
  
  resultat_na <- masse_longueur_fit(data_na)
  
  expect_s3_class(resultat_na$data, "data.frame")
  expect_equal(nrow(resultat_na$data), 2)  # 2 lignes pour les 2 coefficients
  expect_s3_class(resultat_na$plot, "ggplot")
})