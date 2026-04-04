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
  expect_named(
    resultat,
    c("success", "data", "flextable", "plot", "message")
  )
  
  # --- Succès attendu ---
  expect_true(resultat$success)
  expect_null(resultat$message)
  
  # --- Types attendus ---
  expect_s3_class(resultat$data, "data.frame")
  expect_s3_class(resultat$plot, "ggplot")
  expect_s3_class(resultat$flextable, "flextable")
  
  # --- Colonnes attendues ---
  expect_true(all(c(
    "coefficient",
    "estimation",
    "erreur_standard",
    "ic95"
  ) %in% names(resultat$data)))
})

test_that("masse_longueur_fit() échoue si plus d'une espèce est présente", {
  data_multi_especes <- tibble::tibble(
    ltm = c(100, 120, 140),
    masse = c(10, 12, 14),
    sp = c("SAFO", "SAVI", "SAFO"),
    no_specimen = 1:3
  )
  
  expect_error(
    masse_longueur_fit(data_multi_especes),
    "une seule espèce"
  )
})

test_that("masse_longueur_fit() retourne success = FALSE si les données sont vides", {
  data_vide <- tibble::tibble(
    ltm = numeric(0),
    masse = numeric(0),
    sp = character(0),
    no_specimen = integer(0)
  )
  
  resultat <- masse_longueur_fit(data_vide)
  
  expect_type(resultat, "list")
  expect_false(resultat$success)
  expect_null(resultat$data)
  expect_null(resultat$flextable)
  expect_null(resultat$plot)
  expect_match(
    resultat$message,
    "Aucun spécimen valide disponible"
  )
})

test_that("masse_longueur_fit() gère les NA en les filtrant", {
  data_na <- tibble::tibble(
    ltm = c(100, 120, NA, 160, 180),
    masse = c(10, NA, 22, 30, 40),
    sp = rep("SAFO", 5),
    no_specimen = 1:5
  )
  
  resultat_na <- masse_longueur_fit(data_na)
  
  expect_true(resultat_na$success)
  expect_null(resultat_na$message)
  expect_s3_class(resultat_na$data, "data.frame")
  expect_equal(nrow(resultat_na$data), 2)
  expect_s3_class(resultat_na$plot, "ggplot")
  expect_s3_class(resultat_na$flextable, "flextable")
})

test_that("masse_longueur_fit() retourne success = FALSE si aucune donnée exploitable n'est disponible après filtrage", {
  data_sans_valeurs_exploitables <- tibble::tibble(
    ltm = c(NA_real_, NA_real_, NA_real_),
    masse = c(NA_real_, NA_real_, NA_real_),
    sp = rep("SAFO", 3),
    no_specimen = 1:3
  )
  
  resultat <- masse_longueur_fit(data_sans_valeurs_exploitables)
  
  expect_false(resultat$success)
  expect_null(resultat$data)
  expect_null(resultat$flextable)
  expect_null(resultat$plot)
  expect_match(
    resultat$message,
    "Aucune donnée exploitable"
  )
})

test_that("masse_longueur_fit() retourne success = FALSE s'il y a moins de deux spécimens exploitables", {
  data_un_seul_point <- tibble::tibble(
    ltm = c(150, NA_real_, NA_real_),
    masse = c(25, NA_real_, NA_real_),
    sp = rep("SAFO", 3),
    no_specimen = 1:3
  )
  
  resultat <- masse_longueur_fit(data_un_seul_point)
  
  expect_false(resultat$success)
  expect_null(resultat$data)
  expect_null(resultat$flextable)
  expect_null(resultat$plot)
  expect_match(
    resultat$message,
    "au moins deux spécimens"
  )
})

test_that("masse_longueur_fit() retourne success = FALSE si toutes les longueurs valides sont identiques", {
  data_longueur_constante <- tibble::tibble(
    ltm = c(150, 150, 150),
    masse = c(20, 25, 30),
    sp = rep("SAFO", 3),
    no_specimen = 1:3
  )
  
  resultat <- masse_longueur_fit(data_longueur_constante)
  
  expect_false(resultat$success)
  expect_null(resultat$data)
  expect_null(resultat$flextable)
  expect_null(resultat$plot)
  expect_match(
    resultat$message,
    "longueurs valides sont identiques"
  )
})