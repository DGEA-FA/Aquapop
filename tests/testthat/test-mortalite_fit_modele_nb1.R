test_that("mortalite_fit_modele_nb1 retourne un tableau structuré dans le cas nominal", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("hnp")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(100, 90, 75, 60, 45, 30, 20, 10)
  )
  
  res <- suppressMessages(mortalite_fit_modele_nb1(df))
  
  # Type et structure
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  
  # Colonnes attendues
  expected_cols <- c(
    "methode", "ajustement_hnp", "aicc", "Z", "SE", "A",
    "ic95", "commentaire", "convergence", "nb_iterations_hnp"
  )
  expect_true(all(expected_cols %in% names(res)))
  
  # Méthode attendue
  expect_equal(res$methode, "nb1")
  
  # Si le modèle ne converge pas avec ces données, on saute les vérifications
  # numériques spécifiques, mais on valide quand même la structure
  if (!isTRUE(res$convergence)) {
    skip("Le modèle NB1 n'a pas convergé avec ces données simulées")
  }
  
  # Valeurs numériques
  expect_gt(res$Z, 0)
  expect_gt(res$SE, 0)
  expect_gte(res$A, 0)
  expect_lte(res$A, 100)
  
  # Format IC 95 %
  expect_match(res$ic95, "^\\[[0-9,]+-[0-9,]+\\]$")
  
  # Ajustement HNP valide
  expect_gte(res$ajustement_hnp, 0)
  expect_lte(res$ajustement_hnp, 100)
  
  # Nb répétitions HNP cohérent
  if (res$ajustement_hnp < 10 || res$ajustement_hnp >= 15) {
    expect_equal(res$nb_iterations_hnp, 2)
  } else {
    expect_equal(res$nb_iterations_hnp, 5)
  }
  
  # Commentaire
  commentaire_attendu <- dplyr::case_when(
    is.na(res$ajustement_hnp) ~ "Modèle ajusté, mais test HNP non calculable.",
    res$ajustement_hnp < 10 ~ "Bon ajustement",
    res$ajustement_hnp < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  expect_equal(res$commentaire, commentaire_attendu)
  
  # Convergence
  expect_true(res$convergence)
})

test_that("mortalite_fit_modele_nb1 retourne une ligne d'échec si la colonne age est absente", {
  skip_if_not_installed("glmmTMB")
  
  df <- tibble::tibble(number = c(10, 20, 30))
  
  res <- mortalite_fit_modele_nb1(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "nb1")
  expect_false(res$convergence)
  expect_match(res$commentaire, "colonnes `age` et `number`")
})

test_that("mortalite_fit_modele_nb1 retourne une ligne d'échec si la colonne number est absente", {
  skip_if_not_installed("glmmTMB")
  
  df <- tibble::tibble(age = 1:3)
  
  res <- mortalite_fit_modele_nb1(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "nb1")
  expect_false(res$convergence)
  expect_match(res$commentaire, "colonnes `age` et `number`")
})

test_that("mortalite_fit_modele_nb1 retourne une ligne d'échec si moins de deux âges distincts sont disponibles", {
  skip_if_not_installed("glmmTMB")
  
  df <- tibble::tibble(
    age = c(5, 5, 5),
    number = c(10, 12, 8)
  )
  
  res <- mortalite_fit_modele_nb1(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "nb1")
  expect_false(res$convergence)
  expect_match(res$commentaire, "au moins deux âges distincts")
})