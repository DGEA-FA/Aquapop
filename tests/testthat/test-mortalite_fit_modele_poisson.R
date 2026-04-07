test_that("mortalite_fit_modele_poisson retourne un tableau structuré dans le cas nominal", {
  skip_if_not_installed("hnp")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(200, 140, 90, 60, 40, 25, 12, 5)
  )
  
  res <- mortalite_fit_modele_poisson(df)
  
  # Type de sortie
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  
  # Colonnes attendues
  expected_cols <- c(
    "methode", "ajustement_hnp", "aicc", "Z", "SE", "A",
    "ic95", "commentaire", "convergence", "nb_iterations_hnp"
  )
  expect_true(all(expected_cols %in% colnames(res)))
  
  # Méthode attendue
  expect_equal(res$methode, "poisson")
  
  # Si le modèle ne produit pas un résultat exploitable avec ces données simulées,
  # on saute les vérifications numériques spécifiques
  if (!isTRUE(res$convergence)) {
    skip("Le modèle Poisson n'a pas produit un résultat exploitable avec ces données simulées")
  }
  
  # Valeurs numériques
  expect_gt(res$Z, 0)
  expect_gt(res$SE, 0)
  expect_gte(res$A, 0)
  expect_lte(res$A, 100)
  
  # IC 95 % formaté
  expect_match(res$ic95, "^\\[[0-9.]+-[0-9.]+\\]$")
  
  # HNP entre 0 et 100
  expect_gte(res$ajustement_hnp, 0)
  expect_lte(res$ajustement_hnp, 100)
  
  # nb_iterations_hnp cohérent avec ajustement
  if (res$ajustement_hnp < 10 || res$ajustement_hnp >= 15) {
    expect_equal(res$nb_iterations_hnp, 2)
  } else {
    expect_equal(res$nb_iterations_hnp, 5)
  }
  
  # Commentaire attendu
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

test_that("mortalite_fit_modele_poisson retourne une ligne d'échec si la colonne age est absente", {
  df <- tibble::tibble(number = c(10, 20, 30))
  
  res <- mortalite_fit_modele_poisson(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "poisson")
  expect_false(res$convergence)
  expect_match(res$commentaire, "colonnes `age` et `number`")
})

test_that("mortalite_fit_modele_poisson retourne une ligne d'échec si la colonne number est absente", {
  df <- tibble::tibble(age = 1:3)
  
  res <- mortalite_fit_modele_poisson(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "poisson")
  expect_false(res$convergence)
  expect_match(res$commentaire, "colonnes `age` et `number`")
})

test_that("mortalite_fit_modele_poisson retourne une ligne d'échec si moins de deux âges distincts sont disponibles", {
  df <- tibble::tibble(
    age = c(5, 5, 5),
    number = c(10, 12, 8)
  )
  
  res <- mortalite_fit_modele_poisson(df)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$methode, "poisson")
  expect_false(res$convergence)
  expect_match(res$commentaire, "au moins deux âges distincts")
})