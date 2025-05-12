test_that("Cas nominal : modèle NB1 retourne un tableau structuré", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("hnp")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(100, 90, 75, 60, 45, 30, 20, 10)
  )
  
  
  res <- suppressMessages(mortalite_fit_modele_nb1(df))  # évite message() console
  
  # Type et structure
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  
  # Colonnes attendues
  expected_cols <- c("methode", "ajustement_hnp", "aicc", "Z", "SE", "A",
                     "IC 95%", "commentaire", "convergence", "nb_iterations_hnp")
  expect_true(all(expected_cols %in% names(res)))
  
  # Valeurs numériques
  expect_gt(res$Z, 0)
  expect_gt(res$SE, 0)
  expect_gte(res$A, 0)
  expect_lte(res$A, 100)
  
  # Format IC 95%
  expect_match(res$`IC 95%`, "^\\[[0-9.]+-[0-9.]+\\]$")
  
  # Ajustement HNP valide
  expect_gte(res$ajustement_hnp, 0)
  expect_lte(res$ajustement_hnp, 100)
  
  # Nb répétitions HNP cohérent
  if (res$ajustement_hnp < 10 || res$ajustement_hnp > 15) {
    expect_equal(res$nb_iterations_hnp, 2)
  } else {
    expect_equal(res$nb_iterations_hnp, 5)
  }
  
  # Commentaire
  commentaire_attendu <- dplyr::case_when(
    res$ajustement_hnp < 10 ~ "Bon ajustement",
    res$ajustement_hnp < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  expect_equal(res$commentaire, commentaire_attendu)
  
  # Convergence
  expect_true(res$convergence)
})

test_that("Erreur si colonnes age ou number absentes", {
  skip_if_not_installed("glmmTMB")
  
  df1 <- tibble::tibble(number = c(10, 20, 30))
  df2 <- tibble::tibble(age = 1:3)
  
  expect_error(mortalite_fit_modele_nb1(df1), "age")
  expect_error(mortalite_fit_modele_nb1(df2), "number")
})
