test_that("Retourne une liste avec data et flextable valides", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("hnp")
  skip_if_not_installed("flextable")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(200, 140, 90, 60, 40, 25, 12, 5)
  )
  
  res <- suppressMessages(mortalite_compare_modele(df))
  
  # Structure de retour
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  # Colonnes attendues
  expected_cols <- c(
    "methode", "ajustement_hnp", "aicc", "delta_aic",
    "Z", "SE", "A", "ic95", "convergence", "commentaire"
  )
  expect_true(all(expected_cols %in% names(res$data)))
  
  # Lignes : 5 modèles testés
  expect_equal(nrow(res$data), 5)
  
  # Types numériques cohérents
  expect_type(res$data$aicc, "double")
  expect_type(res$data$delta_aic, "double")
  expect_type(res$data$Z, "double")
  expect_type(res$data$ajustement_hnp, "double")
  
  # Convergence doit être booléenne
  expect_type(res$data$convergence, "logical")
  
  # delta_aic doit avoir au moins un 0
  expect_true(any(res$data$delta_aic == 0))
  
  # Le commentaire du modèle à ΔAICc == 0 doit être adapté
  lignes_min <- res$data |>
    dplyr::filter(delta_aic == 0)
  for (c in lignes_min$commentaires) {
    expect_true(
      grepl("AICc est le plus faible", c) ||
        grepl("Il s'agit toutefois du meilleur modèle", c)
    )
  }
})

test_that("Erreur si colonnes age ou number absentes", {
  df1 <- tibble::tibble(number = c(10, 20, 30))
  df2 <- tibble::tibble(age = 1:3)
  
  expect_error(mortalite_compare_modele(df1), "age")
  expect_error(mortalite_compare_modele(df2), "number")
})
