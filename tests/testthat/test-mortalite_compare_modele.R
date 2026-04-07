test_that("mortalite_compare_modele retourne une liste structurée avec data et flextable valides", {
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
  expect_named(res, c("success", "message", "data", "flextable"))
  
  # Statut global
  expect_true(res$success)
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
  
  # Types cohérents
  expect_type(res$data$aicc, "double")
  expect_type(res$data$delta_aic, "double")
  expect_type(res$data$Z, "double")
  expect_type(res$data$ajustement_hnp, "double")
  expect_type(res$data$convergence, "logical")
  expect_type(res$data$commentaire, "character")
  
  # Si au moins un AICc est calculé, il doit exister au moins un delta_aic = 0
  if (!all(is.na(res$data$aicc))) {
    expect_true(any(res$data$delta_aic == 0, na.rm = TRUE))
  }
  
  # Le commentaire du ou des modèles avec ΔAICc == 0 doit être cohérent
  lignes_min <- res$data |>
    dplyr::filter(delta_aic == 0)
  
  if (nrow(lignes_min) > 0) {
    for (commentaire_courant in lignes_min$commentaire) {
      expect_true(
        grepl("AICc est le plus faible", commentaire_courant) ||
          grepl("meilleur modèle parmi les options disponibles", commentaire_courant) ||
          grepl("n'a pas convergé", commentaire_courant)
      )
    }
  }
})

test_that("mortalite_compare_modele retourne success = FALSE si la colonne age est absente", {
  df <- tibble::tibble(number = c(10, 20, 30))
  
  res <- mortalite_compare_modele(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le tableau doit contenir les colonnes `age` et `number`."
  )
  expect_null(res$data)
  expect_null(res$flextable)
})

test_that("mortalite_compare_modele retourne success = FALSE si la colonne number est absente", {
  df <- tibble::tibble(age = 1:3)
  
  res <- mortalite_compare_modele(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le tableau doit contenir les colonnes `age` et `number`."
  )
  expect_null(res$data)
  expect_null(res$flextable)
})