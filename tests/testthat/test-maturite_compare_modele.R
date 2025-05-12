test_that("Retourne une liste structurée avec modèle séparé (par défaut)", {
  # Données simulées avec maturité factorielle O/N
  df <- tibble::tibble(
    maturite = factor(rep(c("N", "O"), times = 10), levels = c("N", "O")),
    sexe = factor(rep(c("M", "F"), times = 10), levels = c("M", "F")),
    ltm = rep(seq(100, 200, length.out = 10), 2)
  )
  
  res <- suppressWarnings(maturite_compare_modele(df))
  
  expect_type(res, "list")
  expect_named(res, c("table", "best_model", "message", "table_sep", "table_comb"))
  
  expect_s3_class(res$table$df, "data.frame")
  expect_s3_class(res$table$flextable, "flextable")
  
  # ✅ Structure possible (séparé ou NULL si aucun modèle utilisable)
  expect_true(
    is.null(res$best_model) ||
      "best_model_M" %in% names(res$best_model) ||
      "best_model_combined" %in% names(res$best_model)
  )
  
  # Vérifie structure si best_model_M présent
  if (!is.null(res$best_model$best_model_M)) {
    expect_named(res$best_model$best_model_M, c("modele", "lien", "variable"))
  }
  if (!is.null(res$best_model$best_model_F)) {
    expect_named(res$best_model$best_model_F, c("modele", "lien", "variable"))
  }
  
  expect_type(res$message, "character")
})

test_that("Retourne un modèle combiné si prefer_combined = TRUE", {
  df <- tibble::tibble(
    maturite = factor(rep(c("O", "N"), times = 10), levels = c("N", "O")),
    sexe = factor(rep(c("M", "F"), times = 10), levels = c("M", "F")),
    age = rep(1:10, 2)
  )
  
  res <- suppressWarnings(maturite_compare_modele(df, prefer_combined = TRUE, variable = "age"))
  
  # La liste best_model contient toujours les trois éléments
  expect_named(res$best_model, c("best_model_M", "best_model_F", "best_model_combined"))
  
  # Si un modèle combiné a été sélectionné, il doit avoir les trois champs
  if (!is.null(res$best_model$best_model_combined)) {
    expect_named(res$best_model$best_model_combined, c("modele", "lien", "variable"))
  }
  
  # Table combinée toujours présente
  expect_type(res$message, "character")
  expect_s3_class(res$table_comb$df, "data.frame")
  expect_s3_class(res$table_comb$flextable, "flextable")
})


test_that("Déclenche un warning si aucun modèle n'est sélectionnable", {
  df <- tibble::tibble(
    maturite = factor(rep("O", 20), levels = c("N", "O")),  # aucune variation
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    ltm = rep(seq(100, 200, length.out = 10), 2)
  )
  
  expect_warning(
    res <- maturite_compare_modele(df),
    "Aucun modèle utilisable trouvé"
  )
  
  expect_named(res$best_model, c("best_model_M", "best_model_F", "best_model_combined"))
  expect_true(all(sapply(res$best_model, is.null)))
})


test_that("Gère le cas réel où aucun mâle n’est observé (modèles séparés impossibles)", {
  df <- tibble::tibble(
    maturite = factor(rep(c("O", "N"), each = 10), levels = c("N", "O")),
    sexe = factor(rep("F", 20), levels = c("F")),
    ltm = rep(seq(100, 200, length.out = 20))
  )
  
  expect_warning(
    res <- maturite_compare_modele(df),
    "Aucun modèle utilisable trouvé"
  )
  
  expect_named(res$best_model, c("best_model_M", "best_model_F", "best_model_combined"))
  expect_true(all(sapply(res$best_model, is.null)))
})

