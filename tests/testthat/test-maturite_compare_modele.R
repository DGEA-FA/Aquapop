test_that("maturite_compare_modele retourne une liste structurée avec modèles séparés par défaut", {
  df <- tibble::tibble(
    maturite = factor(rep(c("N", "O"), times = 10), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(rep(c("M", "F"), times = 10), levels = c("F", "M")),
    ltm = rep(seq(100, 200, length.out = 10), 2)
  )
  
  res <- suppressWarnings(
    maturite_compare_modele(df, variable = "ltm")
  )
  
  expect_type(res, "list")
  expect_named(
    res,
    c("success", "table", "best_model", "message", "table_sep", "table_comb")
  )
  
  expect_true(is.logical(res$success))
  expect_true(res$success)
  
  expect_type(res$table, "list")
  expect_named(res$table, c("df", "flextable"))
  expect_s3_class(res$table$df, "data.frame")
  expect_s3_class(res$table$flextable, "flextable")
  
  expect_type(res$table_sep, "list")
  expect_named(res$table_sep, c("df", "flextable"))
  expect_s3_class(res$table_sep$df, "data.frame")
  expect_s3_class(res$table_sep$flextable, "flextable")
  
  expect_type(res$table_comb, "list")
  expect_named(res$table_comb, c("df", "flextable"))
  expect_s3_class(res$table_comb$df, "data.frame")
  expect_s3_class(res$table_comb$flextable, "flextable")
  
  expect_type(res$best_model, "list")
  expect_named(
    res$best_model,
    c("best_model_M", "best_model_F", "best_model_combined")
  )
  
  if (!is.null(res$best_model$best_model_M)) {
    expect_named(res$best_model$best_model_M, c("modele", "lien", "variable"))
  }
  
  if (!is.null(res$best_model$best_model_F)) {
    expect_named(res$best_model$best_model_F, c("modele", "lien", "variable"))
  }
  
  if (!is.null(res$best_model$best_model_combined)) {
    expect_named(res$best_model$best_model_combined, c("modele", "lien", "variable"))
  }
  
  expect_type(res$message, "character")
})

test_that("maturite_compare_modele peut retourner le tableau principal combiné si prefer_combined = TRUE", {
  df <- tibble::tibble(
    maturite = factor(rep(c("O", "N"), times = 10), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(rep(c("M", "F"), times = 10), levels = c("F", "M")),
    age = rep(1:10, 2)
  )
  
  res <- suppressWarnings(
    maturite_compare_modele(
      specimen_data = df,
      prefer_combined = TRUE,
      variable = "age"
    )
  )
  
  expect_type(res, "list")
  expect_named(
    res$best_model,
    c("best_model_M", "best_model_F", "best_model_combined")
  )
  
  expect_true(res$success)
  
  if (!is.null(res$best_model$best_model_combined)) {
    expect_named(
      res$best_model$best_model_combined,
      c("modele", "lien", "variable")
    )
    expect_equal(res$best_model$best_model_combined$variable, "age")
  }
  
  expect_type(res$message, "character")
  expect_s3_class(res$table$df, "data.frame")
  expect_s3_class(res$table$flextable, "flextable")
  expect_s3_class(res$table_comb$df, "data.frame")
  expect_s3_class(res$table_comb$flextable, "flextable")
})

test_that("maturite_compare_modele retourne success = FALSE si aucun modèle n'est possible faute de variation de maturité", {
  df <- tibble::tibble(
    maturite = factor(rep("O", 20), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("F", "M")),
    ltm = rep(seq(100, 200, length.out = 10), 2)
  )
  
  res <- maturite_compare_modele(df, variable = "ltm")
  
  expect_type(res, "list")
  expect_named(
    res,
    c("success", "table", "best_model", "message", "table_sep", "table_comb")
  )
  
  expect_false(res$success)
  expect_type(res$message, "character")
  expect_match(res$message, "ne contiennent pas à la fois des individus immatures et matures")
  
  expect_named(
    res$best_model,
    c("best_model_M", "best_model_F", "best_model_combined")
  )
  expect_true(all(vapply(res$best_model, is.null, logical(1))))
  
  expect_s3_class(res$table$df, "data.frame")
  expect_s3_class(res$table$flextable, "flextable")
  expect_equal(nrow(res$table$df), 0)
  
  expect_s3_class(res$table_sep$df, "data.frame")
  expect_s3_class(res$table_comb$df, "data.frame")
  expect_equal(nrow(res$table_sep$df), 0)
  expect_equal(nrow(res$table_comb$df), 0)
})

test_that("maturite_compare_modele retourne success = FALSE si aucun spécimen exploitable n'est disponible", {
  df <- tibble::tibble(
    maturite = factor(character(), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(character(), levels = c("F", "M")),
    ltm = numeric()
  )
  
  res <- maturite_compare_modele(df, variable = "ltm")
  
  expect_false(res$success)
  expect_type(res$message, "character")
  expect_match(res$message, "Aucun spécimen valide disponible")
  
  expect_named(
    res$best_model,
    c("best_model_M", "best_model_F", "best_model_combined")
  )
  expect_true(all(vapply(res$best_model, is.null, logical(1))))
  
  expect_equal(nrow(res$table$df), 0)
  expect_equal(nrow(res$table_sep$df), 0)
  expect_equal(nrow(res$table_comb$df), 0)
})

test_that("maturite_compare_modele retourne success = FALSE si effectif insuffisant après nettoyage", {
  df <- tibble::tibble(
    maturite = factor(c("N", "O"), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(c("F", "M"), levels = c("F", "M")),
    ltm = c(120, 140)
  )
  
  res <- maturite_compare_modele(df, variable = "ltm")
  
  expect_false(res$success)
  expect_type(res$message, "character")
  expect_match(res$message, "Trop peu d'individus exploitables")
  
  expect_true(all(vapply(res$best_model, is.null, logical(1))))
  expect_equal(nrow(res$table$df), 0)
})

test_that("maturite_compare_modele retourne une structure complète même si un seul sexe est observé", {
  df <- tibble::tibble(
    maturite = factor(rep(c("O", "N"), each = 10), levels = c("N", "O"), ordered = TRUE),
    sexe = factor(rep("F", 20), levels = c("F", "M")),
    ltm = seq(100, 200, length.out = 20)
  )
  
  res <- suppressWarnings(
    maturite_compare_modele(df, variable = "ltm")
  )
  
  expect_type(res, "list")
  expect_named(
    res,
    c("success", "table", "best_model", "message", "table_sep", "table_comb")
  )
  
  expect_true(is.logical(res$success))
  expect_type(res$message, "character")
  
  expect_named(
    res$best_model,
    c("best_model_M", "best_model_F", "best_model_combined")
  )
  
  expect_s3_class(res$table$df, "data.frame")
  expect_s3_class(res$table_sep$df, "data.frame")
  expect_s3_class(res$table_comb$df, "data.frame")
})