test_that("mortalite_fit_best_modele retourne un modèle explicite selon la methode fournie", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  
  df <- tibble::tibble(
    age = 1:6,
    number = c(200, 130, 80, 50, 25, 10)
  )
  
  mod_poisson <- mortalite_fit_best_modele(df, methode = "poisson")
  expect_s3_class(mod_poisson, "glm")
  
  mod_nb2 <- mortalite_fit_best_modele(df, methode = "nb2")
  expect_s3_class(mod_nb2, "negbin")
  
  mod_nb1 <- mortalite_fit_best_modele(df, methode = "nb1")
  expect_s3_class(mod_nb1, "glmmTMB")
  
  mod_gp <- mortalite_fit_best_modele(df, methode = "gp")
  expect_s3_class(mod_gp, "glmmTMB")
  
  mod_cmp <- mortalite_fit_best_modele(df, methode = "cmp")
  expect_s3_class(mod_cmp, "glmmTMB")
})

test_that("mortalite_fit_best_modele retourne automatiquement le meilleur modèle si methode = NULL", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  skip_if_not_installed("hnp")
  skip_if_not_installed("flextable")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(200, 140, 90, 60, 40, 25, 12, 5)
  )
  
  model <- suppressWarnings(
    suppressMessages(
      mortalite_fit_best_modele(df)
    )
  )
  
  expect_false(is.null(model))
  expect_true(
    inherits(model, "glm") ||
      inherits(model, "negbin") ||
      inherits(model, "glmmTMB")
  )
})

test_that("mortalite_fit_best_modele retourne NULL pour une methode invalide", {
  df <- tibble::tibble(
    age = 1:6,
    number = c(100, 80, 60, 40, 20, 10)
  )
  
  res <- mortalite_fit_best_modele(df, methode = "toto")
  
  expect_null(res)
})

test_that("mortalite_fit_best_modele retourne NULL si les donnees sont vides", {
  df <- tibble::tibble(
    age = numeric(),
    number = numeric()
  )
  
  res <- mortalite_fit_best_modele(df)
  
  expect_null(res)
})

test_that("mortalite_fit_best_modele retourne NULL si la colonne age est absente", {
  df <- tibble::tibble(
    number = c(100, 80, 60)
  )
  
  res <- mortalite_fit_best_modele(df)
  
  expect_null(res)
})

test_that("mortalite_fit_best_modele retourne NULL si la colonne number est absente", {
  df <- tibble::tibble(
    age = 1:3
  )
  
  res <- mortalite_fit_best_modele(df)
  
  expect_null(res)
})