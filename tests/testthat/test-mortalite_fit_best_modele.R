test_that("Retourne un modèle glm/glm.nb/glmmTMB explicite selon methode fournie", {
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

test_that("Retourne le modèle automatiquement sélectionné si methode = NULL", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  
  df <- tibble::tibble(
    age = 1:8,
    number = c(200, 140, 90, 60, 40, 25, 12, 5)
  )
  
  model <- suppressWarnings(suppressMessages(mortalite_fit_best_modele(df)))
  expect_true(inherits(model, "glm") || inherits(model, "glm.nb") || inherits(model, "glmmTMB"))
})

test_that("Déclenche une erreur pour une méthode invalide", {
  df <- tibble::tibble(
    age = 1:6,
    number = c(100, 80, 60, 40, 20, 10)
  )
  expect_error(mortalite_fit_best_modele(df, methode = "toto"))
})
