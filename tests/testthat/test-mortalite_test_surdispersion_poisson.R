test_that("mortalite_test_surdispersion_poisson retourne une liste structurée complète", {
  df <- data.frame(
    age = 2:6,
    number = c(10, 15, 20, 13, 12)
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "dispersion", "plot"))
  expect_true(res$success)
  expect_type(res$message, "character")
  expect_type(res$dispersion, "double")
  expect_s3_class(res$plot, "ggplot")
})

test_that("mortalite_test_surdispersion_poisson retourne un message cohérent si la dispersion est faible", {
  set.seed(42)
  df <- data.frame(
    age = rep(2:6, each = 10),
    number = rpois(50, lambda = 5)
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_true(res$success)
  expect_true(res$dispersion < 1.5)
  expect_match(res$message, "Aucune sur-dispersion majeure")
})

test_that("mortalite_test_surdispersion_poisson retourne un message cohérent si la dispersion est élevée", {
  set.seed(123)
  df <- data.frame(
    age = rep(2:6, each = 10),
    number = rnbinom(50, mu = 5, size = 1)
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_true(res$success)
  expect_true(res$dispersion > 1.5)
  expect_match(res$message, "présentent une sur-dispersion")
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si la colonne age est absente", {
  df <- data.frame(number = c(10, 15, 20))
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "dispersion", "plot"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le tableau doit contenir les colonnes `age` et `number`."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si la colonne number est absente", {
  df <- data.frame(age = c(2, 3, 4))
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "dispersion", "plot"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le tableau doit contenir les colonnes `age` et `number`."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si data est vide", {
  df <- data.frame(age = numeric(0), number = numeric(0))
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucune donnée n'est disponible pour tester la sur-dispersion."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si moins de deux classes valides sont disponibles", {
  df <- data.frame(age = c(2, NA), number = c(10, NA))
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le test de sur-dispersion requiert au moins deux classes d'âge valides."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si un seul âge distinct est disponible", {
  df <- data.frame(age = c(2, 2, 2), number = c(10, 15, 20))
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le test de sur-dispersion requiert au moins deux âges distincts."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})

test_that("mortalite_test_surdispersion_poisson retourne success = FALSE si data n'est pas un data.frame", {
  res <- mortalite_test_surdispersion_poisson(c(1, 2, 3))
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Les données fournies sont invalides."
  )
  expect_null(res$dispersion)
  expect_null(res$plot)
})