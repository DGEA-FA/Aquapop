test_that("mortalite_get_peak_plus retourne correctement le peak plus sur un jeu simple", {
  df <- data.frame(age = c(1, 2, 2, 3, 3, 3, 4))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_true(res$success)
  expect_null(res$message)
  expect_equal(res$value, 4L)
})

test_that("mortalite_get_peak_plus retient le plus petit âge en cas d'ex aequo", {
  df <- data.frame(age = c(2, 2, 3, 3))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_true(res$success)
  expect_null(res$message)
  expect_equal(res$value, 3L)
})

test_that("mortalite_get_peak_plus ignore les valeurs NA dans la colonne age", {
  df <- data.frame(age = c(1, 2, 2, NA, NA))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_true(res$success)
  expect_null(res$message)
  expect_equal(res$value, 3L)
})

test_that("mortalite_get_peak_plus retourne success = FALSE si tous les âges sont NA", {
  df <- data.frame(age = c(NA, NA, NA))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucun âge valide n'est disponible pour déterminer l'âge Peak Plus."
  )
  expect_null(res$value)
})

test_that("mortalite_get_peak_plus retourne success = FALSE si aucune ligne n'est disponible", {
  df <- data.frame(age = numeric(0))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucun spécimen n'est disponible pour déterminer l'âge Peak Plus."
  )
  expect_null(res$value)
})

test_that("mortalite_get_peak_plus retourne success = FALSE si la colonne age est absente", {
  df <- data.frame(taille = c(1, 2, 3))
  
  res <- mortalite_get_peak_plus(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "La colonne `age` est absente des données."
  )
  expect_null(res$value)
})

test_that("mortalite_get_peak_plus retourne success = FALSE si les données sont invalides", {
  res <- mortalite_get_peak_plus(c(1, 2, 3))
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Les données fournies sont invalides."
  )
  expect_null(res$value)
})