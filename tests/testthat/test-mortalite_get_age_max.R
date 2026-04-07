test_that("mortalite_get_age_max retourne le bon âge max dans le cas nominal", {
  df <- data.frame(age = c(1, 3, 2, NA, 5))
  
  res <- mortalite_get_age_max(df)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "value"))
  expect_true(res$success)
  expect_null(res$message)
  expect_equal(res$value, 5)
})

test_that("mortalite_get_age_max retourne success = FALSE si aucun âge valide", {
  df <- data.frame(age = c(NA, NA, NA))
  
  res <- mortalite_get_age_max(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucun âge valide n'est disponible pour déterminer l'âge maximal."
  )
  expect_null(res$value)
})

test_that("mortalite_get_age_max retourne success = FALSE si data.frame vide", {
  df <- data.frame(age = numeric(0))
  
  res <- mortalite_get_age_max(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucun spécimen n'est disponible pour déterminer l'âge maximal."
  )
  expect_null(res$value)
})

test_that("mortalite_get_age_max retourne success = FALSE si colonne age absente", {
  df <- data.frame(other = 1:5)
  
  res <- mortalite_get_age_max(df)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "La colonne `age` est absente des données."
  )
  expect_null(res$value)
})

test_that("mortalite_get_age_max retourne success = FALSE si data invalide", {
  res <- mortalite_get_age_max(c(1, 2, 3))
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Les données fournies sont invalides."
  )
  expect_null(res$value)
})