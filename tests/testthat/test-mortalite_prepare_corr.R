testthat::skip_if_not_installed("fishmethods")

test_that("mortalite_prepare_corr retourne une liste structurée avec data dans le cas nominal", {
  data <- data.frame(age = c(3, 5, 5, 7))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 3, age_max = 7)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data"))
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$data, "data.frame")
  expect_named(res$data, c("age", "number"))
  expect_type(res$data$age, "double")
  expect_true(is.numeric(res$data$number))
})

test_that("mortalite_prepare_corr ajoute bien les zéros pour les âges manquants", {
  data <- data.frame(age = c(3, 5, 5, 7))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 3, age_max = 7)
  
  expect_true(res$success)
  expect_equal(res$data$age, 3:7)
  expect_equal(res$data$number[res$data$age == 4], 0L)
  expect_equal(res$data$number[res$data$age == 6], 0L)
})

test_that("mortalite_prepare_corr gère les doublons correctement", {
  data <- data.frame(age = c(4, 4, 4, 5, 6))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 4, age_max = 6)
  
  expect_true(res$success)
  expect_equal(res$data$number[res$data$age == 4], 3)
  expect_equal(res$data$number[res$data$age == 5], 1)
  expect_equal(res$data$number[res$data$age == 6], 1)
})

test_that("mortalite_prepare_corr retourne les âges dans l'intervalle attendu", {
  data <- data.frame(age = c(2, 3, 4, 5, 6, 7, 8))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 8)
  
  expect_true(res$success)
  expect_true(all(res$data$age >= min(res$data$age)))
  expect_true(all(res$data$age <= max(res$data$age)))
})

test_that("mortalite_prepare_corr retourne success = FALSE si la colonne age est absente", {
  data <- data.frame(taille = c(1, 2, 3))
  
  res <- mortalite_prepare_corr(data, 1, 5)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "La colonne `age` est absente des données."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si la colonne age ne contient que des NA", {
  data <- data.frame(age = c(NA, NA, NA))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 2, age_max = 5)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucune valeur d'âge valide n'est disponible après suppression des NA."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si les données sont insuffisantes pour agesurv()", {
  data <- data.frame(age = c(5))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 6)
  
  expect_false(res$success)
  expect_match(res$message, "trop incomplet")
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne les bonnes fréquences sur un cas simple", {
  data <- data.frame(age = c(5, 5, 6, 7, 7, 7))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 7)
  
  expect_true(res$success)
  expect_equal(res$data$number, c(2, 1, 3))
})

test_that("mortalite_prepare_corr retourne success = FALSE si data est invalide", {
  res <- mortalite_prepare_corr(c(1, 2, 3), age_peak_plus = 1, age_max = 5)
  
  expect_false(res$success)
  expect_equal(res$message, "Les données fournies sont invalides.")
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si age_peak_plus est invalide", {
  data <- data.frame(age = c(2, 3, 4))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = NA, age_max = 5)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "L'âge de départ (`age_peak_plus`) est invalide ou manquant."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si age_max est invalide", {
  data <- data.frame(age = c(2, 3, 4))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 2, age_max = NA)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "L'âge maximal (`age_max`) est invalide ou manquant."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si age_peak_plus >= age_max", {
  data <- data.frame(age = c(2, 3, 4, 5))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 5)
  
  expect_false(res$success)
  expect_match(
    res$message,
    "strictement inférieur à l'âge maximal"
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_corr retourne success = FALSE si age_peak_plus dépasse l'âge max observé", {
  data <- data.frame(age = c(2, 3, 4))
  
  res <- mortalite_prepare_corr(data, age_peak_plus = 10, age_max = 12)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "`age_peak_plus` est supérieur à l'âge maximum observé dans les données."
  )
  expect_null(res$data)
})