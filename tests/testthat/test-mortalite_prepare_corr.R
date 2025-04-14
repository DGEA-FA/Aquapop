testthat::skip_if_not_installed("fishmethods")

test_that("mortalite_prepare_corr retourne un data.frame structuré avec colonnes attendues", {
  data <- data.frame(age = c(3, 5, 5, 7))
  res <- mortalite_prepare_corr(data, age_peak_plus = 3, age_max = 7)
  
  expect_s3_class(res, "data.frame")
  expect_named(res, c("age", "number"))
  expect_type(res$age, "double")
  expect_type(res$number, "double")
})

test_that("mortalite_prepare_corr ajoute bien les zéros pour les âges manquants", {
  data <- data.frame(age = c(3, 5, 5, 7)) # âges 4 et 6 manquants
  res <- mortalite_prepare_corr(data, age_peak_plus = 3, age_max = 7)
  
  expect_equal(res$age, 3:7)
  expect_equal(res$number[res$age == 4], 0)
  expect_equal(res$number[res$age == 6], 0)
})

test_that("mortalite_prepare_corr gère les doublons correctement", {
  data <- data.frame(age = c(4, 4, 4, 5, 6))
  res <- mortalite_prepare_corr(data, age_peak_plus = 4, age_max = 6)
  
  expect_equal(res$number[res$age == 4], 3)
  expect_equal(res$number[res$age == 5], 1)
  expect_equal(res$number[res$age == 6], 1)
})

test_that("mortalite_prepare_corr retourne les âges dans l’intervalle [peak_plus, max]", {
  data <- data.frame(age = c(2, 3, 4, 5, 6, 7, 8))
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 8)
  
  expect_true(all(res$age >= 5 & res$age <= 8))
})

test_that("mortalite_prepare_corr échoue si la colonne age est absente", {
  data <- data.frame(taille = c(1, 2, 3))
  expect_error(mortalite_prepare_corr(data, 1, 5),
               regexp = "doit contenir une colonne nommée `age`|must.include")
})

test_that("mortalite_prepare_corr échoue si la colonne age ne contient que des NA", {
  data <- data.frame(age = c(NA, NA, NA))
  expect_error(
    mortalite_prepare_corr(data, age_peak_plus = 2, age_max = 5),
    regexp = "Aucune valeur d’âge valide"
  )
})

test_that("mortalite_prepare_corr échoue si les données sont insuffisantes pour agesurv()", {
  data <- data.frame(age = c(5))
  expect_error(
    mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 6),
    regexp = "trop incomplet"
  )
})

test_that("mortalite_prepare_corr retourne les bonnes fréquences sur un cas simple", {
  data <- data.frame(age = c(5, 5, 6, 7, 7, 7))
  res <- mortalite_prepare_corr(data, age_peak_plus = 5, age_max = 7)
  
  expect_equal(res$number, c(2, 1, 3))
})
