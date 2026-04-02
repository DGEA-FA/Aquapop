test_that("mortalite_get_age_max retourne le bon âge max", {
  df <- data.frame(age = c(1, 3, 2, NA, 5))
  expect_equal(mortalite_get_age_max(df), 5L)
})

test_that("mortalite_get_age_max retourne NA si aucun âge valide", {
  df <- data.frame(age = c(NA, NA, NA))
  expect_equal(mortalite_get_age_max(df), NA_integer_)
})

test_that("mortalite_get_age_max retourne NA si data.frame vide", {
  df <- data.frame(age = numeric(0))
  expect_equal(mortalite_get_age_max(df), NA_integer_)
})

test_that("mortalite_get_age_max échoue si colonne age absente", {
  df <- data.frame(other = 1:5)
  expect_error(mortalite_get_age_max(df), "age")
})

test_that("mortalite_get_age_max échoue si data n'est pas un data.frame", {
  expect_error(mortalite_get_age_max(c(1, 2, 3)), "data.frame")
})
