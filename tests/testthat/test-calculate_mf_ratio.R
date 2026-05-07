# test-calculate_mf_ratio.R
# Tests unitaires pour la fonction calculate_mf_ratio()
# AquaPop – Vérifie la simplification des ratios M:F

test_that("calculate_mf_ratio() retourne le ratio simplifié correctement", {
  expect_equal(calculate_mf_ratio(6, 4), "3:2")
  expect_equal(calculate_mf_ratio(5, 5), "1:1")
  expect_equal(calculate_mf_ratio(2, 8), "1:4")
  expect_equal(calculate_mf_ratio(8, 2), "4:1")
})

test_that("calculate_mf_ratio() retourne NA_character_ si au moins une valeur est nulle", {
  expect_identical(calculate_mf_ratio(0, 7), NA_character_)
  expect_identical(calculate_mf_ratio(9, 0), NA_character_)
  expect_identical(calculate_mf_ratio(0, 0), NA_character_)
})

test_that("calculate_mf_ratio() retourne NA_character_ si au moins une valeur est manquante", {
  expect_identical(calculate_mf_ratio(NA, 7), NA_character_)
  expect_identical(calculate_mf_ratio(9, NA), NA_character_)
  expect_identical(calculate_mf_ratio(NA, NA), NA_character_)
})

test_that("calculate_mf_ratio() retourne toujours un caractère avec un seul ':' lorsque calculable", {
  res <- calculate_mf_ratio(4, 2)
  
  expect_type(res, "character")
  expect_match(res, "^[0-9]+:[0-9]+$")
})