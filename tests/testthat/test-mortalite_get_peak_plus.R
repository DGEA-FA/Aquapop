# test-mortalite_get_peak_plus.R

test_that("retourne le peak plus correctement sur un jeu simple", {
  df <- data.frame(age = c(1, 2, 2, 3, 3, 3, 4))
  expect_equal(mortalite_get_peak_plus(df), 4) # mode = 3, donc 3 + 1 = 4
})

test_that("retourne le plus petit âge en cas d'ex aequo", {
  df <- data.frame(age = c(2, 2, 3, 3)) # mode partagé entre 2 et 3
  expect_equal(mortalite_get_peak_plus(df), 3) # plus petit mode = 2 → 2 + 1 = 3
})

test_that("ignore les valeurs NA dans la colonne age", {
  df <- data.frame(age = c(1, 2, 2, NA, NA))
  expect_equal(mortalite_get_peak_plus(df), 3) # mode = 2 → 3
})

test_that("retourne NA si tous les âges sont NA", {
  df <- data.frame(age = c(NA, NA, NA))
  expect_true(is.na(mortalite_get_peak_plus(df)))
})

test_that("retourne NA si aucune ligne", {
  df <- data.frame(age = numeric(0))
  expect_true(is.na(mortalite_get_peak_plus(df)))
})

test_that("génère une erreur si la colonne age est absente", {
  df <- data.frame(taille = c(1, 2, 3))
  expect_error(mortalite_get_peak_plus(df), "colonne `age` est manquante")
})
