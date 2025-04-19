test_that("get_info_pen() retourne une liste pour un type de pêche valide", {
  resultat <- get_info_pen("PENT")
  expect_type(resultat, "list")
  expect_equal(resultat$code_sp, "SANA")
})

test_that("get_info_pen() retourne une liste pour un code espèce valide", {
  resultat <- get_info_pen("SAFO")
  expect_type(resultat, "list")
  expect_equal(resultat$code_sp, "SAFO")
})

test_that("get_info_pen() retourne NULL si l'entrée est invalide", {
  expect_null(get_info_pen("XYZ"))
  expect_null(get_info_pen("ESPECE_FAUSSE"))
})
