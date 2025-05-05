# test-get_info_pen.R
# Tests unitaires – Fonction get_info_pen()
# AquaPop – Informations biologiques par espèce ou type de pêche

test_that("get_info_pen() retourne une liste valide pour un code d'espèce connu", {
  res <- get_info_pen("SANA")
  expect_type(res, "list")
  expect_named(res, c("code_sp", "nom_sp", "binwidth", "breaks", "break_labels"))
  expect_equal(res$code_sp, "SANA")
  expect_true(is.numeric(res$binwidth))
  expect_type(res$breaks, "double")
  expect_type(res$break_labels, "character")
  expect_equal(length(res$breaks), length(res$break_labels))
})

test_that("get_info_pen() retourne les infos correctes pour un type de pêche valide", {
  res <- get_info_pen("PENT")
  expect_type(res, "list")
  expect_equal(res$code_sp, "SANA")  # Selon le mapping dans la fonction
})

test_that("get_info_pen() retourne NULL pour un code inconnu", {
  expect_null(get_info_pen("XXXX"))
})

