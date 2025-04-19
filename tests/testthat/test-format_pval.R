# tests/testthat/test-format_pval.R

test_that("format_pval() gère les cas typiques correctement", {
  
  # Cas 1 : NA
  expect_identical(format_pval(NA), NA_character_)
  
  # Cas 2 : p < 0.001
  expect_identical(format_pval(0.0005), "< 0.001")
  
  # Cas 3 : p = 0.001
  expect_identical(format_pval(0.001), "0.001")
  
  # Cas 4 : p = 0.0099 (arrondi)
  expect_identical(format_pval(0.0099), "0.010")
  
  # Cas 5 : p = 0.01549 (arrondi)
  expect_identical(format_pval(0.01549), "0.015")
  
  # Cas 6 : p = 1
  expect_identical(format_pval(1), "1.000")
  
  # Cas 7 : vecteur mixte
  expect_identical(
    format_pval(c(0.0002, 0.001, 0.345, NA)),
    c("< 0.001", "0.001", "0.345", NA_character_)
  )
  
  # Vérifie que le retour est bien un vecteur de caractères
  res <- format_pval(c(0.02, 0.5, NA))
  expect_type(res, "character")
})
