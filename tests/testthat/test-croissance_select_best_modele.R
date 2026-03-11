test_that("sélectionne le modèle avec le plus bas aicc", {
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c(120.5, 115.2, 118.9)
  )
  
  best <- croissance_select_best_modele(df)
  
  expect_equal(best, "Gompertz")
})


test_that("retourne le premier en cas d'ex-aequo sur l'aicc", {
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c(115.2, 115.2, 118.9)
  )
  
  best <- croissance_select_best_modele(df)
  
  expect_equal(best, "Von Bertalanffy")
})


test_that("retourne NA avec un warning si les colonnes requises sont absentes", {
  df_invalide <- tibble::tibble(
    model = c("Von Bertalanffy", "Gompertz"),
    AIC = c(120, 115)
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df_invalide),
    regexp = "colonnes requises"
  )
  
  expect_true(is.na(best))
})


test_that("retourne NA avec un warning si le tableau est vide", {
  df_vide <- tibble::tibble(
    methode = character(),
    aicc = numeric()
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df_vide),
    regexp = "vide ou invalide"
  )
  
  expect_true(is.na(best))
})


test_that("retourne NA avec un warning si tablemodele est NULL", {
  expect_warning(
    best <- croissance_select_best_modele(NULL),
    regexp = "résultats de croissance"
  )
  
  expect_true(is.na(best))
})