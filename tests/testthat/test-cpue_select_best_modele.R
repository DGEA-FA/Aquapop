test_that("sélectionne le meilleur modèle parmi les modèles bien ajustés", {
  df <- tibble::tibble(
    Méthode = c("poisson", "nb1", "nb2", "cmp", "gp"),
    `Ajustement (résultat du test HNP)` = c(5, 8, 12, 15, 20),
    AICc = c(120, 110, 115, 130, 125)
  )
  
  best <- cpue_select_best_modele(df)
  expect_equal(best, "nb1")
})

test_that("sélectionne le meilleur modèle global si aucun bien ajusté", {
  df <- tibble::tibble(
    Méthode = c("poisson", "nb1", "nb2", "cmp", "gp"),
    `Ajustement (résultat du test HNP)` = c(12, 15, 18, 20, 22),
    AICc = c(125, 118, 130, 140, 135)
  )
  
  best <- cpue_select_best_modele(df)
  expect_equal(best, "nb1")
})

test_that("retourne une erreur si colonnes absentes", {
  df_invalide <- tibble::tibble(
    Methode = c("poisson", "nb1"),  # faute de frappe
    AIC = c(100, 90)
  )
  
  expect_error(
    cpue_select_best_modele(df_invalide),
    regexp = "Le tableau fourni n’est pas valide"
  )
})

test_that("retourne NA et un warning si aucune ligne exploitable", {
  df_vide <- tibble::tibble(
    Méthode = character(),
    `Ajustement (résultat du test HNP)` = numeric(),
    AICc = numeric()
  )
  
  expect_warning({
    best <- cpue_select_best_modele(df_vide)
    expect_true(is.na(best))
  })
})

test_that("retourne le premier modèle en cas d’ex-aequo", {
  df <- tibble::tibble(
    Méthode = c("nb1", "nb2"),
    `Ajustement (résultat du test HNP)` = c(5, 5),
    AICc = c(100, 100)
  )
  
  best <- cpue_select_best_modele(df)
  expect_equal(best, "nb1")  # le premier ex-aequo
})

