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

test_that("retourne une erreur si le tableau ne contient pas les bonnes colonnes", {
  df_invalide <- tibble::tibble(
    model = c("Von Bertalanffy", "Gompertz"),
    AIC = c(120, 115)
  )
  
  expect_error(
    croissance_select_best_modele(df_invalide),
    regexp = "n’est pas valide"
  )
})

test_that("retourne NA avec un warning si aucun modèle ne peut être sélectionné", {
  df_vide <- tibble::tibble(
    methode = character(),
    aicc = numeric()
  )
  
  expect_warning({
    best <- croissance_select_best_modele(df_vide)
    expect_true(is.na(best))
  })
})
