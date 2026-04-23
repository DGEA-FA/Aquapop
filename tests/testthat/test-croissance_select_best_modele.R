test_that("sélectionne le modèle convergé avec le plus bas aicc", {
  
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c("120.5", "115.2", "118.9"),
    convergence = c(TRUE, TRUE, TRUE)
  )
  
  best <- croissance_select_best_modele(df)
  
  expect_equal(best, "Gompertz")
  
})

test_that("retourne le premier en cas d'ex-aequo sur l'aicc", {
  
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c("115.2", "115.2", "118.9"),
    convergence = c(TRUE, TRUE, TRUE)
  )
  
  best <- croissance_select_best_modele(df)
  
  expect_equal(best, "Von Bertalanffy")
  
})

test_that("ignore les modèles non convergés", {
  
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c("110.0", "115.2", "-"),
    convergence = c(FALSE, TRUE, FALSE)
  )
  
  best <- croissance_select_best_modele(df)
  
  expect_equal(best, "Gompertz")
  
})

test_that("retourne NA avec un warning si les colonnes requises sont absentes", {
  
  df_invalide <- tibble::tibble(
    model = c("Von Bertalanffy", "Gompertz"),
    AIC = c(120, 115)
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df_invalide),
    regexp = "Aucun modèle n'a pu être sélectionné."
  )
  
  expect_true(is.na(best))
  
})

test_that("retourne NA avec un warning si le tableau est vide", {
  
  df_vide <- tibble::tibble(
    methode = character(),
    aicc = character(),
    convergence = logical()
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df_vide),
    regexp = "Aucun modèle n'a pu être sélectionné."
  )
  
  expect_true(is.na(best))
  
})

test_that("retourne NA avec un warning si tablemodele est NULL", {
  
  expect_warning(
    best <- croissance_select_best_modele(NULL),
    regexp = "Aucun modèle n'a pu être sélectionné."
  )
  
  expect_true(is.na(best))
  
})

test_that("retourne NA avec un warning si aucun modèle convergé n'est disponible", {
  
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    aicc = c("-", "-", "-"),
    convergence = c(FALSE, FALSE, FALSE)
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df),
    regexp = "Aucun modèle n'a pu être sélectionné."
  )
  
  expect_true(is.na(best))
  
})

test_that("retourne NA si aucun aicc valide n'est disponible parmi les modèles convergés", {
  
  df <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz"),
    aicc = c("-", "-"),
    convergence = c(TRUE, TRUE)
  )
  
  expect_warning(
    best <- croissance_select_best_modele(df),
    regexp = "Aucun modèle n'a pu être sélectionné."
  )
  
  expect_true(is.na(best))
  
})