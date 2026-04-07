test_that("mortalite_select_best_modele sélectionne le meilleur modèle avec HNP < 10", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1", "NB2"),
    aicc = c(120, 110, 105),
    ajustement_hnp = c(15, 9, 6)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_equal(res, "NB2")
})

test_that("mortalite_select_best_modele sélectionne le meilleur modèle global si aucun HNP < 10", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1", "NB2"),
    aicc = c(120, 110, 105),
    ajustement_hnp = c(20, 19, 18)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_equal(res, "NB2")
})

test_that("mortalite_select_best_modele retourne NULL si aucune ligne", {
  tablemodele <- tibble::tibble(
    methode = character(0),
    aicc = numeric(0),
    ajustement_hnp = numeric(0)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_null(res)
})

test_that("mortalite_select_best_modele retourne NULL si les colonnes requises sont absentes", {
  table_invalide <- tibble::tibble(
    Methode = c("Poisson", "NB1"),
    aicc = c(120, 110)
  )
  
  res <- mortalite_select_best_modele(table_invalide)
  
  expect_null(res)
})

test_that("mortalite_select_best_modele ignore les modèles sans AICc", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1", "NB2"),
    aicc = c(NA, 110, 105),
    ajustement_hnp = c(5, 5, 12)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_equal(res, "NB1")
})

test_that("mortalite_select_best_modele retourne NULL si tous les AICc sont manquants", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1", "NB2"),
    aicc = c(NA, NA, NA),
    ajustement_hnp = c(5, 7, 9)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_null(res)
})

test_that("mortalite_select_best_modele filtre sur convergence si la colonne est présente", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1", "NB2"),
    aicc = c(100, 90, 80),
    ajustement_hnp = c(5, 5, 5),
    convergence = c(TRUE, FALSE, FALSE)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_equal(res, "Poisson")
})

test_that("mortalite_select_best_modele retourne NULL si aucun modèle convergent n'est disponible", {
  tablemodele <- tibble::tibble(
    methode = c("Poisson", "NB1"),
    aicc = c(100, 90),
    ajustement_hnp = c(5, 4),
    convergence = c(FALSE, FALSE)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  
  expect_null(res)
})