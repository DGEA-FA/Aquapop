test_that("Sélectionne le meilleur modèle avec HNP < 10", {
  tablemodele <- tibble::tibble(
    Méthode = c("Poisson", "NB1", "NB2"),
    AICc = c(120, 110, 105),
    `Ajustement HNP (%)` = c(15, 9, 6)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  expect_equal(res, "NB2")  # Le plus bas AICc parmi HNP < 10
})

test_that("Sélectionne le meilleur modèle global si aucun HNP < 10", {
  tablemodele <- tibble::tibble(
    Méthode = c("Poisson", "NB1", "NB2"),
    AICc = c(120, 110, 105),
    `Ajustement HNP (%)` = c(20, 19, 18)
  )
  
  res <- mortalite_select_best_modele(tablemodele)
  expect_equal(res, "NB2")  # Le plus bas AICc global
})

test_that("Renvoie NA avec avertissement si aucune ligne", {
  tablemodele <- tibble::tibble(
    Méthode = character(0),
    AICc = numeric(0),
    `Ajustement HNP (%)` = numeric(0)
  )
  
  expect_warning(res <- mortalite_select_best_modele(tablemodele))
  expect_true(is.na(res))
})

test_that("Erreur si colonnes manquantes", {
  table_invalide <- tibble::tibble(
    Methode = c("Poisson", "NB1"),
    AICc = c(120, 110)
  )
  
  expect_error(mortalite_select_best_modele(table_invalide))
})
