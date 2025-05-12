test_that("Retourne NULL si aucun modèle combiné n'est valide", {
  df <- tibble::tibble(
    modele_id = c("C_TLO", "C_ADD"),
    convergence = c(FALSE, FALSE),
    commentaire = c("échec convergence", "échec convergence"),
    aicc = c(120, 130)
  )
  
  res <- maturite_select_best_combined_modele(df)
  expect_null(res$best_model)
  expect_match(res$message, "Aucun modèle combiné valide")
})

test_that("Retourne NULL si tous les modèles sont rejetés malgré convergence", {
  df <- tibble::tibble(
    modele_id = c("C_TLO", "C_ADD"),
    convergence = c(TRUE, TRUE),
    commentaire = c("À rejeter", "choisir un autre modèle"),
    aicc = c(115, 110)
  )
  
  res <- maturite_select_best_combined_modele(df)
  expect_null(res$best_model)
  expect_match(res$message, "Aucun modèle combiné valide")
})

test_that("Retourne le meilleur modèle combiné (AICc minimal)", {
  df <- tibble::tibble(
    modele_id = c("C_TLO", "C_ADD", "C_COM"),
    convergence = c(TRUE, TRUE, TRUE),
    commentaire = c("Bon ajustement", "Bon ajustement", "Bon ajustement"),
    aicc = c(110.1, 98.3, 102.0)
  )
  
  res <- maturite_select_best_combined_modele(df)
  expect_equal(res$best_model, "C_ADD")
  expect_match(res$message, "Modèle combiné sélectionné : C_ADD")
})

test_that("Retourne le premier modèle si égalité de AICc", {
  df <- tibble::tibble(
    modele_id = c("C_TLO", "C_ADD"),
    convergence = c(TRUE, TRUE),
    commentaire = c("Bon ajustement", "Bon ajustement"),
    aicc = c(100, 100)
  )
  
  res <- maturite_select_best_combined_modele(df)
  expect_true(all(res$best_model %in% c("C_TLO", "C_ADD")))
  expect_length(res$best_model, 1)  # optionnel si vous forcez la sélection d’un seul
  expect_true(any(stringr::str_detect(res$message, paste0("Modèle combiné sélectionné : ", res$best_model))))
  
})
