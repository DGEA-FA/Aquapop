test_that("Retourne use_combined = TRUE si aucun modèle valide", {
  df <- tibble::tibble(
    modele_id = c("F_logit", "M_logit"),
    convergence = c(FALSE, FALSE),
    commentaire = c("convergence échouée", "convergence échouée"),
    aicc = c(100, 110)
  )
  
  res <- maturite_select_best_separated_modele(df)
  
  expect_true(res$use_combined)
  expect_null(res$best_model_F)
  expect_null(res$best_model_M)
  expect_match(
    res$message,
    "Aucun modèle valide.*mâles et les femelles"
  )
})

test_that("Retourne use_combined = TRUE si aucun modèle mâle", {
  df <- tibble::tibble(
    modele_id = c("F_logit", "F_cloglog"),
    convergence = c(TRUE, TRUE),
    commentaire = c("Bon ajustement", "Bon ajustement"),
    aicc = c(95, 90)
  )
  
  res <- maturite_select_best_separated_modele(df)
  
  expect_true(res$use_combined)
  expect_null(res$best_model_M)
  expect_null(res$best_model_F)
  expect_match(
    res$message,
    "Aucun modèle valide.*mâles"
  )
})

test_that("Retourne use_combined = TRUE si aucun modèle femelle", {
  df <- tibble::tibble(
    modele_id = c("M_logit", "M_cloglog"),
    convergence = c(TRUE, TRUE),
    commentaire = c("Bon ajustement", "Bon ajustement"),
    aicc = c(95, 90)
  )
  
  res <- maturite_select_best_separated_modele(df)
  
  expect_true(res$use_combined)
  expect_null(res$best_model_F)
  expect_null(res$best_model_M)
  expect_match(
    res$message,
    "Aucun modèle valide.*femelles"
  )
})

test_that("Retourne les meilleurs modèles M et F avec use_combined = FALSE", {
  df <- tibble::tibble(
    modele_id = c("F_logit", "F_cloglog", "M_logit", "M_probit"),
    convergence = c(TRUE, TRUE, TRUE, TRUE),
    commentaire = c("Bon ajustement", "Bon ajustement", "Bon ajustement", "Bon ajustement"),
    aicc = c(92, 85, 101, 98)
  )
  
  res <- maturite_select_best_separated_modele(df)
  
  expect_false(res$use_combined)
  expect_equal(res$best_model_F, "F_cloglog")
  expect_equal(res$best_model_M, "M_probit")
  expect_match(res$message, "Modèle sélectionné pour les mâles : M_probit")
  expect_match(res$message, "Modèle sélectionné pour les femelles : F_cloglog")
})