test_that("cpue_fit_modele_nb2() retourne un tableau bien structuré", {
  set.seed(123)
  df <- data.frame(no_station = 1:30, cpue = rnbinom(30, mu = 4, size = 2))
  result <- cpue_fit_modele_nb2(df)
  
  # Vérifie que la sortie est un data.frame d'une ligne
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  
  # Vérifie la présence de toutes les colonnes attendues
  expect_named(result, c("methode", "ajustement_hnp", "aicc", "cpue_moyenne",
                         "ic_95", "commentaire", "convergence", "nb_iterations_hnp"))
  
  # Vérifie les types des colonnes clés
  expect_type(result$methode, "character")
  expect_type(result$ajustement_hnp, "double")
  expect_type(result$aicc, "double")
  expect_type(result$cpue_moyenne, "double")
  expect_type(result$ic_95, "character")
  expect_type(result$commentaire, "character")
  expect_type(result$convergence, "logical")
  expect_type(result$nb_iterations_hnp, "double")
  
  # Vérifie que la méthode est bien "nb2" et convergence à TRUE
  expect_equal(result$methode, "nb2")
  expect_true(result$convergence)
})

