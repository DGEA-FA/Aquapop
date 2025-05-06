test_that("cpue_fit_modele_poisson() retourne un tableau bien structuré", {
  set.seed(123)
  df <- data.frame(no_station = 1:30, CPUE = rpois(30, lambda = 4))
  result <- cpue_fit_modele_poisson(df)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  
  expect_named(result, c("methode", "ajustement_hnp", "aicc", "cpue_moyenne",
                         "ic_95", "commentaire", "convergence", "nb_iterations_hnp"))
  
  expect_type(result$methode, "character")
  expect_type(result$ajustement_hnp, "double")
  expect_type(result$aicc, "double")
  expect_type(result$cpue_moyenne, "double")
  expect_type(result$ic_95, "character")
  expect_type(result$commentaire, "character")
  expect_type(result$convergence, "logical")
  expect_type(result$nb_iterations_hnp, "double")
  
  expect_equal(result$methode, "poisson")
  expect_true(result$convergence)
})


