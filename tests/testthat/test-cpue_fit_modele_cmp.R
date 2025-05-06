test_that("cmp - bon ajustement (valeurs variées)", {
  df <- tibble::tibble(no_station = paste0("st", 1:10), CPUE = 1:10)
  res <- cpue_fit_modele_cmp(df)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_named(res, c("methode", "ajustement_hnp", "aicc", "cpue_moyenne",
                      "ic_95", "commentaire", "convergence", "nb_iterations_hnp"))
  expect_equal(res$methode, "cmp")
  expect_true(res$ajustement_hnp >= 0 && res$ajustement_hnp <= 100)
  expect_type(res$convergence, "logical")
  expect_equal(res$cpue_moyenne, round(res$cpue_moyenne, 2))
  expect_type(res$nb_iterations_hnp, "double")
})

test_that("cmp - mauvais ajustement (valeurs bruitées)", {
  set.seed(101)
  df <- tibble::tibble(no_station = paste0("st", 1:10), CPUE = c(0, 0, 10, 20, 0, 15, 0, 30, 5, 0))
  res <- cpue_fit_modele_cmp(df)
  expect_true(res$ajustement_hnp >= 15)
  expect_match(res$commentaire, "Mauvais ajustement")
})
