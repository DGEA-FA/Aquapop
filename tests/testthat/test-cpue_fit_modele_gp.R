# test/test-cpue_fit_modele_gp.R

test_that("cpue_fit_modele_gp() fonctionne correctement - cas nominal", {
  set.seed(123)
  cpue_data <- tibble::tibble(
    no_station = 1:10,
    CPUE = rpois(10, lambda = 5)
  )
  
  res <- cpue_fit_modele_gp(cpue_data)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_named(res, c("methode", "ajustement_hnp", "aicc", "cpue_moyenne",
                      "ic_95", "commentaire", "convergence", "nb_iterations_hnp"))
  expect_equal(res$methode, "gp")
  expect_true(is.numeric(res$ajustement_hnp))
  expect_true(res$ajustement_hnp >= 0 && res$ajustement_hnp <= 100)
  expect_match(res$ic_95, "^\\([0-9.]+-[0-9.]+\\)$")
  expect_true(res$convergence %in% c(TRUE, FALSE))
  expect_true(res$nb_iterations_hnp %in% c(2, 5))
  
  # commentaire cohérent
  if (res$ajustement_hnp < 10) {
    expect_match(res$commentaire, "Bon ajustement")
  } else if (res$ajustement_hnp < 15) {
    expect_match(res$commentaire, "Ajustement marginal")
  } else {
    expect_match(res$commentaire, "Mauvais ajustement")
  }
})



