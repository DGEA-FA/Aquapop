# tests/testthat/test-psd_q.R

test_that("psd_q() retourne les bons éléments avec des données valides", {
  # Données simulées réparties dans plusieurs classes
  set.seed(123)
  data_ex <- data.frame(
    ltm = c(
      runif(10, 50, 99),     # Classe stock
      runif(15, 100, 199),   # Qualité
      runif(10, 200, 299)    # Trophy
    ),
    sp = "SAFO"
  )
  
  res <- psd_q(data_ex)
  
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  
  expect_s3_class(res$data, "data.frame")
  expect_true(all(c("Q", "IC 95%") %in% colnames(res$data)))
  
  expect_s3_class(res$flextable, "flextable")
})



test_that("psd_q() échoue si plusieurs espèces", {
  data_multi <- data.frame(
    ltm = runif(20, 100, 250),
    sp = rep(c("SAFO", "SAVI"), each = 10)
  )
  expect_error(psd_q(data_multi), "filtrées pour une seule espèce")
})

test_that("psd_q() échoue si ltm ou sp manquants", {
  expect_error(psd_q(data.frame(sp = "SAFO")), "doit contenir les colonnes `ltm` et `sp`")
  expect_error(psd_q(data.frame(ltm = 200)), "doit contenir les colonnes `ltm` et `sp`")
})

test_that("psd_q() échoue si espèce non supportée", {
  data_nok <- data.frame(
    ltm = runif(20, 100, 300),
    sp = "INCONNU"
  )
  expect_error(psd_q(data_nok), "Espèce non supportée")
})
