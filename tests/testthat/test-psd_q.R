# tests/testthat/test-psd_q.R

test_that("psd_q() retourne les bons éléments avec des données valides", {
  set.seed(123)
  
  data_ex <- data.frame(
    ltm = c(
      runif(10, 50, 99),
      runif(15, 100, 199),
      runif(10, 200, 299)
    ),
    sp = "SAFO"
  )
  
  res <- psd_q(data_ex)
  
  expect_type(res, "list")
  expect_named(res, c("success", "data", "flextable", "message"))
  
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$data, "data.frame")
  expect_equal(colnames(res$data), c("Q", "ic95"))
  
  expect_type(res$data$Q, "double")
  expect_type(res$data$ic95, "character")
  
  expect_false(any(is.na(res$data$Q)))
  expect_false(any(is.na(res$data$ic95)))
  
  expect_s3_class(res$flextable, "flextable")
})

test_that("psd_q() conserve Q numérique et formate ic95 en français", {
  set.seed(123)
  
  data_ex <- data.frame(
    ltm = c(
      runif(10, 50, 99),
      runif(15, 100, 199),
      runif(10, 200, 299)
    ),
    sp = "SAFO"
  )
  
  res <- psd_q(data_ex)
  
  expect_type(res$data$Q, "double")
  
  expect_match(
    res$data$ic95,
    "^\\[[0-9]+,[0-9]{1} – [0-9]+,[0-9]{1}\\]$"
  )
})

test_that("psd_q() retourne success = FALSE si les données sont vides", {
  data_vide <- data.frame(
    ltm = numeric(),
    sp = character()
  )
  
  res <- psd_q(data_vide)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("psd_q() retourne success = FALSE si toutes les longueurs sont manquantes", {
  data_na <- data.frame(
    ltm = rep(NA_real_, 10),
    sp = rep("SAFO", 10)
  )
  
  res <- psd_q(data_na)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucune longueur exploitable")
})

test_that("psd_q() retourne success = FALSE si l'espèce n'est pas supportée", {
  data_nok <- data.frame(
    ltm = runif(20, 100, 300),
    sp = "INCONNU"
  )
  
  res <- psd_q(data_nok)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "n'est pas supportée")
})

test_that("psd_q() retourne success = FALSE si aucun spécimen n'atteint le seuil requis", {
  data_sous_seuil <- data.frame(
    ltm = rep(50, 20),
    sp = rep("SAFO", 20)
  )
  
  res <- psd_q(data_sous_seuil)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen n'atteint la longueur minimale")
})

test_that("psd_q() échoue si plusieurs espèces sont présentes", {
  data_multi <- data.frame(
    ltm = runif(20, 100, 250),
    sp = rep(c("SAFO", "SAVI"), each = 10)
  )
  
  expect_error(
    psd_q(data_multi),
    "Les données doivent être filtrées pour une seule espèce"
  )
})

test_that("psd_q() échoue si ltm ou sp sont manquants", {
  expect_error(
    psd_q(data.frame(sp = "SAFO")),
    "Le jeu de données doit contenir les colonnes `ltm` et `sp`"
  )
  
  expect_error(
    psd_q(data.frame(ltm = 200)),
    "Le jeu de données doit contenir les colonnes `ltm` et `sp`"
  )
})