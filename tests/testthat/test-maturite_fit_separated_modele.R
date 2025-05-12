test_that("Retourne une liste de 6 modèles glm pour ltm", {
  df <- tibble::tibble(
    maturite = rep(0:1, each = 10),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    ltm = rep(seq(100, 200, length.out = 10), 2)
  )
  
  models <- maturite_fit_separated_modele(df, variable = "ltm")
  
  expect_type(models, "list")
  expect_named(models, c("M_logit", "M_probit", "M_cloglog", "F_logit", "F_probit", "F_cloglog"))
  lapply(models, function(m) expect_s3_class(m, "glm"))
})

test_that("Retourne une liste de 6 modèles glm pour age", {
  df <- tibble::tibble(
    maturite = rep(0:1, each = 10),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    age = rep(1:10, 2)
  )
  
  models <- maturite_fit_separated_modele(df, variable = "age")
  
  expect_type(models, "list")
  expect_named(models, c("M_logit", "M_probit", "M_cloglog", "F_logit", "F_probit", "F_cloglog"))
  lapply(models, function(m) expect_s3_class(m, "glm"))
})

test_that("Erreur si colonnes obligatoires manquantes", {
  df1 <- tibble::tibble(sexe = factor("M"), ltm = 100)
  df2 <- tibble::tibble(maturite = 1, ltm = 100)
  df3 <- tibble::tibble(maturite = 1, sexe = factor("M"))
  
  expect_error(maturite_fit_separated_modele(df1, "ltm"), "maturite")
  expect_error(maturite_fit_separated_modele(df2, "ltm"), "sexe")
  expect_error(maturite_fit_separated_modele(df3, "ltm"), "ltm")
})
