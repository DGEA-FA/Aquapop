test_that("maturite_fit_separated_modele retourne une liste de 6 modèles glm pour ltm", {
  df <- tibble::tibble(
    maturite = c(
      0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1
    ),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    ltm = c(
      seq(100, 190, by = 10),
      seq(110, 200, by = 10)
    )
  )
  
  models <- maturite_fit_separated_modele(df, variable = "ltm")
  
  expect_type(models, "list")
  expect_named(
    models,
    c("M_logit", "M_probit", "M_cloglog", "F_logit", "F_probit", "F_cloglog")
  )
  
  lapply(models, expect_s3_class, class = "glm")
  
  expect_equal(as.character(stats::formula(models$M_logit))[3], "ltm")
  expect_equal(models$M_logit$family$link, "logit")
  expect_equal(models$M_probit$family$link, "probit")
  expect_equal(models$M_cloglog$family$link, "cloglog")
  
  expect_equal(as.character(stats::formula(models$F_logit))[3], "ltm")
  expect_equal(models$F_logit$family$link, "logit")
  expect_equal(models$F_probit$family$link, "probit")
  expect_equal(models$F_cloglog$family$link, "cloglog")
})

test_that("maturite_fit_separated_modele retourne une liste de 6 modèles glm pour age", {
  df <- tibble::tibble(
    maturite = c(
      0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1
    ),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    age = c(
      1:10,
      1:10
    )
  )
  
  models <- maturite_fit_separated_modele(df, variable = "age")
  
  expect_type(models, "list")
  expect_named(
    models,
    c("M_logit", "M_probit", "M_cloglog", "F_logit", "F_probit", "F_cloglog")
  )
  
  lapply(models, expect_s3_class, class = "glm")
  
  expect_equal(as.character(stats::formula(models$M_logit))[3], "age")
  expect_equal(as.character(stats::formula(models$F_logit))[3], "age")
})

test_that("maturite_fit_separated_modele déclenche une erreur si colonnes obligatoires manquantes", {
  df1 <- tibble::tibble(
    sexe = factor("M"),
    ltm = 100
  )
  
  df2 <- tibble::tibble(
    maturite = 1,
    ltm = 100
  )
  
  df3 <- tibble::tibble(
    maturite = 1,
    sexe = factor("M")
  )
  
  expect_error(
    maturite_fit_separated_modele(df1, variable = "ltm"),
    "maturite"
  )
  
  expect_error(
    maturite_fit_separated_modele(df2, variable = "ltm"),
    "sexe"
  )
  
  expect_error(
    maturite_fit_separated_modele(df3, variable = "ltm"),
    "ltm"
  )
})

test_that("maturite_fit_separated_modele retourne NULL avec warning si un seul sexe est observé", {
  df <- tibble::tibble(
    maturite = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
    sexe = factor(rep("F", 10), levels = c("M", "F")),
    ltm = seq(100, 190, by = 10)
  )
  
  expect_warning(
    res <- maturite_fit_separated_modele(df, variable = "ltm"),
    "Un seul sexe observé"
  )
  
  expect_null(res)
})

test_that("maturite_fit_separated_modele accepte sexe comme caractère si M et F sont présents", {
  df <- tibble::tibble(
    maturite = c(
      0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1
    ),
    sexe = rep(c("M", "F"), each = 10),
    ltm = c(
      seq(100, 190, by = 10),
      seq(110, 200, by = 10)
    )
  )
  
  models <- maturite_fit_separated_modele(df, variable = "ltm")
  
  expect_type(models, "list")
  expect_length(models, 6)
  lapply(models, expect_s3_class, class = "glm")
})

test_that("maturite_fit_separated_modele utilise bien uniquement les données du sexe correspondant", {
  df <- tibble::tibble(
    maturite = c(
      0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1
    ),
    sexe = factor(rep(c("M", "F"), each = 10), levels = c("M", "F")),
    ltm = c(
      seq(100, 190, by = 10),
      seq(210, 300, by = 10)
    )
  )
  
  models <- maturite_fit_separated_modele(df, variable = "ltm")
  
  expect_true(all(models$M_logit$model$sexe == "M"))
  expect_true(all(models$F_logit$model$sexe == "F"))
  
  expect_equal(nrow(models$M_logit$model), 10)
  expect_equal(nrow(models$F_logit$model), 10)
})