test_that("maturite_fit_combined_modele retourne une liste de 12 modèles glm pour ltm", {
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
  
  models <- maturite_fit_combined_modele(df, variable = "ltm")
  
  expect_type(models, "list")
  expect_length(models, 12)
  expect_named(models, c(
    "TLO_logit", "TLO_probit", "TLO_cloglog",
    "ADD_logit", "ADD_probit", "ADD_cloglog",
    "INT_logit", "INT_probit", "INT_cloglog",
    "COM_logit", "COM_probit", "COM_cloglog"
  ))
  
  lapply(models, expect_s3_class, class = "glm")
  
  expect_equal(as.character(stats::formula(models$TLO_logit))[3], "ltm")
  expect_equal(as.character(stats::formula(models$ADD_logit))[3], "ltm + sexe")
  expect_equal(as.character(stats::formula(models$INT_logit))[3], "ltm * sexe")
  expect_equal(as.character(stats::formula(models$COM_logit))[3], "ltm + sexe + ltm:sexe")
  
  expect_equal(models$TLO_logit$family$link, "logit")
  expect_equal(models$TLO_probit$family$link, "probit")
  expect_equal(models$TLO_cloglog$family$link, "cloglog")
})

test_that("maturite_fit_combined_modele retourne une liste de 12 modèles glm pour age", {
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
  
  models <- maturite_fit_combined_modele(df, variable = "age")
  
  expect_type(models, "list")
  expect_length(models, 12)
  expect_named(models, c(
    "TLO_logit", "TLO_probit", "TLO_cloglog",
    "ADD_logit", "ADD_probit", "ADD_cloglog",
    "INT_logit", "INT_probit", "INT_cloglog",
    "COM_logit", "COM_probit", "COM_cloglog"
  ))
  
  lapply(models, expect_s3_class, class = "glm")
  
  expect_equal(as.character(stats::formula(models$TLO_logit))[3], "age")
  expect_equal(as.character(stats::formula(models$ADD_logit))[3], "age + sexe")
  expect_equal(as.character(stats::formula(models$INT_logit))[3], "age * sexe")
  expect_equal(as.character(stats::formula(models$COM_logit))[3], "age + sexe + age:sexe")
})

test_that("maturite_fit_combined_modele déclenche une erreur si colonnes obligatoires manquantes", {
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
    maturite_fit_combined_modele(df1, variable = "ltm"),
    "maturite"
  )
  
  expect_error(
    maturite_fit_combined_modele(df2, variable = "ltm"),
    "sexe"
  )
  
  expect_error(
    maturite_fit_combined_modele(df3, variable = "ltm"),
    "ltm"
  )
})

test_that("maturite_fit_combined_modele retourne une liste vide avec warning si un seul sexe est observé", {
  df <- tibble::tibble(
    maturite = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
    sexe = factor(rep("F", 10), levels = c("M", "F")),
    ltm = seq(100, 190, by = 10)
  )
  
  expect_warning(
    res <- maturite_fit_combined_modele(df, variable = "ltm"),
    "un seul sexe"
  )
  
  expect_type(res, "list")
  expect_length(res, 0)
})

test_that("maturite_fit_combined_modele accepte sexe comme caractère si M et F sont présents", {
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
  
  models <- maturite_fit_combined_modele(df, variable = "ltm")
  
  expect_type(models, "list")
  expect_length(models, 12)
  lapply(models, expect_s3_class, class = "glm")
})

test_that("maturite_fit_combined_modele ajuste les modèles sur toutes les lignes du jeu de données", {
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
  
  models <- maturite_fit_combined_modele(df, variable = "ltm")
  
  expect_equal(nrow(models$TLO_logit$model), 20)
  expect_equal(nrow(models$ADD_logit$model), 20)
  expect_equal(nrow(models$INT_logit$model), 20)
  expect_equal(nrow(models$COM_logit$model), 20)
})