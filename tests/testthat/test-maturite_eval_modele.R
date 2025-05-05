test_that("maturite_eval_modele fonctionne avec modèles L50 et A50 valides", {
  set.seed(123)
  
  # Données simulées avec suffisamment d'observations
  df <- data.frame(
    ltm = sample(120:350, 100, replace = TRUE),
    age = sample(2:7, 100, replace = TRUE),
    sexe = rep(c("F", "M"), each = 50),
    maturite = rbinom(100, 1, 0.5)
  )
  
  df <- df |>
    dplyr::mutate(
      maturite = factor(ifelse(maturite == 1, "O", "N"), levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  # Ajustement de 4 modèles simples (ltm/age x logit/probit)
  mod_ltm_logit   <- glm(maturite ~ ltm, family = binomial("logit"), data = df)
  mod_ltm_probit  <- glm(maturite ~ ltm, family = binomial("probit"), data = df)
  mod_age_logit   <- glm(maturite ~ age, family = binomial("logit"), data = df)
  mod_age_probit  <- glm(maturite ~ age, family = binomial("probit"), data = df)
  
  models <- list(
    ltm_logit = mod_ltm_logit,
    ltm_probit = mod_ltm_probit,
    age_logit = mod_age_logit,
    age_probit = mod_age_probit
  )
  
  res <- maturite_eval_modele(models)
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "modele_id", "modele", "lien", "convergence",
    "pearson_x2_pval", "goodness_of_link_pval", "aicc", "commentaire"
  ) %in% names(res)))
  
  expect_true(all(res$convergence %in% c(TRUE, FALSE)))
  expect_true(all(res$lien %in% c("logit", "probit")))
  expect_type(res$aicc, "double")
  expect_type(res$commentaire, "character")
})

test_that("maturite_eval_modele gère les modèles NULL", {
  models <- list(
    null_model = NULL
  )
  
  res <- maturite_eval_modele(models)
  expect_equal(nrow(res), 1)
  expect_false(res$convergence)
  expect_true(is.character(res$commentaire))
  expect_equal(res$commentaire[[1]], "Données insuffisantes")
  })
