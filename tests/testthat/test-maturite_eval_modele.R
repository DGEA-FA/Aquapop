test_that("build_individual_model_row retourne une ligne valide pour un glm", {
  set.seed(123)
  
  df <- data.frame(
    ltm = rep(seq(100, 290, by = 10), 2),
    sexe = rep(c("F", "M"), each = 10),
    maturite = c(
      "N", "N", "N", "N", "O", "O", "O", "O", "O", "O",
      "N", "N", "N", "O", "O", "O", "O", "O", "O", "O"
    )
  ) |>
    dplyr::mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  mod <- glm(
    maturite ~ ltm,
    family = binomial(link = "logit"),
    data = df
  )
  
  res <- build_individual_model_row(mod, id = "TLO_logit_ltm")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  
  expect_named(
    res,
    c(
      "modele_id",
      "modele",
      "lien",
      "convergence",
      "pearson_x2_pval",
      "goodness_of_link_pval",
      "aicc",
      "commentaire"
    )
  )
  
  expect_equal(res$modele_id, "TLO_logit_ltm")
  expect_true(is.character(res$modele))
  expect_equal(res$lien, "logit")
  expect_true(is.logical(res$convergence))
  expect_true(is.numeric(res$pearson_x2_pval) || is.na(res$pearson_x2_pval))
  expect_true(is.numeric(res$goodness_of_link_pval) || is.na(res$goodness_of_link_pval))
  expect_true(is.numeric(res$aicc) || is.na(res$aicc))
  expect_true(is.character(res$commentaire))
})

test_that("build_individual_model_row retourne une ligne standardisée si le modèle est NULL", {
  res <- build_individual_model_row(NULL, id = "modele_null")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  
  expect_equal(res$modele_id, "modele_null")
  expect_true(is.na(res$modele))
  expect_true(is.na(res$lien))
  expect_false(res$convergence)
  expect_true(is.na(res$pearson_x2_pval))
  expect_true(is.na(res$goodness_of_link_pval))
  expect_true(is.na(res$aicc))
  expect_equal(res$commentaire, "Données insuffisantes")
})

test_that("maturite_eval_modele retourne un tibble vide bien structuré si la liste est vide", {
  res <- maturite_eval_modele(list())
  
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  
  expect_named(
    res,
    c(
      "modele_id",
      "modele",
      "lien",
      "convergence",
      "pearson_x2_pval",
      "goodness_of_link_pval",
      "aicc",
      "commentaire"
    )
  )
  
  labels <- labelled::var_label(res)
  
  expect_equal(labels$modele_id, "ID")
  expect_equal(labels$modele, "Modèle")
  expect_equal(labels$lien, "Lien")
  expect_equal(labels$convergence, "Convergence")
  expect_equal(labels$pearson_x2_pval, "Goodness-of-fit (p-valeur)")
  expect_equal(labels$goodness_of_link_pval, "Goodness-of-link (p-valeur)")
  expect_equal(labels$aicc, "AICc")
  expect_equal(labels$commentaire, "Commentaires")
})

test_that("maturite_eval_modele compile plusieurs modèles et conserve les labels", {
  set.seed(123)
  
  df <- data.frame(
    ltm = rep(seq(100, 290, by = 10), 2),
    sexe = rep(c("F", "M"), each = 10),
    maturite = c(
      "N", "N", "N", "N", "O", "O", "O", "O", "O", "O",
      "N", "N", "N", "O", "O", "O", "O", "O", "O", "O"
    )
  ) |>
    dplyr::mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  mod_tlo <- glm(
    maturite ~ ltm,
    family = binomial(link = "logit"),
    data = df
  )
  
  mod_add <- glm(
    maturite ~ ltm + sexe,
    family = binomial(link = "probit"),
    data = df
  )
  
  models <- list(
    TLO_logit = mod_tlo,
    ADD_probit = mod_add,
    INT_cloglog = NULL
  )
  
  res <- maturite_eval_modele(models)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  
  expect_named(
    res,
    c(
      "modele_id",
      "modele",
      "lien",
      "convergence",
      "pearson_x2_pval",
      "goodness_of_link_pval",
      "aicc",
      "commentaire"
    )
  )
  
  expect_true(all(c("TLO_logit", "ADD_probit", "INT_cloglog") %in% res$modele_id))
  expect_true(any(res$convergence))
  expect_true(any(!res$convergence))
  
  labels <- labelled::var_label(res)
  expect_equal(labels$modele_id, "ID")
  expect_equal(labels$commentaire, "Commentaires")
})

test_that("maturite_eval_modele trie les résultats par convergence puis AICc", {
  set.seed(123)
  
  df <- data.frame(
    ltm = rep(seq(100, 290, by = 10), 2),
    sexe = rep(c("F", "M"), each = 10),
    maturite = c(
      "N", "N", "N", "N", "O", "O", "O", "O", "O", "O",
      "N", "N", "N", "O", "O", "O", "O", "O", "O", "O"
    )
  ) |>
    dplyr::mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  mod_1 <- glm(
    maturite ~ ltm,
    family = binomial(link = "logit"),
    data = df
  )
  
  mod_2 <- glm(
    maturite ~ ltm + sexe,
    family = binomial(link = "logit"),
    data = df
  )
  
  models <- list(
    modele_1 = mod_1,
    modele_2 = mod_2,
    modele_null = NULL
  )
  
  res <- maturite_eval_modele(models)
  
  expect_false(res$convergence[nrow(res)])
  
  res_converges <- res |>
    dplyr::filter(convergence)
  
  if (nrow(res_converges) >= 2) {
    expect_true(all(diff(res_converges$aicc) >= 0))
  }
})

test_that("build_individual_model_row retourne le bon commentaire si le modèle est forcé à non convergent", {
  set.seed(123)
  
  df <- data.frame(
    ltm = rep(seq(100, 290, by = 10), 2),
    sexe = rep(c("F", "M"), each = 10),
    maturite = c(
      "N", "N", "N", "N", "O", "O", "O", "O", "O", "O",
      "N", "N", "N", "O", "O", "O", "O", "O", "O", "O"
    )
  ) |>
    dplyr::mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  mod <- glm(
    maturite ~ ltm,
    family = binomial(link = "logit"),
    data = df
  )
  
  mod$converged <- FALSE
  
  res <- build_individual_model_row(mod, id = "modele_non_convergent")
  
  expect_false(res$convergence)
  expect_equal(
    res$commentaire,
    "Ce modèle ne converge pas et devrait être rejeté."
  )
})
