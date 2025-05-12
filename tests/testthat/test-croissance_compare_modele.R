# Jeu de données simulé avec croissance crédible
df_valid <- tibble::tibble(
  no_specimen = 1:60,
  sp = "TEST",
  age = rep(1:6, each = 10),
  ltm = c(
    rnorm(10, 100, 5),
    rnorm(10, 140, 5),
    rnorm(10, 170, 5),
    rnorm(10, 190, 5),
    rnorm(10, 210, 5),
    rnorm(10, 220, 5)
  )
)

test_that("croissance_compare_modele retourne un data.frame avec les bonnes colonnes", {
  res <- croissance_compare_modele(df_valid, format = "data.frame")
  
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  
  df_out <- res$data
  expect_s3_class(df_out, "data.frame")
  expect_true(all(c(
    "methode", "l_inf", "l_inf_ic", "k", "k_ic",
    "t0", "t0_ic", "AICc", "Delta_AICc", "AICcWt", "converged"
  ) %in% names(df_out)))
})

test_that("le résultat contient exactement trois modèles", {
  df_out <- croissance_compare_modele(df_valid, format = "data.frame")$data
  
  expect_equal(nrow(df_out), 3)
  expect_setequal(df_out$methode, c("Von Bertalanffy", "Gompertz", "Logistique"))
})

test_that("format = 'flextable' retourne un objet flextable", {
  res <- croissance_compare_modele(df_valid, format = "flextable")
  
  expect_s3_class(res$flextable, "flextable")
})

test_that("échoue proprement si données insuffisantes", {
  df_bad <- tibble::tibble(
    no_specimen = 1:6,
    sp = "TEST",
    age = 1:6,
    ltm = rep(150, 6)
  )
  
  expect_error(
    croissance_compare_modele(df_bad, format = "data.frame")
  )
})
