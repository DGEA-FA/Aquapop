test_that("La fonction retourne une liste avec dispersion, message et plot", {
  df <- data.frame(
    age = 2:6,
    number = c(10, 15, 20, 13, 12)
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_type(res, "list")
  expect_named(res, c("dispersion", "message", "plot"))
  expect_type(res$dispersion, "double")
  expect_type(res$message, "character")
  expect_s3_class(res$plot, "gg")
})

test_that("Valeur de dispersion faible → message cohérent", {
  set.seed(42)
  df <- data.frame(
    age = rep(2:6, each = 10),
    number = rpois(50, lambda = 5)
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_true(res$dispersion < 1.5)
  expect_match(res$message, "Aucune sur-dispersion majeure")
})

test_that("Valeur de dispersion élevée → message cohérent", {
  set.seed(123)
  df <- data.frame(
    age = rep(2:6, each = 10),
    number = rnbinom(50, mu = 5, size = 1)  # sur-dispersion simulée
  )
  
  res <- mortalite_test_surdispersion_poisson(df)
  
  expect_true(res$dispersion > 1.5)
  expect_match(res$message, "présentent une sur-dispersion")
})

test_that("Erreur explicite si colonne age ou number absente", {
  df1 <- data.frame(number = c(10, 15, 20))
  df2 <- data.frame(age = c(2, 3, 4))
  
  expect_error(mortalite_test_surdispersion_poisson(df1), "age")
  expect_error(mortalite_test_surdispersion_poisson(df2), "number")
})
