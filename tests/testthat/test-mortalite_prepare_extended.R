test_that("mortalite_prepare_extended fonctionne dans le cas nominal", {
  df <- data.frame(age = 2:5, number = c(4, 3, 2, 1))
  age_max <- 5
  
  res <- mortalite_prepare_extended(df, age_max)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data"))
  expect_true(res$success)
  expect_null(res$message)
  expect_s3_class(res$data, "data.frame")
  
  # Vérifie que les âges fictifs ont été ajoutés
  expected_ages <- 2:(age_max * 3)
  expect_equal(res$data$age, expected_ages)
  
  # Vérifie que les âges fictifs ont number = 0
  fictifs <- res$data[res$data$age > age_max, ]
  expect_true(all(fictifs$number == 0))
  
  # Vérifie que les valeurs d'origine ont été conservées
  originaux <- res$data[res$data$age <= age_max, ]
  expect_equal(originaux$number, c(4, 3, 2, 1))
})

test_that("mortalite_prepare_extended retourne success = FALSE si les colonnes sont manquantes", {
  df_invalide <- data.frame(age = 1:3)
  
  res <- mortalite_prepare_extended(df_invalide, age_max = 3)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Le tableau `df_corrigee` doit contenir les colonnes `age` et `number`."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended retourne success = FALSE si age_max est NA", {
  df <- data.frame(age = 1:3, number = c(2, 2, 1))
  
  res <- mortalite_prepare_extended(df, age_max = NA)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "L'âge maximal (`age_max`) est invalide ou manquant."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended retourne success = FALSE si age_max est négatif", {
  df <- data.frame(age = 1:3, number = c(2, 2, 1))
  
  res <- mortalite_prepare_extended(df, age_max = -1)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "L'âge maximal (`age_max`) doit être un nombre supérieur ou égal à 0."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended retourne success = FALSE si age_max a une longueur > 1", {
  df <- data.frame(age = 1:3, number = c(2, 2, 1))
  
  res <- mortalite_prepare_extended(df, age_max = c(3, 4))
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "L'âge maximal (`age_max`) est invalide ou manquant."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended retourne success = FALSE si df_corrigee est invalide", {
  res <- mortalite_prepare_extended(c(1, 2, 3), age_max = 3)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Les données corrigées sont invalides."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended retourne success = FALSE si df_corrigee est vide", {
  df <- data.frame(age = numeric(0), number = numeric(0))
  
  res <- mortalite_prepare_extended(df, age_max = 3)
  
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucune donnée corrigée n'est disponible pour étendre la structure d'âge."
  )
  expect_null(res$data)
})

test_that("mortalite_prepare_extended n'ajoute pas d'âge fictif si age_max = 0", {
  df <- data.frame(age = 0, number = 5)
  
  res <- mortalite_prepare_extended(df, age_max = 0)
  
  expect_true(res$success)
  expect_null(res$message)
  expect_equal(res$data, df[order(df$age), , drop = FALSE])
})