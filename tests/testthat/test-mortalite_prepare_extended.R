test_that("mortalite_prepare_extended fonctionne dans le cas nominal", {
  df <- data.frame(age = 2:5, number = c(4, 3, 2, 1))
  age_max <- 5
  
  res <- mortalite_prepare_extended(df, age_max)
  
  # Vérifie que les âges fictifs ont été ajoutés
  expected_ages <- 2:(age_max * 3)
  expect_equal(res$age, expected_ages)
  
  # Vérifie que les âges fictifs ont number = 0
  fictifs <- res[res$age > age_max, ]
  expect_true(all(fictifs$number == 0))
  
  # Vérifie que les valeurs d'origine ont été conservées
  originaux <- res[res$age <= age_max, ]
  expect_equal(originaux$number, c(4, 3, 2, 1))
})

test_that("mortalite_prepare_extended retourne une erreur si colonnes manquantes", {
  df_invalide <- data.frame(age = 1:3)
  expect_error(mortalite_prepare_extended(df_invalide, age_max = 3), "doit contenir les colonnes")
})

test_that("mortalite_prepare_extended retourne une erreur si age_max est invalide", {
  df <- data.frame(age = 1:3, number = c(2, 2, 1))
  
  expect_error(mortalite_prepare_extended(df, age_max = NA), "doit être un nombre numérique positif")
  expect_error(mortalite_prepare_extended(df, age_max = -1), "doit être un nombre numérique positif")
  expect_error(mortalite_prepare_extended(df, age_max = c(3, 4)), "doit être un nombre numérique positif")
})
