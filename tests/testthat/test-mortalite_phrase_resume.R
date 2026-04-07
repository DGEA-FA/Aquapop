test_that("mortalite_phrase_resume fonctionne dans les cas standards", {
  df <- data.frame(
    methode = c("Poisson", "NB1", "NB2"),
    A = c(45, 37, 52)
  )
  
  expect_equal(
    mortalite_phrase_resume(df, "NB1"),
    "Le modèle NB1 décrit le mieux la mortalité de la population. La mortalité annuelle s'élève à 37 %."
  )
})

test_that("mortalite_phrase_resume retourne NULL si le modèle est absent", {
  df <- data.frame(
    methode = c("Poisson", "NB2"),
    A = c(45, 52)
  )
  
  expect_null(mortalite_phrase_resume(df, "NB1"))
})

test_that("mortalite_phrase_resume retourne une phrase partielle si A est manquant", {
  df <- data.frame(
    methode = c("NB1"),
    A = NA
  )
  
  expect_equal(
    mortalite_phrase_resume(df, "NB1"),
    "Le modèle NB1 a été sélectionné, mais la mortalité annuelle n'est pas disponible."
  )
})

test_that("mortalite_phrase_resume retourne NULL si le nom du modèle est vide", {
  df <- data.frame(
    methode = c("NB1"),
    A = 37
  )
  
  expect_null(mortalite_phrase_resume(df, ""))
})

test_that("mortalite_phrase_resume retourne NULL si la table est vide", {
  df <- data.frame(
    methode = character(0),
    A = numeric(0)
  )
  
  expect_null(mortalite_phrase_resume(df, "NB1"))
})

test_that("mortalite_phrase_resume retourne NULL si data_comparaison n'est pas un data.frame", {
  expect_null(mortalite_phrase_resume(c("NB1", "NB2"), "NB1"))
})

test_that("mortalite_phrase_resume retourne NULL si la colonne methode est absente", {
  df <- data.frame(
    A = c(37, 45)
  )
  
  expect_null(mortalite_phrase_resume(df, "NB1"))
})

test_that("mortalite_phrase_resume retourne NULL si modele_nom est NA", {
  df <- data.frame(
    methode = c("NB1"),
    A = 37
  )
  
  expect_null(mortalite_phrase_resume(df, NA_character_))
})