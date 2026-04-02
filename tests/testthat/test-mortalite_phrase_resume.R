test_that("mortalite_phrase_resume fonctionne dans les cas standards", {
  df <- data.frame(methode = c("Poisson", "NB1", "NB2"), A = c(45, 37, 52))
  
  expect_equal(
    mortalite_phrase_resume(df, "NB1"),
    "Le modèle NB1 décrit le mieux la mortalité de la population. La mortalité annuelle s'élève à 37 %."
  )
})

test_that("mortalite_phrase_resume retourne une erreur si le modèle est absent", {
  df <- data.frame(methode = c("Poisson", "NB2"), A = c(45, 52))
  
  expect_error(
    mortalite_phrase_resume(df, "NB1"),
    "Modèle NB1 non trouvé"
  )
})

test_that("mortalite_phrase_resume retourne une phrase partielle si A est manquant", {
  df <- data.frame(methode = c("NB1"), A = NA)
  
  expect_match(
    mortalite_phrase_resume(df, "NB1"),
    "la mortalité annuelle n'est pas disponible"
  )
})

test_that("mortalite_phrase_resume gère un modèle vide", {
  df <- data.frame(methode = c("NB1"), A = 37)
  
  expect_error(
    mortalite_phrase_resume(df, ""),
    "modèle est invalide"
  )
})

test_that("mortalite_phrase_resume gère une table vide", {
  df <- data.frame(methode = character(0), A = numeric(0))
  
  expect_error(
    mortalite_phrase_resume(df, "NB1"),
    "Aucune donnée de comparaison disponible"
  )
})
