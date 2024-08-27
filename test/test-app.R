# /test/test-app.R

library(shiny)
library(shinytest)
library(testthat)

app <- ShinyDriver$new("C:/Users/bruca03/OneDrive - Ministère de l'Environnement et la Lutte contre les changements climatiques/Documents/DynapopQC/app.R")

# Tester le chargement des données et vérification des messages
test_that("Les messages d'erreur pour les dataframes sont affichés correctement", {
  # Simuler le chargement des données vides
  app$setInputs(data_station = data.frame())
  app$setInputs(data_recolte = data.frame())
  app$setInputs(data_specimen = data.frame())
  app$setInputs(data_lac = data.frame())
  app$setInputs(data_parametres = data.frame())
  app$setInputs(data_profil = data.frame())
  
  # Vérifier les messages affichés
  expect_equal(app$getElementText("status_text_data_station"), "Stations est vide.")
  expect_equal(app$getElementText("status_text_data_recolte"), "Récolte est vide.")
  expect_equal(app$getElementText("status_text_data_specimen"), "Spécimen est vide.")
  expect_equal(app$getElementText("status_text_data_lac"), "Lac est vide.")
  expect_equal(app$getElementText("status_text_data_parametres"), "Paramètres est vide.")
  expect_equal(app$getElementText("status_text_data_profil"), "Profil est vide.")
  
  # Simuler le chargement des données non vides
  app$setInputs(data_station = data.frame(x = 1:3))
  app$setInputs(data_recolte = data.frame(x = 1:3))
  app$setInputs(data_specimen = data.frame(x = 1:3))
  app$setInputs(data_lac = data.frame(x = 1:3))
  app$setInputs(data_parametres = data.frame(x = 1:3))
  app$setInputs(data_profil = data.frame(x = 1:3))
  
  # Vérifier que les messages ne sont pas affichés pour les données non vides
  expect_null(app$getElementText("status_text_data_station"))
  expect_null(app$getElementText("status_text_data_recolte"))
  expect_null(app$getElementText("status_text_data_specimen"))
  expect_null(app$getElementText("status_text_data_lac"))
  expect_null(app$getElementText("status_text_data_parametres"))
  expect_null(app$getElementText("status_text_data_profil"))
})

# Tester les doublons
test_that("Les messages de doublons sont affichés correctement", {
  # Simuler le chargement des données avec doublons
  df_with_doublons <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  app$setInputs(data_station = df_with_doublons)
  app$setInputs(data_recolte = df_with_doublons)
  
  # Vérifier les messages de doublons
  expect_equal(app$getElementText("doublons_data_station"), "Doublons trouvés dans Stations")
  expect_equal(app$getElementText("doublons_data_recolte"), "Doublons trouvés dans Récolte")
  
  # Simuler le chargement des données sans doublons
  df_without_doublons <- data.frame(a = c(1, 2), b = c("x", "y"))
  app$setInputs(data_station = df_without_doublons)
  app$setInputs(data_recolte = df_without_doublons)
  
  # Vérifier que les messages de doublons ne sont pas affichés
  expect_null(app$getElementText("doublons_data_station"))
  expect_null(app$getElementText("doublons_data_recolte"))
})
