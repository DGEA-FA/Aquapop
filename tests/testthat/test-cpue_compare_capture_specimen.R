test_that("cpue_compare_capture_specimen retourne une liste structurée", {
  capture <- data.frame(
    no_station = c(1, 2, 3),
    nb_capture = c(2, 1, 0)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  
  expect_true(res$success)
  expect_type(res$message, "character")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
})

test_that("cpue_compare_capture_specimen calcule correctement les écarts sans différence", {
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(2, 1)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  expect_equal(nrow(res$data), 2)
  expect_equal(res$data$nb_capture_recolte, c(2, 1))
  expect_equal(res$data$nb_specimens, c(2, 1))
  expect_equal(res$data$ecart, c(0, 0))
  expect_true(all(res$data$interpretation == "Aucun écart"))
  
  expect_match(res$message, "Aucun écart détecté")
})

test_that("cpue_compare_capture_specimen détecte les captures supérieures aux spécimens", {
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(3, 2)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  station_1 <- res$data[res$data$no_station == 1, ]
  
  expect_equal(station_1$nb_capture_recolte, 3)
  expect_equal(station_1$nb_specimens, 2)
  expect_equal(station_1$ecart, -1)
  expect_equal(
    station_1$interpretation,
    "Capture supérieure aux spécimens"
  )
  
  expect_match(res$message, "Des écarts sont présents")
})

test_that("cpue_compare_capture_specimen détecte les spécimens supérieurs aux captures", {
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(1, 1)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  station_1 <- res$data[res$data$no_station == 1, ]
  
  expect_equal(station_1$nb_capture_recolte, 1)
  expect_equal(station_1$nb_specimens, 2)
  expect_equal(station_1$ecart, 1)
  expect_equal(
    station_1$interpretation,
    "Spécimens supérieurs à la capture"
  )
  
  expect_match(res$message, "Des écarts sont présents")
})

test_that("cpue_compare_capture_specimen conserve les stations présentes seulement dans capture", {
  capture <- data.frame(
    no_station = c(1, 2, 3),
    nb_capture = c(2, 1, 4)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  station_3 <- res$data[res$data$no_station == 3, ]
  
  expect_equal(station_3$nb_capture_recolte, 4)
  expect_equal(station_3$nb_specimens, 0)
  expect_equal(station_3$ecart, -4)
  expect_equal(
    station_3$interpretation,
    "Capture supérieure aux spécimens"
  )
})

test_that("cpue_compare_capture_specimen conserve les stations présentes seulement dans specimen", {
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(1, 1)
  )
  
  specimen <- data.frame(
    no_station = c(1, 3, 3)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  station_3 <- res$data[res$data$no_station == 3, ]
  
  expect_equal(station_3$nb_capture_recolte, 0)
  expect_equal(station_3$nb_specimens, 2)
  expect_equal(station_3$ecart, 2)
  expect_equal(
    station_3$interpretation,
    "Spécimens supérieurs à la capture"
  )
})

test_that("cpue_compare_capture_specimen additionne les captures répétées par station", {
  capture <- data.frame(
    no_station = c(1, 1, 2),
    nb_capture = c(1, 2, 1)
  )
  
  specimen <- data.frame(
    no_station = c(1, 1, 1, 2)
  )
  
  res <- cpue_compare_capture_specimen(
    capture = capture,
    specimen = specimen
  )
  
  station_1 <- res$data[res$data$no_station == 1, ]
  
  expect_equal(station_1$nb_capture_recolte, 3)
  expect_equal(station_1$nb_specimens, 3)
  expect_equal(station_1$ecart, 0)
  expect_equal(station_1$interpretation, "Aucun écart")
})

test_that("cpue_compare_capture_specimen déclenche une erreur si capture est invalide", {
  specimen <- data.frame(
    no_station = c(1, 1, 2)
  )
  
  expect_error(
    cpue_compare_capture_specimen(
      capture = "pas un data.frame",
      specimen = specimen
    )
  )
})

test_that("cpue_compare_capture_specimen déclenche une erreur si specimen est invalide", {
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(1, 1)
  )
  
  expect_error(
    cpue_compare_capture_specimen(
      capture = capture,
      specimen = "pas un data.frame"
    )
  )
})

test_that("cpue_compare_capture_specimen déclenche une erreur si une colonne requise est absente", {
  capture_sans_nb_capture <- data.frame(
    no_station = c(1, 2)
  )
  
  capture <- data.frame(
    no_station = c(1, 2),
    nb_capture = c(1, 1)
  )
  
  specimen_sans_no_station <- data.frame(
    id = c(1, 2, 3)
  )
  
  expect_error(
    cpue_compare_capture_specimen(
      capture = capture_sans_nb_capture,
      specimen = data.frame(no_station = c(1, 2))
    )
  )
  
  expect_error(
    cpue_compare_capture_specimen(
      capture = capture,
      specimen = specimen_sans_no_station
    )
  )
})