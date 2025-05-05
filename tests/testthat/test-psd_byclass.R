# test/testthat/test-psd_byclass.R

test_that("psd_byclass retourne les bons éléments de sortie", {
  data_ex <- tibble::tibble(
    sp = rep("SANA", 100),
    ltm = sample(100:1000, 100, replace = TRUE)
  )
  result <- psd_byclass(data_ex)
  
  expect_type(result, "list")
  expect_named(result, c("data", "flextable", "plot"))
})

test_that("Les fréquences sont cohérentes et bien arrondies", {
  data_ex <- tibble::tibble(
    sp = rep("SANA", 120),
    ltm = sample(c(100, 200, 300, 400, 500, 600, 700, 800), 120, replace = TRUE)
  )
  result <- psd_byclass(data_ex)$data
  
  expect_true(all(result$freq >= 0))
  expect_true(sum(as.numeric(result$freq)) <= 100 + 1)  # arrondis
})

test_that("Les classes manquantes apparaissent avec des fréquences nulles", {
  data_ex <- tibble::tibble(
    sp = rep("SANA", 20),
    ltm = rep(120, 20) # une seule classe
  )
  result <- psd_byclass(data_ex)$data
  
  expect_true(any(result$freq == 0))  # au moins une classe à 0
  expect_equal(length(result$classe), length(psd_classnames))
})

test_that("Les noms de classes et les intervalles sont bien alignés", {
  data_ex <- tibble::tibble(
    sp = rep("SANA", 60),
    ltm = sample(200:700, 60, replace = TRUE)
  )
  result <- psd_byclass(data_ex)$data
  
  expect_true(all(result$classe %in% psd_classnames))
  expect_equal(nrow(result), length(psd_classnames))
})

test_that("Erreur si colonne ltm ou sp absente", {
  data_bad1 <- tibble::tibble(sp = rep("SANA", 10))
  data_bad2 <- tibble::tibble(ltm = rep(100, 10))
  
  expect_error(psd_byclass(data_bad1))
  expect_error(psd_byclass(data_bad2), "Les données doivent être filtrées pour une seule espèce.")
})

test_that("Erreur si plusieurs espèces présentes", {
  data_multi <- tibble::tibble(
    sp = rep(c("SANA", "MAME"), each = 10),
    ltm = rep(200, 20)
  )
  
  expect_error(psd_byclass(data_multi), "données doivent être filtrées pour une seule espèce")
})
