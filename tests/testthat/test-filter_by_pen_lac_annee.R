test_that("filter_by_pen_lac_annee() retourne toutes les données si aucun filtre", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    no_lac = c("00001", "00002", "00003"),
    annee = c(2021, 2022, 2023)
  )
  
  result <- filter_by_pen_lac_annee(data)
  expect_equal(nrow(result), 3)
  expect_s3_class(result, "data.frame")
})

test_that("filtrage par typ_pech fonctionne avec une seule valeur", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENT", "PENOF", "PENT"),
    no_lac = c("00001", "00001", "00002", "00003"),
    annee = c(2021, 2022, 2022, 2021)
  )
  
  result <- filter_by_pen_lac_annee(data, typ_pech = "PENT")
  expect_equal(nrow(result), 3)
  expect_true(all(result$typ_pech == "PENT"))
})

test_that("filtrage par no_lac fonctionne", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF"),
    no_lac = c("00001", "00002"),
    annee = c(2021, 2022)
  )
  
  result <- filter_by_pen_lac_annee(data, no_lac = "00002")
  expect_equal(nrow(result), 1)
  expect_equal(result$no_lac, "00002")
})

test_that("filtrage par annee fonctionne", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF", "PENT"),
    no_lac = c("00001", "00001", "00002"),
    annee = c(2021, 2022, 2022)
  )
  
  result <- filter_by_pen_lac_annee(data, annee = 2022)
  expect_equal(nrow(result), 2)
  expect_true(all(result$annee == 2022))
})

test_that("filtrage par typ_pech et no_lac fonctionne", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENT", "PENOF", "PENT"),
    no_lac = c("00001", "00001", "00002", "00003"),
    annee = c(2021, 2021, 2022, 2023)
  )
  
  result <- filter_by_pen_lac_annee(data, typ_pech = "PENT", no_lac = "00001")
  expect_equal(nrow(result), 2)
  expect_true(all(result$typ_pech == "PENT"))
  expect_true(all(result$no_lac == "00001"))
})

test_that("erreur si typ_pech contient plusieurs valeurs", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    no_lac = c("00001", "00002", "00003"),
    annee = c(2021, 2022, 2023)
  )
  
  expect_error(
    filter_by_pen_lac_annee(data, typ_pech = c("PENT", "PENOF")),
    "doit contenir une seule valeur"
  )
})

test_that("retourne un tableau vide si aucune ligne ne correspond", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF"),
    no_lac = c("00001", "00002"),
    annee = c(2021, 2022)
  )
  
  result <- filter_by_pen_lac_annee(data, typ_pech = "PENDJ")
  expect_equal(nrow(result), 0)
})

test_that("la fonction accepte character, numeric et factor si convertis en character", {
  data <- tibble::tibble(
    typ_pech = c("PENT", "PENOF"),
    no_lac = c("00001", "00002"),
    annee = c(2021, 2022)
  )
  
  r1 <- filter_by_pen_lac_annee(data, no_lac = "00001")
  r2 <- filter_by_pen_lac_annee(data, annee = 2021)
  r3 <- filter_by_pen_lac_annee(data, typ_pech = "PENT")
  
  expect_equal(nrow(r1), 1)
  expect_equal(nrow(r2), 1)
  expect_equal(nrow(r3), 1)
})

