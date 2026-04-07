test_that("mortalite_plot_modele retourne un ggplot valide avec données simulées", {
  specimen <- tibble::tibble(
    sp = "TEST",
    age = sample(0:10, size = 200, replace = TRUE)
  )
  
  modele <- glm(age ~ 1, data = specimen, family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  p <- mortalite_plot_modele(specimen, modele, info_modele)
  
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomBar"))
  expect_true(inherits(p$layers[[2]]$geom, "GeomLine"))
  expect_true(grepl("A = 42 %", p$labels$subtitle))
})

test_that("mortalite_plot_modele retourne NULL si aucun âge valide n'est présent", {
  specimen <- tibble::tibble(
    sp = "TEST",
    age = NA_real_
  )
  
  modele <- glm(age ~ 1, data = tibble::tibble(age = 0:1, sp = "TEST"), family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  p <- mortalite_plot_modele(specimen, modele, info_modele)
  
  expect_null(p)
})

test_that("mortalite_plot_modele retourne NULL si la colonne sp est absente", {
  modele <- glm(age ~ 1, data = tibble::tibble(age = 0:1, sp = "TEST"), family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  df <- tibble::tibble(age = 1:10)
  
  expect_null(mortalite_plot_modele(df, modele, info_modele))
})

test_that("mortalite_plot_modele retourne NULL si la colonne age est absente", {
  modele <- glm(age ~ 1, data = tibble::tibble(age = 0:1, sp = "TEST"), family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  df <- tibble::tibble(sp = "TEST")
  
  expect_null(mortalite_plot_modele(df, modele, info_modele))
})

test_that("mortalite_plot_modele retourne NULL si specimen est vide", {
  specimen <- tibble::tibble(
    sp = character(0),
    age = numeric(0)
  )
  
  modele <- glm(age ~ 1, data = tibble::tibble(age = 0:1, sp = "TEST"), family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  expect_null(mortalite_plot_modele(specimen, modele, info_modele))
})

test_that("mortalite_plot_modele retourne NULL si modele est NULL", {
  specimen <- tibble::tibble(
    sp = "TEST",
    age = sample(0:10, size = 50, replace = TRUE)
  )
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  expect_null(mortalite_plot_modele(specimen, NULL, info_modele))
})

test_that("mortalite_plot_modele retourne NULL si plusieurs espèces sont présentes", {
  specimen <- tibble::tibble(
    sp = c("TEST1", "TEST2", "TEST1", "TEST2"),
    age = c(1, 2, 3, 4)
  )
  
  modele <- glm(age ~ 1, data = tibble::tibble(age = 0:1, sp = "TEST"), family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    methode = "poisson",
    A = 42,
    ic95 = "[36-48]"
  )
  
  expect_null(mortalite_plot_modele(specimen, modele, info_modele))
})