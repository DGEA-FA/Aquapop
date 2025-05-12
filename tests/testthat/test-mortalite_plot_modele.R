test_that("Retourne un ggplot valide avec données simulées (Poisson)", {
  specimen <- tibble::tibble(
    sp = "TEST",
    age = sample(0:10, size = 200, replace = TRUE)
  )
  
  # définir la couleur manquante si pas déjà dans l’environnement
  couleur_default <- "grey30"
  
  modele <- glm(age ~ 1, data = specimen, family = poisson())
  attr(modele, "methode") <- "poisson"
  
  info_modele <- tibble::tibble(
    Méthode = "poisson",
    A = 42,
    `IC 95%` = "36–48"
  )
  
  p <- mortalite_plot_modele(specimen, modele, info_modele)
  
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomBar"))
  expect_true(inherits(p$layers[[2]]$geom, "GeomLine"))
  expect_true(grepl("A = 42 %", p$labels$subtitle))
})


test_that("Retourne un ggplot vide mais valide si aucun âge présent", {
  specimen <- tibble::tibble(sp = "TEST", age = NA)
  modele <- glm(age ~ 1, data = tibble(age = 0:1, sp = "TEST"), family = poisson())
  info_modele <- tibble::tibble(Méthode = "poisson", A = 42, `IC 95%` = "36–48")
  
  expect_warning({
    p <- mortalite_plot_modele(specimen, modele, info_modele)
  })
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 0)  # aucun tracé
})

test_that("Erreur si colonne 'age' ou 'sp' est manquante", {
  modele <- glm(age ~ 1, data = tibble(age = 0:1, sp = "TEST"), family = poisson())
  info_modele <- tibble::tibble(Méthode = "poisson", A = 42, `IC 95%` = "36–48")
  
  df1 <- tibble::tibble(age = 1:10)    # manque 'sp'
  df2 <- tibble::tibble(sp = "TEST")   # manque 'age'
  
  expect_error(mortalite_plot_modele(df1, modele, info_modele), "sp")
  expect_error(mortalite_plot_modele(df2, modele, info_modele), "age")
})

