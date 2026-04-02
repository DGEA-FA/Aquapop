test_that("croissance_plot() fonctionne pour les trois modèles convergés", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:6,
    age = c(1, 2, 3, 4, 5, 6),
    ltm = c(100, 130, 155, 175, 190, 200)
  )
  
  model_table <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    l_inf = c("220", "215", "210"),
    k = c("0.25", "0.30", "0.35"),
    t0 = c("-0.5", "0.0", "0.3"),
    convergence = c("Convergé", "Convergé", "Convergé")
  )
  
  for (mod in model_table$methode) {
    
    g <- croissance_plot(specimen_data, model_table, mod)
    
    expect_s3_class(g, "ggplot")
    
    caption_text <- g$labels$caption
    params <- dplyr::filter(model_table, methode == mod)
    
    expect_true(grepl(mod, caption_text))
    expect_true(grepl(round(as.numeric(params$l_inf), 2), caption_text))
    expect_true(grepl(round(as.numeric(params$k), 3), caption_text))
    expect_true(grepl(round(as.numeric(params$t0), 3), caption_text))
    
  }
  
})

test_that("croissance_plot() filtre les NA correctement", {
  
  data_na <- tibble::tibble(
    no_specimen = 1:6,
    age = c(1, 2, 3, 4, NA, 6),
    ltm = c(100, 120, NA, 160, 170, 180)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "200",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  g <- croissance_plot(data_na, model_table, "Von Bertalanffy")
  
  expect_s3_class(g, "ggplot")
  
})

test_that("croissance_plot() retourne NULL avec un warning pour un modèle inconnu", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "200",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, "Inconnu"),
    regexp = "modèle demandé est invalide"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() fonctionne avec peu d'individus", {
  
  small_data <- tibble::tibble(
    no_specimen = 1:6,
    age = 1:6,
    ltm = c(100, 120, 130, 140, 150, 160)
  )
  
  model_table <- tibble::tibble(
    methode = "Gompertz",
    l_inf = "180",
    k = "0.25",
    t0 = "0.1",
    convergence = "Convergé"
  )
  
  g <- croissance_plot(small_data, model_table, "Gompertz")
  
  expect_s3_class(g, "ggplot")
  
})

test_that("croissance_plot() retourne NULL avec un warning si tablemodele est NULL", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, NULL, "Von Bertalanffy"),
    regexp = "résultats de modèles ne sont pas disponibles"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() retourne NULL avec un warning si modele est NA", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "200",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, NA_character_),
    regexp = "aucun modèle valide n'a été sélectionné"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() retourne NULL avec un warning si le modèle est absent du tableau", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "200",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, "Gompertz"),
    regexp = "modèle sélectionné est absent"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() retourne NULL avec un warning si le modèle n'a pas convergé", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "-",
    k = "-",
    t0 = "-",
    convergence = "Le modèle n'a pas convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, "Von Bertalanffy"),
    regexp = "n'a pas convergé"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() retourne NULL avec un warning si les paramètres du modèle sont invalides", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "-",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, "Von Bertalanffy"),
    regexp = "paramètres du modèle sont invalides"
  )
  
  expect_null(g)
  
})

test_that("croissance_plot() retourne NULL avec un warning si les paramètres du modèle sont manquants après conversion", {
  
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = "abc",
    k = "0.2",
    t0 = "-0.5",
    convergence = "Convergé"
  )
  
  expect_warning(
    g <- croissance_plot(specimen_data, model_table, "Von Bertalanffy"),
    regexp = "paramètres du modèle sont manquants"
  )
  
  expect_null(g)
  
})