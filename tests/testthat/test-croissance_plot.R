test_that("croissance_plot() fonctionne pour les trois modèles", {
  # --- Données simulées ---
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    l_inf = c(200, 200, 200),
    k = c(0.2, 0.3, 0.4),
    t0 = c(-0.5, 0.0, 0.5)
  )
  
  # --- Boucle sur les 3 modèles ---
  for (mod in model_table$methode) {
    g <- croissance_plot(specimen_data, model_table, mod)
    
    # Vérifie que le retour est un ggplot
    expect_s3_class(g, "ggplot")
    
    # Vérifie que le caption contient les bons paramètres
    caption_text <- g$labels$caption
    params <- dplyr::filter(model_table, methode == mod)
    expect_true(grepl(mod, caption_text))
    expect_true(grepl(round(params$l_inf, 2), caption_text))
    expect_true(grepl(round(params$k, 3), caption_text))
    expect_true(grepl(round(params$t0, 3), caption_text))
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
    l_inf = 200,
    k = 0.2,
    t0 = -0.5
  )
  
  g <- croissance_plot(data_na, model_table, "Von Bertalanffy")
  expect_s3_class(g, "ggplot")
})

test_that("croissance_plot() déclenche une erreur pour un modèle inconnu", {
  specimen_data <- tibble::tibble(
    no_specimen = 1:5,
    age = c(1, 2, 3, 4, 5),
    ltm = c(100, 140, 160, 180, 190)
  )
  
  model_table <- tibble::tibble(
    methode = "Von Bertalanffy",
    l_inf = 200,
    k = 0.2,
    t0 = -0.5
  )
  
  expect_error(
    croissance_plot(specimen_data, model_table, "Inconnu"),
    "Le modèle doit être l’un de"
  )
})

test_that("croissance_plot() fonctionne avec peu d'individus", {
  small_data <- tibble::tibble(
    no_specimen = 1:6,
    age = 1:6,
    ltm = c(100, 120, 130, 140, 150, 160)
  )
  
  model_table <- tibble::tibble(
    methode = "Gompertz",
    l_inf = 180,
    k = 0.25,
    t0 = 0.1
  )
  
  g <- croissance_plot(small_data, model_table, "Gompertz")
  expect_s3_class(g, "ggplot")
})
