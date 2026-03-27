# =============================================================================
# Tests pour structure_age()
# =============================================================================

specimen_age_test <- tibble::tibble(
  sp = rep("SANA", 10),
  age = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6),
  sexe = rep(c("F", "M"), each = 5),
  maturite = rep(c("O", "N"), times = 5),
  marquage = rep("NMA", 10)
)

test_that("structure_age() fonctionne avec un jeu de données fictif structuré", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data <- specimen_age_test
  
  expect_true(all(c("sp", "age", "sexe", "maturite", "marquage") %in% names(data)))
  expect_equal(unique(data$sp), "SANA")
  
  res <- structure_age(data, groupement = "sexe")
  
  expect_type(res, "list")
  expect_named(res, c("success", "plot", "data", "flextable", "message"))
  
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  expect_gt(nrow(res$data), 0)
})

test_that("structure_age() retourne success = FALSE si aucun spécimen n'est disponible", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_vide <- specimen_age_test[0, ]
  
  res <- structure_age(data_vide, groupement = "sexe")
  
  expect_type(res, "list")
  expect_named(res, c("success", "plot", "data", "flextable", "message"))
  
  expect_false(res$success)
  expect_null(res$plot)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("structure_age() retourne success = FALSE si tous les âges sont manquants", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_sans_age <- specimen_age_test |>
    dplyr::mutate(age = NA_real_)
  
  res <- structure_age(data_sans_age, groupement = "sexe")
  
  expect_false(res$success)
  expect_null(res$plot)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("structure_age() échoue si plusieurs espèces sont présentes", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_multi_especes <- specimen_age_test |>
    dplyr::mutate(
      sp = c(rep("SANA", 5), rep("SAFO", 5))
    )
  
  expect_error(
    structure_age(data_multi_especes, groupement = "sexe"),
    regexp = "une seule espèce"
  )
})

test_that("structure_age() échoue si la colonne de groupement est manquante", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  df <- data.frame(
    sp = "SANA",
    age = c(1, 2, 3)
  )
  
  expect_error(
    structure_age(df, groupement = "maturite"),
    regexp = "La colonne correspondant au groupement 'maturite' est manquante"
  )
})

test_that("structure_age() échoue si le groupement est invalide", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  expect_error(
    structure_age(specimen_age_test, groupement = "bidon"),
    regexp = "Groupement non reconnu|Must be TRUE"
  )
})

test_that("structure_age() échoue si l'espèce n'est pas reconnue par get_info_pen()", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  df <- data.frame(
    sp = "ESPECE_FAUSSE",
    age = c(1, 2, 3)
  )
  
  expect_error(
    structure_age(df),
    regexp = "Espèce non reconnue"
  )
})

test_that("structure_age() force les niveaux absents dans la légende", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  df <- data.frame(
    sp = "SANA",
    age = c(1, 2, 3),
    sexe = "M"
  )
  
  res <- structure_age(df, groupement = "sexe")
  
  expect_true(res$success)
  expect_s3_class(res$plot, "ggplot")
  
  levels_plot <- levels(res$plot$data$groupe)
  expect_true(all(c("M", "F") %in% levels_plot))
})

test_that("structure_age() fonctionne pour le groupement 'maturite'", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  df <- data.frame(
    sp = "SANA",
    age = c(1, 2, 3),
    maturite = c("I", "M", "I")
  )
  
  res <- structure_age(df, groupement = "maturite")
  
  expect_true(res$success)
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
})