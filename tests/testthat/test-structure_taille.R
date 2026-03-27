# =============================================================================
# Tests pour structure_taille()
# =============================================================================

specimen_test <- tibble::tibble(
  sp = rep("SANA", 10),
  ltm = c(300, 310, 320, 340, 350, 370, 380, 390, 400, 410),
  sexe = rep(c("F", "M"), each = 5),
  maturite = rep(c("O", "N"), times = 5),
  marquage = rep("NMA", 10)
)

test_that("structure_taille() fonctionne avec un jeu de données fictif structuré", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data <- specimen_test
  
  expect_true(all(c("sp", "ltm", "sexe", "maturite", "marquage") %in% names(data)))
  expect_equal(unique(data$sp), "SANA")
  
  res <- structure_taille(data, groupement = "sexe")
  
  expect_type(res, "list")
  expect_named(res, c("success", "plot", "data", "flextable", "message"))
  
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  expect_gt(nrow(res$data), 0)
})

test_that("structure_taille() retourne success = FALSE si aucun spécimen n'est disponible", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_vide <- specimen_test[0, ]
  
  res <- structure_taille(data_vide, groupement = "sexe")
  
  expect_type(res, "list")
  expect_named(res, c("success", "plot", "data", "flextable", "message"))
  
  expect_false(res$success)
  expect_null(res$plot)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("structure_taille() retourne success = FALSE si toutes les longueurs sont manquantes", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_sans_ltm <- specimen_test |>
    dplyr::mutate(ltm = NA_real_)
  
  res <- structure_taille(data_sans_ltm, groupement = "sexe")
  
  expect_false(res$success)
  expect_null(res$plot)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_match(res$message, "Aucun spécimen valide disponible|Aucune")
})

test_that("structure_taille() échoue si plusieurs espèces sont présentes", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  data_multi_especes <- specimen_test |>
    dplyr::mutate(
      sp = c(rep("SANA", 5), rep("SAFO", 5))
    )
  
  expect_error(
    structure_taille(data_multi_especes, groupement = "sexe"),
    regexp = "une seule espèce"
  )
})

test_that("structure_taille() échoue si le groupement est invalide", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  expect_error(
    structure_taille(specimen_test, groupement = "bidon"),
    regexp = "Groupement non reconnu|Groupement invalide|Must be element of set"
  )
})