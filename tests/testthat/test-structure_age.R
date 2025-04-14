test_that("structure_age fonctionne sans erreur pour groupement = 'tous'", {
  df <- data.frame(sp = "SANA", age = c(1, 2, 2, 3, 3, 3))
  res <- structure_age(df, groupement = "tous")
  
  expect_type(res, "list")
  expect_named(res, c("plot", "data", "flextable"))
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  expect_true(all(c("age", "n") %in% names(res$data)))
})

test_that("structure_age fonctionne pour un groupement valide (e.g. sexe)", {
  df <- data.frame(sp = "SANA", age = c(1, 2, 3), sexe = c("M", "F", "M"))
  res <- structure_age(df, groupement = "sexe")
  
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
})

test_that("structure_age retourne un plot vide si aucun âge valide", {
  df <- data.frame(sp = "SANA", age = NA, sexe = "F")
  res <- structure_age(df, groupement = "sexe")
  
  expect_s3_class(res$plot, "ggplot")
  expect_equal(nrow(res$data), 0)
})

test_that("structure_age échoue si plusieurs espèces", {
  df <- data.frame(sp = c("SANA", "SAFO"), age = c(1, 2))
  expect_error(structure_age(df), "une seule espèce")
})

test_that("structure_age échoue si colonne manquante", {
  df <- data.frame(sp = "SANA", age = c(1, 2))
  expect_error(structure_age(df, groupement = "maturite"),
               regexp = "La colonne correspondant au groupement 'maturite' est manquante")
})

test_that("structure_age échoue si groupement invalide", {
  df <- data.frame(sp = "SANA", age = c(1, 2))
  expect_error(structure_age(df, groupement = "erreur"), "Groupement non reconnu")
})

test_that("structure_age échoue si l'espèce n'est pas reconnue par get_info_pen()", {
  df <- data.frame(sp = "ESPECE_FAUSSE", age = c(1, 2, 3))
  expect_error(structure_age(df), "Espèce non reconnue")
})

test_that("structure_age force les niveaux absents dans la légende", {
  df <- data.frame(sp = "SANA", age = c(1, 2, 3), sexe = "M")
  res <- structure_age(df, groupement = "sexe")
  levels_present <- levels(df$sexe)
  levels_plot <- levels(res$plot$data$groupe)
  expect_true(all(c("M", "F") %in% levels_plot))  # suppose que F est forcé même si absent
})

test_that("structure_age fonctionne pour le groupement 'maturite'", {
  df <- data.frame(sp = "SANA", age = c(1, 2, 3), maturite = c("I", "M", "I"))
  res <- structure_age(df, groupement = "maturite")
  expect_s3_class(res$plot, "ggplot")
})
