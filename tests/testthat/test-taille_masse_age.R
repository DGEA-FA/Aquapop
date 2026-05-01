test_that("taille_masse_age() gère les cas normaux", {
  data_test <- data.frame(
    ltm = c(150, 160, 140, 135, NA),
    masse = c(60, 80, 55, 50, 70),
    age = c(2, 3, 2, NA, 1),
    sexe = c("F", "M", "M", "F", "IND"),
    maturite = c("O", "O", "N", "N", "IND")
  )
  
  res <- taille_masse_age(data_test)
  
  expect_type(res, "list")
  expect_named(res, c("success", "data", "flextable", "message"))
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  expected_cols <- c(
    "Sexe",
    paste0("ltm_", c("nb", "moy", "e_t", "min", "max")),
    paste0("masse_", c("nb", "moy", "e_t", "min", "max")),
    paste0("age_", c("nb", "moy", "e_t", "min", "max"))
  )
  
  expect_named(res$data, expected_cols)
  expect_length(res$data$Sexe, 8)
  
  expect_setequal(
    as.character(res$data$Sexe),
    c(
      "Tous",
      "Femelle",
      "Mâle",
      "Sexe inconnu",
      "Reprod. actifs femelles",
      "Reprod. actifs mâles",
      "Imm. ou reprod. inactifs",
      "Statut reprod. inconnu"
    )
  )
})

test_that("taille_masse_age() conserve les types analytiques dans data", {
  data_test <- data.frame(
    ltm = c(150, 160, 140, 135, NA),
    masse = c(60, 80, 55, 50, 70),
    age = c(2, 3, 2, NA, 1),
    sexe = c("F", "M", "M", "F", "IND"),
    maturite = c("O", "O", "N", "N", "IND")
  )
  
  res <- taille_masse_age(data_test)
  
  expect_true(res$success)
  expect_s3_class(res$data$Sexe, "factor")
  
  colonnes_stats <- setdiff(names(res$data), "Sexe")
  
  expect_true(all(vapply(res$data[colonnes_stats], is.numeric, logical(1))))
  expect_false(any(vapply(res$data[colonnes_stats], is.character, logical(1))))
})

test_that("taille_masse_age() conserve les NA dans data plutôt que des tirets", {
  data_test <- data.frame(
    ltm = c(150, 160, 140, 135, NA),
    masse = c(60, 80, 55, 50, 70),
    age = c(2, 3, 2, NA, 1),
    sexe = c("F", "M", "M", "F", "IND"),
    maturite = c("O", "O", "N", "N", "IND")
  )
  
  res <- taille_masse_age(data_test)
  
  expect_true(res$success)
  
  colonnes_stats <- setdiff(names(res$data), "Sexe")
  
  expect_false(any(res$data[colonnes_stats] == "-", na.rm = TRUE))
  expect_true(any(is.na(res$data[colonnes_stats])))
})

test_that("taille_masse_age() retourne des statistiques non arrondies dans data", {
  data_test <- data.frame(
    ltm = c(100, 101, 103),
    masse = c(10, 11, 13),
    age = c(1, 2, 4),
    sexe = c("F", "F", "F"),
    maturite = c("O", "O", "O")
  )
  
  res <- taille_masse_age(data_test)
  
  ligne_tous <- res$data[res$data$Sexe == "Tous", ]
  
  expect_equal(ligne_tous$ltm_moy, mean(data_test$ltm))
  expect_equal(ligne_tous$ltm_e_t, stats::sd(data_test$ltm))
  
  expect_false(
    identical(
      ligne_tous$ltm_e_t,
      round(ligne_tous$ltm_e_t, 1)
    )
  )
})

test_that("taille_masse_age() retourne un résultat propre si les données sont vides", {
  data_vide <- data.frame(
    ltm = numeric(),
    masse = numeric(),
    age = numeric(),
    sexe = character(),
    maturite = character()
  )
  
  res <- taille_masse_age(data_vide)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_identical(
    res$message,
    "Aucun spécimen valide disponible pour produire le tableau de taille, masse et âge."
  )
})

test_that("taille_masse_age() retourne un résultat propre si toutes les variables sont manquantes", {
  data_na <- data.frame(
    ltm = c(NA_real_, NA_real_),
    masse = c(NA_real_, NA_real_),
    age = c(NA_real_, NA_real_),
    sexe = c("F", "M"),
    maturite = c("O", "N")
  )
  
  res <- taille_masse_age(data_na)
  
  expect_type(res, "list")
  expect_named(res, c("success", "data", "flextable", "message"))
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_identical(
    res$message,
    "Aucune donnée exploitable n'est disponible pour les variables ltm, masse et age."
  )
})