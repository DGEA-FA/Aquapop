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
    res$data$Sexe,
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
  
  res_chr <- dplyr::select(res$data, -Sexe) |>
    purrr::map_chr(~ paste0(unique(.), collapse = " "))
  
  expect_true(all(stringr::str_detect(res_chr, "-") | stringr::str_detect(res_chr, "\\d")))
  
  max_cols <- grep("max|moy", names(res$data), value = TRUE)
  val_extraites <- res$data[1, max_cols]
  
  expect_true(
    all(grepl("^\\d+(\\.\\d)?$|^-$", as.character(unlist(val_extraites))))
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