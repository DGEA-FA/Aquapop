test_that("maturite_validate_data retourne success = TRUE avec des données valides pour ltm", {
  specimen_data <- data.frame(
    maturite = c("N", "O", "N", "O", "N", "O"),
    sexe = c("F", "M", "F", "M", "F", "M"),
    ltm = c(120, 130, 140, 150, 160, 170),
    age = c(1, 2, 2, 3, 3, 4)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_type(res, "list")
  expect_named(res, c("success", "data", "message"))
  
  expect_true(res$success)
  expect_null(res$message)
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 6)
  expect_equal(levels(res$data$maturite), c("N", "O"))
  expect_equal(levels(res$data$sexe), c("F", "M"))
  expect_s3_class(res$data$maturite, "ordered")
})

test_that("maturite_validate_data retourne success = TRUE avec des données valides pour age", {
  specimen_data <- data.frame(
    maturite = c("N", "O", "N", "O", "N", "O"),
    sexe = c("F", "M", "F", "M", "F", "M"),
    ltm = c(120, 130, 140, 150, 160, 170),
    age = c(1, 2, 2, 3, 3, 4)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "age"
  )
  
  expect_true(res$success)
  expect_null(res$message)
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 6)
  expect_false(any(is.na(res$data$age)))
})

test_that("maturite_validate_data déclenche une erreur si des colonnes requises sont absentes", {
  specimen_data <- data.frame(
    sexe = c("F", "M"),
    age = c(1, 2)
  )
  
  expect_error(
    maturite_validate_data(
      specimen_data = specimen_data,
      variable = "age"
    ),
    "doit contenir les colonnes"
  )
})

test_that("maturite_validate_data retourne success = FALSE si le jeu de données brut est vide", {
  specimen_data <- data.frame(
    maturite = character(),
    sexe = character(),
    ltm = numeric(),
    age = numeric()
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_false(res$success)
  expect_null(res$data)
  expect_true(is.character(res$message))
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("maturite_validate_data retourne un message spécifique si aucun âge n'est disponible", {
  specimen_data <- data.frame(
    maturite = c("N", "O", "N", "O"),
    sexe = c("F", "M", "F", "M"),
    age = c(NA, NA, NA, NA),
    ltm = c(120, 130, 140, 150)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "age"
  )
  
  expect_false(res$success)
  expect_null(res$data)
  expect_true(is.character(res$message))
  expect_match(res$message, "Il n'y a pas d'âge disponible")
})

test_that("maturite_validate_data retourne un message spécifique si aucune longueur n'est disponible", {
  specimen_data <- data.frame(
    maturite = c("N", "O", "N", "O"),
    sexe = c("F", "M", "F", "M"),
    age = c(1, 2, 3, 4),
    ltm = c(NA, NA, NA, NA)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_false(res$success)
  expect_null(res$data)
  expect_true(is.character(res$message))
  expect_match(res$message, "Il n'y a pas de longueur disponible")
})

test_that("maturite_validate_data retourne success = FALSE si aucune donnée exploitable ne reste après nettoyage", {
  specimen_data <- data.frame(
    maturite = c("IND", "IND", "IND"),
    sexe = c("F", "M", "IND"),
    ltm = c(100, 110, 120),
    age = c(1, 2, 3)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_false(res$success)
  expect_null(res$data)
  expect_true(is.character(res$message))
  expect_match(
    res$message,
    "Aucune donnée exploitable n'est disponible après le nettoyage"
  )
})

test_that("maturite_validate_data retourne success = FALSE si un seul état de maturité est présent après nettoyage", {
  specimen_data <- data.frame(
    maturite = c("O", "O", "O", "O", "O", "O"),
    sexe = c("F", "M", "F", "M", "F", "M"),
    ltm = c(120, 130, 140, 150, 160, 170),
    age = c(1, 2, 2, 3, 3, 4)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_false(res$success)
  expect_null(res$data)
  expect_true(is.character(res$message))
  expect_match(
    res$message,
    "ne contiennent pas à la fois des individus immatures et matures"
  )
})

test_that("maturite_validate_data retire correctement les lignes avec IND et NA", {
  specimen_data <- data.frame(
    maturite = c("N", "O", "IND", "N", "O", "N", "O", "N"),
    sexe = c("F", "M", "F", "IND", "M", "F", "M", "F"),
    ltm = c(120, 130, 140, 150, NA, 170, 180, 190),
    age = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
  
  res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = "ltm"
  )
  
  expect_true(res$success)
  expect_s3_class(res$data, "data.frame")
  expect_false(any(is.na(res$data$ltm)))
  expect_false(any(as.character(res$data$maturite) == "IND"))
  expect_false(any(as.character(res$data$sexe) == "IND"))
  expect_equal(nrow(res$data), 5)
})

test_that("maturite_validate_data déclenche une erreur si specimen_data n'est pas un data.frame", {
  expect_error(
    maturite_validate_data(
      specimen_data = c(1, 2, 3),
      variable = "ltm"
    )
  )
})