# Jeu de données simulé avec croissance crédible ----
df_valid <- tibble::tibble(
  no_specimen = 1:60,
  sp = "TEST",
  age = rep(1:6, each = 10),
  ltm = c(
    rnorm(10, 100, 5),
    rnorm(10, 140, 5),
    rnorm(10, 170, 5),
    rnorm(10, 190, 5),
    rnorm(10, 210, 5),
    rnorm(10, 220, 5)
  )
)

# Jeu de données problématique où aucun modèle ne converge ----
df_fail <- tibble::tibble(
  no_specimen = c(
    1, 10, 11, 12, 13, 14, 15, 16, 18, 19,
    2, 20, 21, 22, 23, 24, 25, 26, 27, 28,
    29, 3, 30, 31, 32, 33, 34, 35, 36, 37,
    38, 39, 4, 40, 41, 42, 43, 44, 46, 47,
    48, 49, 5, 50, 51, 6, 7, 8, 9
  ),
  sp = "TEST",
  age = c(
    2, 2, 3, 3, 2, 1, 1, 3, 2, 1,
    2, 2, 4, 1, 2, 2, 1, 4, 2, 2,
    3, 1, 4, 4, 3, 4, 2, 4, 3, 2,
    2, 2, 1, 2, 4, 2, 2, 3, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 4, 4
  ),
  ltm = c(
    185, 214, 305, 278, 273, 167, 144, 333, 164, 125,
    195, 249, 352, 211, 191, 191, 159, 377, 200, 193,
    253, 180, 145, 145, 150, 156, 144, 202, 263, 237,
    260, 341, 142, 182, 250, 184, 187, 212, 227, 356,
    285, 340, 165, 335, 124, 185, 150, 339, 316
  )
)

test_that("croissance_compare_modele retourne une structure valide", {
  
  res <- croissance_compare_modele(df_valid)
  
  expect_type(res, "list")
  
  expect_named(
    res,
    c("success", "data", "flextable", "message")
  )
  
  expect_true(res$success)
  
  df_out <- res$data
  
  expect_s3_class(df_out, "data.frame")
  
  expect_true(all(c(
    "methode", "l_inf", "l_inf_ic",
    "k", "k_ic",
    "t0", "t0_ic",
    "aicc", "delta_aicc", "aiccwt",
    "convergence"
  ) %in% names(df_out)))
  
})

test_that("le résultat contient exactement trois modèles", {
  
  res <- croissance_compare_modele(df_valid)
  
  expect_true(res$success)
  
  df_out <- res$data
  
  expect_equal(nrow(df_out), 3)
  
  expect_setequal(
    df_out$methode,
    c("Von Bertalanffy", "Gompertz", "Logistique")
  )
  
})

test_that("retourne un objet flextable dans la liste de sortie", {
  
  res <- croissance_compare_modele(df_valid)
  
  expect_true(res$success)
  expect_s3_class(res$flextable, "flextable")
  
})

test_that("retourne success = FALSE si moins de 3 spécimens valides", {
  
  df_bad <- tibble::tibble(
    no_specimen = 1:2,
    sp = "TEST",
    age = c(1, 2),
    ltm = c(150, 160)
  )
  
  res <- croissance_compare_modele(df_bad)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_type(res$message, "character")
  expect_match(res$message, "au moins 3 spécimens", fixed = TRUE)
  
})

test_that("retourne success = FALSE si moins de 3 âges distincts", {
  
  df_bad <- tibble::tibble(
    no_specimen = 1:6,
    sp = "TEST",
    age = c(1, 1, 1, 2, 2, 2),
    ltm = c(100, 105, 110, 130, 135, 140)
  )
  
  res <- croissance_compare_modele(df_bad)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_type(res$message, "character")
  expect_match(res$message, "au moins 3 âges distincts", fixed = TRUE)
  
})

test_that("fonction tolère des IC non calculables", {
  
  df <- df_valid
  df$ltm <- df$ltm + rnorm(nrow(df), 0, 20)
  
  res <- croissance_compare_modele(df)
  
  expect_true(res$success)
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 3)
  
})

test_that("si aucun modèle ne converge, la fonction retourne un tableau valide avec message global", {
  
  res <- croissance_compare_modele(df_fail)
  
  expect_true(res$success)
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 3)
  
  expect_true(all(res$data$convergence == FALSE))
  expect_true(all(is.na(res$data$l_inf)))
  expect_true(all(is.na(res$data$k)))
  expect_true(all(is.na(res$data$t0)))
  
  expect_type(res$message, "character")
  expect_match(res$message, "Aucun des modèles de croissance", fixed = TRUE)
  expect_match(res$message, "n'a convergé", fixed = TRUE)
  
})

test_that("si au moins un modèle converge et vbStarts fonctionne, le message global est NULL", {
  
  res <- croissance_compare_modele(df_valid)
  
  expect_true(res$success)
  expect_null(res$message)
  
})

test_that("si vbStarts échoue, la fonction utilise les valeurs initiales de secours", {
  
  local_mocked_bindings(
    vbStarts = function(...) {
      stop("échec simulé de vbStarts")
    }
  )
  
  res <- croissance_compare_modele(df_valid)
  
  expect_true(res$success)
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 3)
  
  expect_type(res$message, "character")
  expect_match(res$message, "valeurs des paramètres initiaux", fixed = TRUE)
  expect_match(res$message, "K = 0.3", fixed = TRUE)
  expect_match(res$message, "t0 = 0", fixed = TRUE)
  
})

test_that("si vbStarts échoue et qu'au moins un modèle converge, le message contient l'avertissement sur les valeurs initiales", {
  
  local_mocked_bindings(
    vbStarts = function(...) {
      stop("échec simulé de vbStarts")
    }
  )
  
  res <- croissance_compare_modele(df_valid)
  
  expect_true(res$success)
  expect_type(res$message, "character")
  expect_match(res$message, "valeurs des paramètres initiaux", fixed = TRUE)
  expect_match(res$message, "longueur du plus grand spécimen", fixed = TRUE)
  expect_false(grepl("Aucun des modèles de croissance", res$message, fixed = TRUE))
  
})

test_that("si vbStarts échoue et qu'aucun modèle ne converge, le message combine l'avertissement et le message global", {
  
  local_mocked_bindings(
    vbStarts = function(...) {
      stop("échec simulé de vbStarts")
    }
  )
  
  res <- croissance_compare_modele(df_fail)
  
  expect_true(res$success)
  expect_true(all(res$data$convergence == FALSE))
  
  expect_type(res$message, "character")
  expect_match(res$message, "valeurs des paramètres initiaux", fixed = TRUE)
  expect_match(res$message, "K = 0.3", fixed = TRUE)
  expect_match(res$message, "Aucun des modèles de croissance", fixed = TRUE)
  
})

test_that("si growth échoue complètement après le fallback, la fonction retourne success = FALSE", {
  
  local_mocked_bindings(
    vbStarts = function(...) {
      stop("échec simulé de vbStarts")
    },
    growth = function(...) {
      stop("échec simulé de growth")
    }
  )
  
  res <- croissance_compare_modele(df_valid)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  
  expect_type(res$message, "character")
  expect_match(res$message, "valeurs des paramètres initiaux", fixed = TRUE)
  expect_match(
    res$message,
    "La modélisation de croissance a échoué",
    fixed = TRUE
  )
  
})