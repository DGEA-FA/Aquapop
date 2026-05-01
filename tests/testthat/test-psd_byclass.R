# tests/testthat/test-psd_byclass.R

test_that("psd_byclass() retourne les bons éléments avec des données valides", {
  set.seed(123)
  
  data_ex <- tibble(
    sp = rep("SANA", 100),
    ltm = sample(100:1000, 100, replace = TRUE)
  )
  
  res <- psd_byclass(data_ex)
  
  expect_type(res, "list")
  expect_named(res, c("success", "data", "flextable", "plot", "message"))
  
  expect_true(res$success)
  expect_null(res$message)
  
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  expect_s3_class(res$plot, "ggplot")
})

test_that("psd_byclass() conserve les types analytiques dans data", {
  set.seed(123)
  
  data_ex <- tibble(
    sp = rep("SANA", 100),
    ltm = sample(100:1000, 100, replace = TRUE)
  )
  
  res <- psd_byclass(data_ex)
  result <- res$data
  
  expect_true(res$success)
  expect_equal(colnames(result), c("classe", "intervalle", "n", "freq"))
  
  expect_s3_class(result$classe, "factor")
  expect_type(result$intervalle, "character")
  expect_type(result$n, "integer")
  expect_type(result$freq, "double")
})

test_that("les fréquences de psd_byclass() sont numériques et cohérentes", {
  set.seed(123)
  
  data_ex <- tibble(
    sp = rep("SANA", 120),
    ltm = sample(c(100, 200, 300, 400, 500, 600, 700, 800), 120, replace = TRUE)
  )
  
  res <- psd_byclass(data_ex)
  result <- res$data
  
  expect_true(res$success)
  
  freq_non_na <- result$freq[!is.na(result$freq)]
  
  expect_true(all(freq_non_na >= 0))
  expect_true(sum(freq_non_na) <= 101)
})

test_that("les classes manquantes apparaissent avec des fréquences NA", {
  data_ex <- tibble(
    sp = rep("SANA", 20),
    ltm = rep(120, 20)
  )
  
  res <- psd_byclass(data_ex)
  result <- res$data
  
  expect_true(res$success)
  expect_true(any(is.na(result$freq)))
  expect_equal(length(result$classe), length(psd_classnames))
})

test_that("les noms de classes et les intervalles sont bien alignés", {
  set.seed(123)
  
  data_ex <- tibble(
    sp = rep("SANA", 60),
    ltm = sample(200:700, 60, replace = TRUE)
  )
  
  res <- psd_byclass(data_ex)
  result <- res$data
  
  expect_true(res$success)
  
  expect_true(all(as.character(result$classe) %in% psd_classnames))
  expect_equal(nrow(result), length(psd_classnames))
})

test_that("psd_byclass() retourne success = FALSE si les données sont vides", {
  data_vide <- tibble(
    sp = character(),
    ltm = numeric()
  )
  
  res <- psd_byclass(data_vide)
  
  expect_type(res, "list")
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_null(res$plot)
  expect_match(res$message, "Aucun spécimen valide disponible")
})

test_that("psd_byclass() retourne success = FALSE si toutes les longueurs sont manquantes", {
  data_na <- tibble(
    sp = rep("SANA", 10),
    ltm = rep(NA_real_, 10)
  )
  
  res <- psd_byclass(data_na)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_null(res$plot)
  expect_match(res$message, "Aucune longueur exploitable")
})

test_that("psd_byclass() retourne success = FALSE si l'espèce n'est pas supportée", {
  data_nok <- tibble(
    sp = rep("INCONNU", 10),
    ltm = rep(200, 10)
  )
  
  res <- psd_byclass(data_nok)
  
  expect_false(res$success)
  expect_null(res$data)
  expect_null(res$flextable)
  expect_null(res$plot)
  expect_match(res$message, "n'est pas supportée")
})

test_that("psd_byclass() échoue si colonne ltm ou sp absente", {
  data_sans_ltm <- tibble(
    sp = rep("SANA", 10)
  )
  
  data_sans_sp <- tibble(
    ltm = rep(100, 10)
  )
  
  expect_error(
    psd_byclass(data_sans_ltm),
    "Le jeu de données doit contenir les colonnes `sp` et `ltm`"
  )
  
  expect_error(
    psd_byclass(data_sans_sp),
    "Le jeu de données doit contenir les colonnes `sp` et `ltm`"
  )
})

test_that("psd_byclass() échoue si plusieurs espèces sont présentes", {
  data_multi <- tibble(
    sp = rep(c("SANA", "MAME"), each = 10),
    ltm = rep(200, 20)
  )
  
  expect_error(
    psd_byclass(data_multi),
    "Les données doivent être filtrées pour une seule espèce"
  )
})