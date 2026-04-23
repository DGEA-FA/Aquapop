test_that("mortalite_chaprob fonctionne dans le cas nominal", {
  specimen <- data.frame(age = c(2, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8))
  
  res <- mortalite_chaprob(specimen, pp = 2, age_max = 8)
  
  # Vérifie la structure retournée
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  
  # Vérifie le statut de succès
  expect_true(res$success)
  expect_null(res$message)
  
  # Vérifie les objets retournés
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  # Vérifie les colonnes du tableau
  expect_named(res$data, c("z", "se", "a", "ic_95"))
  
  # Vérifie les types des colonnes
  expect_type(res$data$z, "double")
  expect_type(res$data$se, "double")
  expect_type(res$data$a, "double")
  expect_type(res$data$ic_95, "character")
  
  # Vérifie le calcul de A
  z <- res$data$z
  a_calcule <- round((1 - exp(-z)) * 100, 1)
  expect_equal(res$data$a, a_calcule)
  
  # Vérifie le calcul de l'IC 95 % (avec virgules)
  se <- res$data$se
  
  borne_inf <- format(
    round((1 - exp(-(z - se))) * 100, 1),
    nsmall = 1,
    decimal.mark = ","
  )
  
  borne_sup <- format(
    round((1 - exp(-(z + se))) * 100, 1),
    nsmall = 1,
    decimal.mark = ","
  )
  
  ic_attendu <- glue("[{borne_inf} – {borne_sup}]")
  
  expect_equal(res$data$ic_95, ic_attendu)
})

test_that("mortalite_chaprob retourne success = FALSE si tous les âges sont NA", {
  specimen <- data.frame(age = c(NA, NA, NA))
  
  res <- mortalite_chaprob(specimen, pp = 2, age_max = 8)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  expect_false(res$success)
  expect_equal(
    res$message,
    "Aucune donnée d'âge valide n'est disponible pour Chapman-Robson."
  )
  expect_null(res$data)
  expect_null(res$flextable)
})

test_that("mortalite_chaprob retourne success = FALSE avec un seul âge distinct", {
  specimen <- data.frame(age = c(5))
  
  res <- mortalite_chaprob(specimen, pp = 2, age_max = 8)
  
  expect_type(res, "list")
  expect_named(res, c("success", "message", "data", "flextable"))
  expect_false(res$success)
  expect_match(
    res$message,
    "au moins deux classes d'âge différentes"
  )
  expect_null(res$data)
  expect_null(res$flextable)
})