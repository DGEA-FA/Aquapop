test_that("mortalite_chaprob fonctionne dans le cas nominal", {
  specimen <- data.frame(age = c(2, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8))
  
  res <- mortalite_chaprob(specimen, pp = 2, age_max = 8)
  
  # Vérifie la structure retournée
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  expect_s3_class(res$flextable, "flextable")
  
  # Vérifie les colonnes du tableau
  expect_named(res$data, c("z", "se", "a", "ic_95"))
  
  # Vérifie les types des colonnes
  expect_type(res$data$z, "double")
  expect_type(res$data$se, "double")
  expect_type(res$data$a, "double")
  expect_type(res$data$ic_95, "character")
  
  # Vérifie le calcul de a
  z <- res$data$z
  a_calcule <- round((1 - exp(-z)) * 100, 1)
  expect_equal(res$data$a, a_calcule)
  
  # Vérifie le calcul de l'IC 95%
  se <- res$data$se
  borne_inf <- round((1 - exp(-(z - se))) * 100, 1)
  borne_sup <- round((1 - exp(-(z + se))) * 100, 1)
  ic_attendu <- glue::glue("[{borne_inf}-{borne_sup}]")
  expect_equal(res$data$ic_95, as.character(ic_attendu))
})

test_that("mortalite_chaprob retourne une erreur si tous les âges sont NA", {
  specimen <- data.frame(age = c(NA, NA, NA))
  expect_error(mortalite_chaprob(specimen, pp = 2, age_max = 8))
})

test_that("mortalite_chaprob retourne une erreur avec un seul âge distinct", {
  specimen <- data.frame(age = c(5))
  expect_error(
    mortalite_chaprob(specimen, pp = 5, age_max = 5),
    regexp = "au moins deux classes d’âge"
  )
})
