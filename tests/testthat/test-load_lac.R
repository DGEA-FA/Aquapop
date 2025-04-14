test_that("load_lac fonctionne avec un fichier complet valide", {
  fichier <- test_path("testdata", "lac_complet.xlsx")
  df <- load_lac(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("ID", "annee", "nom_lac", "typ_pech", "no_lac") %in% names(df)))
  expect_equal(anyDuplicated(df$ID), 0)
})

test_that("load_lac ajoute la colonne comments comme NA si absente", {
  fichier <- test_path("testdata", "lac_sans_comments.xlsx")
  df <- load_lac(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true("comments" %in% names(df))
  expect_true(all(is.na(df$comments)))
})

test_that("load_lac tolère les colonnes supplémentaires sans les inclure dans le résultat", {
  fichier <- test_path("testdata", "lac_avec_colonnes_sup.xlsx")
  df <- load_lac(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true("no_lac" %in% names(df))
  expect_false("extra1" %in% names(df))
  expect_false("extra2" %in% names(df))
})

test_that("load_lac échoue si une colonne obligatoire est absente dans les synonymes", {
  fichier <- test_path("testdata", "lac_manque_colonne.xlsx")
  expect_error(
    load_lac(fichier, verbose = FALSE),
    "Colonnes obligatoires manquantes"
  )
})

test_that("load_lac fonctionne avec des noms de colonnes désordonnés", {
  fichier <- test_path("testdata", "lac_colonnes_desordonnees.xlsx")
  df <- load_lac(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true("ID" %in% names(df))
  expect_equal(anyDuplicated(df$ID), 0)
})

test_that("load_lac reconnait les synonymes via clean_names", {
  # Création d'un jeu de données avec des synonymes réalistes
  df <- data.frame(
    "Numéro LCE" = "00123",
    "Nom du lac" = "Lac Test",
    "Type pêche" = "PE",
    "Année"      = "2023"
  )
  
  fichier <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list("Lac" = df), fichier)
  
  resultat <- load_lac(fichier, verbose = FALSE)
  
  expect_true(all(c("no_lac", "nom_lac", "typ_pech", "annee", "ID") %in% names(resultat)))
  expect_equal(as.character(resultat$nom_lac[1]), "Lac Test")
  expect_equal(resultat$annee[1], 2023L)
})

test_that("load_lac ajoute toutes les colonnes optionnelles manquantes comme NA", {
  df <- data.frame(
    no_lac = "123",
    nom_lac = "Lac Minimal",
    typ_pech = "PE",
    annee = "2021"
  )
  fichier <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list("Lac" = df), fichier)
  
  result <- load_lac(fichier, verbose = FALSE)
  
  colonnes_optionnelles <- c(
    "region_admin", "sp_pen", "long_dd.dec", "lat_dd.dec", "terr_faun", "zon_pech",
    "superficie_ha", "perimetre_km", "prof_max_m", "prof_moy_m", "comments"
  )
  
  expect_true(all(colonnes_optionnelles %in% names(result)))
  expect_true(all(unlist(lapply(result[colonnes_optionnelles], function(x) all(is.na(x))))))
})
