test_that("load_recolte fonctionne avec un fichier valide complet", {
  fichier <- test_path("testdata", "recolte_complete.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("no_lac", "typ_pech", "annee", "no_station", "sp", "nb_capture", "nb_pese", "comments_recolte") %in% names(df)))
  expect_type(df$annee, "integer")
  expect_type(df$nb_capture, "double")
  expect_type(df$comments_recolte, "character")
})

test_that("load_recolte convertit annee au format Excel correctement", {
  fichier <- test_path("testdata", "recolte_annee_excel.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_true(all(df$annee == 2020L))
})

test_that("load_recolte accepte annee au format texte", {
  fichier <- test_path("testdata", "recolte_annee_texte.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_true(all(df$annee == 2021L))
})

test_that("load_recolte retire les doublons exacts", {
  fichier <- test_path("testdata", "recolte_doublons.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_equal(nrow(df), 1)
})

test_that("load_recolte conserve les lignes distinctes par comments", {
  fichier <- test_path("testdata", "recolte_doublons_commentaires.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_equal(nrow(df), 2)
})

test_that("load_recolte supprime la colonne nom_lac si présente", {
  fichier <- test_path("testdata", "recolte_avec_nom_lac.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_false("nom_lac" %in% names(df))
})

test_that("load_recolte tolère l'absence de comments", {
  fichier <- test_path("testdata", "recolte_sans_comments.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_true("comments_recolte" %in% names(df))
  expect_true(all(is.na(df$comments_recolte)))
})

test_that("load_recolte accepte colonnes désordonnées", {
  fichier <- test_path("testdata", "recolte_colonnes_desordonnees.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
})

test_that("load_recolte ignore les colonnes supplémentaires", {
  fichier <- test_path("testdata", "recolte_colonnes_sup.xlsx")
  df <- load_recolte(fichier, verbose = FALSE)
  
  expect_false("extra1" %in% names(df))
  expect_false("extra2" %in% names(df))
})
