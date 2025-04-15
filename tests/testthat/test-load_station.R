test_that("load_station fonctionne avec un fichier complet valide", {
  fichier <- test_path("testdata", "station_complet.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c(
    "no_lac", "typ_pech", "annee", "no_station", "st_valide", "st_hasard", 
    "lat_dd.dec", "long_dd.dec", "prof_deb", "prof_fin",
    "date_pose", "date_leve", "h_pose", "h_leve", "pose", "leve", "duree",
    "type_maill", "comments_station"
  ) %in% names(df)))
  expect_equal(anyDuplicated(df$no_station), 0)
})

test_that("load_station fonctionne avec colonnes en désordre", {
  fichier <- test_path("testdata", "station_desordonnee.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true("no_station" %in% names(df))
  expect_equal(anyDuplicated(df$no_station), 0)
})

test_that("load_station ignore les colonnes supplémentaires", {
  fichier <- test_path("testdata", "station_colonnes_sup.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_false("extra1" %in% names(df))
  expect_false("extra2" %in% names(df))
})

test_that("load_station fonctionne sans les colonnes optionnelles", {
  fichier <- test_path("testdata", "station_sans_optionnelles.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  colonnes_optionnelles <- c(
    "lat_dd.dec", "long_dd.dec", "prof_deb", "prof_fin", "type_maill",
    "heure_pose", "min_pose", "heure_leve", "min_leve",
    "date_pose", "date_leve", "h_pose", "h_leve", "pose", "leve", "duree",
    "comments_station"
  )
  
  expect_s3_class(df, "data.frame")
  expect_true(all(colonnes_optionnelles %in% names(df)))
  expect_true(all(unlist(lapply(df[colonnes_optionnelles], function(x) all(is.na(x))))))
})

test_that("load_station échoue si une colonne obligatoire est absente", {
  fichier <- test_path("testdata", "station_manque_obligatoire.xlsx")
  expect_error(
    load_station(fichier, verbose = FALSE),
    "Colonnes obligatoires manquantes"
  )
})

test_that("load_station convertit correctement les années Excel", {
  fichier <- test_path("testdata", "station_excel_annee.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  expect_type(df$annee, "integer")
  expect_true(all(df$annee >= 2000))
})

test_that("load_station remplace correctement les statuts NA ou IND", {
  fichier <- test_path("testdata", "station_statuts_ind.xlsx")
  df <- load_station(fichier, verbose = FALSE)
  
  expect_true(all(df$st_valide %in% c("O", "N")))
  expect_true(all(df$st_hasard %in% c("O", "N")))
})

