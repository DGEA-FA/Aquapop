test_that("load_specimen fonctionne avec un fichier nominal complet", {
  path <- test_path("testdata/specimen_nominal.xlsx")
  df <- load_specimen(path, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c(
    "no_lac", "typ_pech", "no_station", "no_specimen", "sp",
    "ltm", "masse", "age", "sexe", "maturite", "annee",
    "lf", "marquage", "ind_insec", "ind_benth", "ind_planc",
    "ind_chyme", "ind_vide", "ind_poiss", "poiss1", "poiss2", "comments_specimen"
  ) %in% names(df)))
})

test_that("load_specimen fonctionne avec un fichier minimal", {
  path <- test_path("testdata/specimen_minimal.xlsx")
  df <- load_specimen(path, verbose = FALSE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("no_lac", "typ_pech", "no_station", "no_specimen", "sp",
                    "ltm", "masse", "age", "sexe", "maturite", "annee") %in% names(df)))
  
  colonnes_optionnelles <- c(
    "lf", "marquage", "ind_insec", "ind_benth", "ind_planc", "ind_chyme",
    "ind_vide", "ind_poiss", "poiss1", "poiss2", "comments_specimen"
  )
  
  colonnes_absentes <- colonnes_optionnelles
  
  # Valeurs par défaut à tester
  valeurs_defaut <- list(
    sexe     = "IND",
    maturite = "IND",
    marquage = "NMA"
  )
  
  for (col in colonnes_absentes) {
    if (col %in% names(valeurs_defaut)) {
      expect_true(all(as.character(df[[col]]) == valeurs_defaut[[col]]),
                  info = paste("Colonne", col, "devrait être remplie avec", valeurs_defaut[[col]]))
    } else {
      expect_true(all(is.na(df[[col]])), info = paste("Colonne", col, "devrait être NA"))
    }
  }
})

test_that("load_specimen détecte une colonne obligatoire manquante", {
  path <- test_path("testdata/specimen_colonne_manquante.xlsx")
  expect_error(load_specimen(path, verbose = FALSE),
               "Colonnes obligatoires manquantes")
})

test_that("load_specimen ignore les colonnes supplémentaires", {
  path <- test_path("testdata/specimen_colonnes_sup.xlsx")
  df <- load_specimen(path, verbose = FALSE)
  
  expect_false("extra1" %in% names(df))
  expect_false("extra2" %in% names(df))
})

test_that("load_specimen reconnait les colonnes dans le désordre", {
  path <- test_path("testdata/specimen_colonnes_desordonnees.xlsx")
  df <- load_specimen(path, verbose = FALSE)
  
  expect_equal(as.character(df$sp[1]), "SANA")
  expect_equal(df$ltm[1], 110)
})

test_that("load_specimen convertit les années Excel en dates valides", {
  df <- data.frame(
    no_lac      = "999",
    typ_pech    = "PE",
    no_station  = "03",
    no_specimen = "999",
    sp          = "PECA",
    ltm         = "105",
    masse       = "12.4",
    age         = "1",
    sexe        = "M",
    valide      = "O",
    hasard      = "O",
    maturite    = "O",
    annee       = "44927"  # équivalent à 2023-01-01
  )
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list("Specimens" = df), path)
  
  resultat <- load_specimen(path, verbose = FALSE)
  expect_equal(resultat$annee[1], 2023)
})

test_that("load_specimen attribue les bons niveaux aux colonnes facteur", {
  path <- test_path("testdata/specimen_nominal.xlsx")
  df <- load_specimen(path, verbose = FALSE)
  
  expect_true(inherits(df$sexe, "factor"))
  expect_true(inherits(df$maturite, "factor"))
  expect_true(inherits(df$marquage, "factor"))
  
  niveaux_sexe <- levels(df$sexe)
  niveaux_maturite <- levels(df$maturite)
  niveaux_marquage <- levels(df$marquage)
  
  expect_true(all(c("F", "M", "IND") %in% niveaux_sexe),
              info = paste("Niveaux manquants dans sexe :", toString(setdiff(c("F", "M", "IND"), niveaux_sexe))))
  
  expect_true(all(c("O", "N", "IND") %in% niveaux_maturite),
              info = paste("Niveaux manquants dans maturite :", toString(setdiff(c("O", "N", "IND"), niveaux_maturite))))
  
  expect_true(all(c("MA", "NMA") %in% niveaux_marquage),
              info = paste("Niveaux manquants dans marquage :", toString(setdiff(c("MA", "NMA"), niveaux_marquage))))
})
