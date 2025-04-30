test_that("generate_recapitulatif_inventaire() retourne un tableau conforme", {
  # --- Jeu de données fictif : data_lac ---
  data_lac <- tibble::tibble(
    typ_pech = "Filet",
    no_lac = "12345",
    nom_lac = "Lac Test",
    superficie_ha = 42.0,
    annee = c(2020, 2021)
  )
  
  # --- Jeu de données fictif : data_station ---
  data_station <- tibble::tibble(
    date_pose = as.Date(c("2020-07-01", "2020-07-02", NA)),
    date_leve = as.Date(c("2020-07-03", "2020-07-04", NA)),
    st_hasard = c("O", "N", "O"),
    st_valide = c("O", "O", "N")
  )
  
  # --- Appel de la fonction ---
  recap <- generate_recapitulatif_inventaire(data_lac, data_station)
  
  # --- Vérification 1 : classe ---
  expect_s3_class(recap, "data.frame")
  
  # --- Vérification 2 : colonnes attendues ---
  expect_true("Type de pêche" %in% names(recap))
  expect_true("Filet" %in% names(recap))
  
  # --- Vérification 3 : lignes attendues ---
  lignes_attendues <- c(
    "Nom du lac", "No de lac", "Superficie du lac (ha)",
    "Année(s) de l’inventaire (aaaa)",
    "Date de début de l’inventaire (aaaa-mm-jj)",
    "Date de fin de l’inventaire (aaaa-mm-jj)",
    "N stations aléatoires", "N stations dirigées",
    "N stations valides", "N stations invalides",
    "N stations total"
  )
  expect_true(all(lignes_attendues %in% recap$`Type de pêche`))
  
  # --- Vérification 4 : valeurs précises ---
  expect_equal(recap[recap$`Type de pêche` == "Nom du lac", "Filet", drop = TRUE], "Lac Test")
  expect_equal(recap[recap$`Type de pêche` == "N stations valides", "Filet", drop = TRUE], "2")
  expect_equal(recap[recap$`Type de pêche` == "N stations invalides", "Filet", drop = TRUE], "1")
  expect_equal(recap[recap$`Type de pêche` == "N stations total", "Filet", drop = TRUE], "3")
  
  # --- Vérification 5 : gestion des NA dans les dates ---
  station_na_dates <- data_station
  station_na_dates$date_pose <- NA
  station_na_dates$date_leve <- NA
  recap_na <- generate_recapitulatif_inventaire(data_lac, station_na_dates)
  expect_equal(
    recap_na[recap_na$`Type de pêche` == "Date de début de l’inventaire (aaaa-mm-jj)", "Filet", drop = TRUE],
    "Aucune donnée disponible"
  )
  expect_equal(
    recap_na[recap_na$`Type de pêche` == "Date de fin de l’inventaire (aaaa-mm-jj)", "Filet", drop = TRUE],
    "Aucune donnée disponible"
  )
})
