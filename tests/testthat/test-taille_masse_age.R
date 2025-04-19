test_that("taille_masse_age() gère les cas normaux et limites", {
  # Jeu de données de test complet avec tous les groupes
  data_test <- data.frame(
    ltm = c(150, 160, 140, 135, NA),
    masse = c(60, 80, 55, 50, 70),
    age = c(2, 3, 2, NA, 1),
    sexe = c("F", "M", "M", "F", "IND"),
    maturite = c("O", "O", "N", "N", "IND")
  )
  
  # Appel de la fonction
  res <- taille_masse_age(data_test)
  
  # Test structure
  expect_type(res, "list")
  expect_named(res, c("data", "flextable"))
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  # Test des colonnes attendues
  expected_cols <- c("Sexe",
                     paste0("ltm_", c("nb", "moy", "e_t", "min", "max")),
                     paste0("masse_", c("nb", "moy", "e_t", "min", "max")),
                     paste0("age_", c("nb", "moy", "e_t", "min", "max")))
  expect_named(res$data, expected_cols)
  
  # Test du nombre de groupes
  expect_length(res$data$Sexe, 8)  # 8 groupes attendus
  expect_setequal(res$data$Sexe, c("Tous", "Femelle", "Mâle", "Sexe inconnu",
                                   "Reprod. actifs femelles", "Reprod. actifs mâles",
                                   "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))
  
  # Vérifie que toutes les valeurs NA / Inf ont été remplacées par "-"
  res_chr <- dplyr::select(res$data, -Sexe) %>% purrr::map_chr(~ paste0(unique(.), collapse = " "))
  expect_true(all(stringr::str_detect(res_chr, "-") | stringr::str_detect(res_chr, "\\d")))
  
  # Vérifie que les colonnes "max" et "moy" sont bien arrondies (pas de valeur longue ou scientifique)
  max_cols <- grep("max|moy", names(res$data), value = TRUE)
  val_extraites <- res$data[1, max_cols]
  expect_true(all(grepl("^\\d+(\\.\\d)?$|^-$", as.character(unlist(val_extraites)))))
  
  # Vérifie que la table flextable peut être imprimée sans erreur
  expect_silent(print(res$flextable))
})
