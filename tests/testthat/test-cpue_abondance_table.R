test_that("cpue_abondance_table() retourne un tableau structuré avec CPUE (caractères)", {
  specimens <- tibble::tibble(
    sexe = c("M", "F", "F", "IND", "M", "F", "F", "M", "M", "F"),
    maturite = c("O", "O", "N", "IND", "O", "N", "O", "N", "N", "IND")
  )
  
  cpue_table_tous <- tibble::tibble(
    methode = c("poisson", "nb1"),
    cpue = c("12.3", "15.8"),  # maintenant character
    ic95 = c("10.5–14.1", "13.2–18.4")
  )
  
  cpue_table_femelles <- tibble::tibble(
    methode = c("nb2", "cmp"),
    cpue = c("7.4", "6.8"),  # maintenant character
    ic95 = c("6.1–8.7", "5.5–8.1")
  )
  
  res <- cpue_abondance_table(
    data = specimens,
    cpue_table_tous = cpue_table_tous,
    cpue_table_femelles = cpue_table_femelles,
    best_model_tous = "nb1",
    best_model_femelles = "nb2"
  )
  
  tab <- res$data
  
  expect_s3_class(tab, "data.frame")
  expect_true(all(c("groupe", "abondance", "proportion", "cpue", "ic95", "mf_ratio") %in% names(tab)))
  
  # Groupes avec cpue attendue
  ligne_tous <- tab[tab$groupe == "Tous", ]
  ligne_f <- tab[tab$groupe == "Repro. actifs femelles", ]
  
  expect_equal(ligne_tous$cpue[[1]], "15.8")
  expect_equal(ligne_tous$ic95[[1]], "13.2–18.4")
  
  expect_equal(ligne_f$cpue[[1]], "7.4")
  expect_equal(ligne_f$ic95[[1]], "6.1–8.7")
  
  # Tous les autres groupes doivent avoir des cpue/ic95 à NA_character_
  groupes_sans_cpue <- tab |>
    dplyr::filter(!groupe %in% c("Tous", "Repro. actifs femelles"))
  
  expect_true(all(is.na(groupes_sans_cpue$cpue)))
  expect_true(all(is.na(groupes_sans_cpue$ic95)))
})
