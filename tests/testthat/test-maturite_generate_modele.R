test_that("maturite_generate_modele fonctionne pour tous les modèles et liens", {
  set.seed(123)
  
  df <- data.frame(
    ltm = c(
      sample(100:300, 100, replace = TRUE),
      sample(100:300, 100, replace = TRUE)
    ),
    sexe = rep(c("M", "F"), each = 100),
    maturite = rbinom(200, 1, 0.5)
  ) |>
    dplyr::mutate(
      maturite = factor(ifelse(maturite == 1, "O", "N"),
                        levels = c("N", "O"),
                        ordered = TRUE
      ),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  modeles <- c("TLO", "ADD", "COM", "INT")
  liens <- c("logit", "probit", "cloglog")
  
  for (mod in modeles) {
    for (li in liens) {
      res <- maturite_generate_modele(
        data = df,
        variable = "ltm",
        modele = mod,
        lien = li
      )
      
      expect_type(res, "list")
      expect_named(
        res,
        c(
          "success",
          "table_resultats",
          "table_resultats_flextable",
          "commentaire",
          "message",
          "graphique",
          "donnees_ogive"
        )
      )
      
      expect_true(isTRUE(res$success))
      expect_null(res$message)
      
      expect_s3_class(res$graphique, "ggplot")
      expect_s3_class(res$table_resultats_flextable, "flextable")
      expect_true(is.character(res$commentaire) || is.na(res$commentaire))
      expect_s3_class(res$table_resultats, "data.frame")
      expect_s3_class(res$donnees_ogive, "data.frame")
      
      if (mod == "TLO") {
        expect_true(all(c("intervalle", "b0", "b1") %in% names(res$table_resultats)))
        
        cols_50 <- grep("50", names(res$table_resultats), value = TRUE)
        expect_equal(length(cols_50), 1)
      } else {
        cols_50 <- grep("50", names(res$table_resultats), value = TRUE)
        expect_true(
          length(cols_50) >= 2,
          info = glue::glue(
            "Aucune paire de colonnes contenant '50' trouvée dans le modèle {mod} avec le lien {li}."
          )
        )
      }
    }
  }
})

test_that("maturite_generate_modele déclenche une erreur informative si colonnes manquantes", {
  df_bad <- data.frame(
    age = 1:10,
    sexe = rep("F", 10)
  )
  
  expect_error(
    maturite_generate_modele(df_bad, variable = "age"),
    "doit contenir les colonnes"
  )
})

test_that("maturite_generate_modele retourne une structure valide même avec peu de données", {
  df_few <- data.frame(
    ltm = c(120, 130),
    sexe = c("M", "F"),
    maturite = factor(
      c("O", "N"),
      levels = c("N", "O"),
      ordered = TRUE
    )
  )
  
  res <- suppressWarnings(
    maturite_generate_modele(df_few)
  )
  
  expect_type(res, "list")
  expect_named(
    res,
    c(
      "success",
      "table_resultats",
      "table_resultats_flextable",
      "commentaire",
      "message",
      "graphique",
      "donnees_ogive"
    )
  )
  
  expect_true(is.logical(res$success))
  
  if (isTRUE(res$success)) {
    expect_s3_class(res$table_resultats, "data.frame")
    expect_s3_class(res$table_resultats_flextable, "flextable")
    expect_s3_class(res$graphique, "ggplot")
    expect_s3_class(res$donnees_ogive, "data.frame")
    expect_true(is.null(res$message))
  } else {
    expect_null(res$table_resultats)
    expect_null(res$table_resultats_flextable)
    expect_null(res$graphique)
    expect_null(res$donnees_ogive)
    expect_true(is.character(res$message))
  }
})
