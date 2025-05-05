test_that("maturite_generate_modele fonctionne pour tous les modèles et liens", {
  set.seed(123)
  df <- data.frame(
    ltm = c(sample(100:300, 100, replace = TRUE), sample(100:300, 100, replace = TRUE)),
    sexe = rep(c("M", "F"), each = 100),
    maturite = rbinom(200, 1, 0.5)
  ) |>
    dplyr::mutate(
      maturite = factor(ifelse(maturite == 1, "O", "N"), levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  modeles <- c("TLO", "ADD", "COM", "INT")
  liens <- c("logit", "probit", "cloglog")
  
  for (mod in modeles) {
    for (li in liens) {
      res <- maturite_generate_modele(df, variable = "ltm", modele = mod, lien = li)
      expect_type(res, "list")
      expect_named(res, c("table_resultats", "table_resultats_flextable", "commentaire", "graphique", "donnees_ogive"))
      expect_s3_class(res$graphique, "ggplot")
      expect_s3_class(res$table_resultats_flextable, "flextable")
      expect_true(is.character(res$commentaire) || is.na(res$commentaire))
      
      if (mod == "TLO") {
        expect_true(all(c("l50", "intervalle", "b0", "b1") %in% names(res$table_resultats)))
      } else {
        cols_50 <- grep("50", names(res$table_resultats), value = TRUE)
        expect_true(length(cols_50) >= 1, info = glue::glue("Aucune colonne contenant '50' trouvée dans le modèle {mod} avec le lien {li}."))
      }
    }
  }
})

test_that("déclenche une erreur informative si colonnes manquantes", {
  df_bad <- data.frame(age = 1:10, sexe = rep("F", 10))
  expect_error(maturite_generate_modele(df_bad, variable = "age"), "doit contenir les colonnes")
})

test_that("déclenche une erreur si < 10 individus après nettoyage", {
  df_few <- data.frame(
    ltm = c(120, 130),
    sexe = c("M", "F"),
    maturite = factor(c("O", "N"), levels = c("N", "O"), ordered = TRUE)
  )
  expect_error(maturite_generate_modele(df_few), "Trop peu d’individus")
})
