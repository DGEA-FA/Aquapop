#' Ajuste le modèle TLO_probit (sexes combinés) pour L50 ou A50
#'
#' @param data Jeu de données contenant les colonnes `maturite`, `sexe` et `ltm` ou `age`
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#' @param nboot Nombre de tirages Monte Carlo (défaut: 10000)
#'
#' @return Liste avec `table_resultats`, `table_resultats_flextable`, `commentaire`, `graphique`, `donnees_ogive`
#' @export
fit_maturite_tlo_probit <- function(data, variable = c("ltm", "age"), nboot = 10000) {
  variable <- match.arg(variable)
  
  if (!all(c(variable, "maturite", "sexe") %in% names(data))) {
    stop(glue::glue("❌ Le jeu de données doit contenir les colonnes `{variable}`, `maturite` et `sexe`."))
  }
  
  donnees_modeles <- prepare_maturite_data(data, variable = variable)
  
  if (nrow(donnees_modeles) < 10) {
    stop(glue::glue("❌ Trop peu d’individus après nettoyage (n = {nrow(donnees_modeles)})."))
  }
  
  # Ajustement du modèle ----------------------------------------------------
  if (variable == "ltm") {
    modele <- sans_warning_proba(
      glm(maturite ~ ltm, family = binomial(link = "probit"), data = donnees_modeles))
  } else {
    modele <- sans_warning_proba(
      glm(maturite ~ age, family = binomial(link = "probit"), data = donnees_modeles))
  }
  
  # Tests d'ajustement du modèle ------------------------------------------------
  ## B.1 Osius and Rojek Standardizec Pearson X2 GOODNESS-OF-FIT test
  pval_ajustement <- o.r.test(modele)
  
  ## B.2 GOODNESS-OF-LINK test of McCullagh and Nelder (1989) 
  eta2 <- predict(modele)^2
  modele_avec_eta2 <- sans_warning_proba(update(modele, . ~ . + eta2))
  pval_lien <- anova(modele, modele_avec_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  # Vérification de la convergence du modèle ------------------------------------
  commentaire <- NA_character_
  if (!modele$converged) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (pval_ajustement < 0.05 || pval_lien < 0.05) {
    commentaire <- "Ce modèle ne s’ajuste pas bien aux données. Il est préférable de choisir un autre modèle."
  }
  
  # Estimation du point de 50 % (L50 ou A50) ------------------------------------
  point50_ic <- confint_L(modele, method = "montecarlo", interval_type = "bca", nboot = nboot)
  
  point50 <- round(point50_ic[2])
  point50_inf <- round(point50_ic[1])
  point50_sup <- round(point50_ic[3])
  
  # Calcul des prédictions pour la courbe ---------------------------------------
  x_min <- min(donnees_modeles[[variable]], na.rm = TRUE)
  x_max <- max(donnees_modeles[[variable]], na.rm = TRUE)
  
  donnees_prediction <- data.frame(temp = seq(from = x_min, to = x_max, by = 1))
  names(donnees_prediction) <- variable
  
  prediction_model <- predict(modele,
                              donnees_prediction,
                              full = TRUE,
                              type = "link",
                              se.fit = TRUE)
  
  donnees_ogive <- donnees_prediction %>%
    mutate(
      maturite = pnorm(prediction_model$fit),
      lim_inf = pnorm(prediction_model$fit - 1.96 * prediction_model$se.fit),
      lim_sup = pnorm(prediction_model$fit + 1.96 * prediction_model$se.fit)
    )
  
  # Extraction des coefficients du modèle ---------------------------------------
  intercept <- coef(modele)["(Intercept)"]
  pente <- coef(modele)[[variable]]
  
  # Création d’une table récapitulative -----------------------------------------
  nom_point50 <- ifelse(variable == "ltm", "l50", "a50")
  table_resultats <- data.frame(
    intervalle = glue::glue("[{point50_inf}-{point50_sup}]"),
    b0 = round(intercept, 3),
    b1 = round(pente, 3)
  )
  table_resultats[[nom_point50]] <- point50
  table_resultats <- table_resultats[, c(nom_point50, "intervalle", "b0", "b1")]
  
  
  # Table au format flextable ---------------------------------------------------
  etiquette <- if (variable == "ltm") "L" else "A"
  
  labels <- setNames(
    list(glue::glue("{etiquette}50"), "IC 95%", "b0", "b1"),
    c(nom_point50, "intervalle", "b0", "b1")
  )
  
  ft <- flextable::flextable(table_resultats)
  ft <- do.call(flextable::set_header_labels, c(list(ft), labels))
  table_resultats_flextable <- ft %>%
    flextable::compose(
      j = nom_point50,
      part = "header",
      value = as_paragraph(etiquette, as_sub("50"))
    ) %>%
    flextable::compose(
      j = "b0",
      part = "header",
      value = as_paragraph("b", as_sub("0"))
    ) %>%
    flextable::compose(
      j = "b1",
      part = "header",
      value = as_paragraph("b", as_sub("1"))
    ) %>%
    flextable::autofit() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(part = "header")
  
  
  # Graphique -------------------------------------------------------------------
  graphique <- ggplot(data = donnees_ogive, aes(x = .data[[variable]], y = maturite)) +
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.1, fill = "blue") +
    annotate("segment", x = point50, xend = point50, y = 0, yend = 0.5, color = "black", lty = 2) +
    annotate("segment", x = x_min, xend = point50, y = 0.5, yend = 0.5, color = "black", lty = 2) +
    geom_point(data = donnees_modeles,
               mapping = aes(x = .data[[variable]], y = as.numeric(maturite) - 1),
               color = "blue", alpha = 0.5) +
    theme_classic() +
    labs(
      x = ifelse(variable == "ltm", "Longueur totale maximale (mm)", "Âge"),
      y = "Proportion reproducteurs actifs",
      title = glue("Ogive de maturité")
    ) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "none")
  
  return(list(
    table_resultats = table_resultats,
    table_resultats_flextable = table_resultats_flextable,
    commentaire = commentaire,
    graphique = graphique,
    donnees_ogive = donnees_ogive
  ))
}
