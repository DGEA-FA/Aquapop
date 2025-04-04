#' Ajuste le modèle INT_probit (interaction sexe x taille)
#'
#' @param data Jeu de données contenant les colonnes `ltm`, `maturite`, et `sexe`
#' @param nboot Nombre de tirages Monte Carlo (défaut: 10000)
#'
#' @return Liste avec `minitable`, `minitable_flextable`, `commentaire`, `plot`, `DATAogive`
#' @export
modele_INT_probit <- function(data, nboot = 10000) {
  if (!all(c("ltm", "maturite", "sexe") %in% names(data))) {
    stop("❌ Le jeu de données doit contenir les colonnes `ltm`, `maturite` et `sexe`.")
  }
  
  # Préparation des données
  df <- prepare_maturite_l50_data(data)
  if (nrow(df) < 10) {
    stop(glue::glue("❌ Trop peu d’individus après nettoyage (n = {nrow(df)})."))
  }
  
  # Modèle probit avec interaction
  modele <- glm(maturite ~ ltm * sexe, family = binomial(link = "probit"), data = df)
  
  # Tests d’ajustement
  pval.fit <- o.r.test(modele)
  eta2 <- predict(modele)^2
  modele_eta2 <- update(modele, . ~ . + eta2)
  pval.link <- anova(modele, modele_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  # Commentaire
  commentaire <- NA_character_
  if (!modele$converged) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (pval.fit < 0.05 || pval.link < 0.05) {
    commentaire <- "Ce modèle ne s’ajuste pas bien aux données."
  }
  
  # Extraction des coefficients
  b0 <- coef(modele)[["(Intercept)"]]
  b1 <- coef(modele)[["ltm"]]
  b2 <- coef(modele)[["sexeM"]]
  b3 <- coef(modele)[["ltm:sexeM"]]
  
  # Calcul L50 coefficients
  l50_F <- -b0 / b1
  l50_M <- (-b0 - b2) / (b1 + b3)
  
  # Plage de valeurs pour chaque sexe
  ltmminM <- min(df$ltm[df$sexe == "M"])
  ltmmaxM <- max(df$ltm[df$sexe == "M"])
  ltmminF <- min(df$ltm[df$sexe == "F"])
  ltmmaxF <- max(df$ltm[df$sexe == "F"])
  
  # Données pour prédictions
  newDF_M <- data.frame(sexe = "M", ltm = seq(ltmminM, ltmmaxM, by = 1))
  newDF_F <- data.frame(sexe = "F", ltm = seq(ltmminF, ltmmaxF, by = 1))
  newDF <- rbind(newDF_M, newDF_F)
  
  newDFpred <- predict(modele, newDF, type = "link", se.fit = TRUE)
  
  # Ogive de maturité
  DATAogive <- newDF %>%
    mutate(
      maturite = pnorm(newDFpred$fit),
      lim_inf = pnorm(newDFpred$fit - (1.96 * newDFpred$se.fit)),
      lim_sup = pnorm(newDFpred$fit + (1.96 * newDFpred$se.fit))
    )
  
  # Estimation visuelle L50 (point où y ≈ 0.5)
  L50_M_manual <- DATAogive %>%
    filter(sexe == "M") %>%
    filter(abs(maturite - 0.5) == min(abs(maturite - 0.5))) %>%
    pull(ltm) %>%
    mean()
  
  L50_F_manual <- DATAogive %>%
    filter(sexe == "F") %>%
    filter(abs(maturite - 0.5) == min(abs(maturite - 0.5))) %>%
    pull(ltm) %>%
    mean()
  
  # AICc
  aicc <- MuMIn::AICc(modele)
  
  # Tableau résumé
  minitable <- tibble::tibble(
    `L50 M (coeff)` = round(l50_M),
    `L50 M (visuel)` = round(L50_M_manual, 1),
    `L50 F (coeff)` = round(l50_F),
    `L50 F (visuel)` = round(L50_F_manual, 1),
    b0 = round(b0, 3),
    b1 = round(b1, 3),
    sexe = round(b2, 3),
    interaction = round(b3, 3),
    AICc = round(aicc, 2),
    pval_fit = signif(pval.fit, 3),
    pval_link = signif(pval.link, 3)
  )
  
  minitable_flextable <- minitable %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      `L50 M (coeff)` = "L50 ♂ (coeff)",
      `L50 M (visuel)` = "L50 ♂ (visuel)",
      `L50 F (coeff)` = "L50 ♀ (coeff)",
      `L50 F (visuel)` = "L50 ♀ (visuel)",
      b0 = "b0",
      b1 = "b1",
      sexe = "b2 (sexeM)",
      interaction = "b3 (interaction)",
      AICc = "AICc",
      pval_fit = "p-val. ajustement",
      pval_link = "p-val. lien"
    ) %>%
    flextable::autofit() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::set_caption("Résumé du modèle INT_probit (interaction sexe x taille)")
  
  # Graphique
  plot <- ggplot(DATAogive, aes(x = ltm, y = maturite, color = sexe)) +
    geom_line() +
    scale_color_manual(values = c("F" = "red", "M" = "black")) +
    geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.1, fill = "blue") +
    annotate("segment", x = l50_M, xend = l50_M, y = 0, yend = 0.5, color = "black", lty = 2) +
    annotate("segment", x = ltmminM, xend = l50_M, y = 0.5, yend = 0.5, color = "black", lty = 2) +
    annotate("segment", x = l50_F, xend = l50_F, y = 0, yend = 0.5, color = "red", lty = 2) +
    annotate("segment", x = ltmminF, xend = l50_F, y = 0.5, yend = 0.5, color = "red", lty = 2) +
    geom_point(data = df,
               aes(x = ltm, y = as.numeric(maturite) - 1, color = sexe),
               alpha = 0.5) +
    theme_classic() +
    labs(x = "Longueur totale maximale (mm)", y = "Proportion reproducteurs actifs", 
         title = "Ogive de maturité (INT_probit)") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "none")
  
  return(list(
    minitable = minitable,
    minitable_flextable = minitable_flextable,
    commentaire = commentaire,
    plot = plot,
    DATAogive = DATAogive
  ))
}
