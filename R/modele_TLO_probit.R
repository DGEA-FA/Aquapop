#' Ajuste le modèle TLO_probit (sexes combinés)
#'
#' @param data Jeu de données contenant les colonnes `ltm`, `maturite`, et `sexe`
#' @param nboot Nombre de tirages Monte Carlo (défaut: 10000)
#'
#' @return Liste avec `minitable`, `minitable_flextable`, `commentaire`, `plot`, `DATAogive`
#' @export
modele_TLO_probit <- function(data, nboot = 10000) {
  if (!all(c("ltm", "maturite", "sexe") %in% names(data))) {
    stop("❌ Le jeu de données doit contenir les colonnes `ltm`, `maturite` et `sexe`.")
  }
  
  df <- prepare_maturite_l50_data(data)
  
  if (nrow(df) < 10) {
    stop(glue::glue("❌ Trop peu d’individus après nettoyage (n = {nrow(df)})."))
  }
  
  modele <- glm(maturite ~ ltm, family = binomial(link = "probit"), data = df)
  
  pval.fit <- o.r.test(modele)
  eta2 <- predict(modele)^2
  modele_eta2 <- update(modele, . ~ . + eta2)
  pval.link <- anova(modele, modele_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  commentaire <- NA_character_
  if (!modele$converged) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (pval.fit < 0.05 || pval.link < 0.05) {
    commentaire <- "Ce modèle ne s’ajuste pas bien aux données."
  }
  
  aicc <- MuMIn::AICc(modele)
  b0 <- coef(modele)["(Intercept)"]
  b1 <- coef(modele)["ltm"]
  l50_ic <- confint_L(modele, method = "montecarlo", interval_type = "bca", nboot = nboot)
  l50 <- round(l50_ic[2])
  li <- round(l50_ic[1])
  ls <- round(l50_ic[3])
  
  ltm_seq <- seq(min(df$ltm), max(df$ltm), by = 1)
  newDF <- data.frame(ltm = ltm_seq)
  pred <- predict(modele, newdata = newDF, type = "link", se.fit = TRUE)
  
  DATAogive <- newDF %>%
    mutate(
      maturite = pnorm(pred$fit),
      lim_inf = pnorm(pred$fit - 1.96 * pred$se.fit),
      lim_sup = pnorm(pred$fit + 1.96 * pred$se.fit)
    )
  
  L50_manual <- DATAogive %>%
    filter(abs(maturite - 0.5) == min(abs(maturite - 0.5))) %>%
    pull(ltm) %>%
    mean()
  
  L50_coeff <- round(-b0 / b1)
  
  minitable <- tibble::tibble(
    L50_montecarlo = round(l50_ic[2], 1),
    IC95_montecarlo = glue::glue("[{li}-{ls}]"),
    L50_visuel = round(L50_manual, 1),
    L50_coeff = L50_coeff,
    b0 = round(b0, 3),
    b1 = round(b1, 3),
    AICc = round(aicc, 2),
    pval_fit = signif(pval.fit, 3),
    pval_link = signif(pval.link, 3)
  )
  
  minitable_flextable <- minitable %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      L50_montecarlo = "L50 (Monte Carlo)",
      IC95_montecarlo = "IC95%",
      L50_visuel = "L50 (visuel)",
      L50_coeff = "L50 (coefficients)",
      b0 = "b0 (intercept)",
      b1 = "b1 (pente)",
      AICc = "AICc",
      pval_fit = "p-val. ajustement",
      pval_link = "p-val. lien"
    ) %>%
    flextable::autofit() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::set_caption("Résumé du modèle TLO_probit (sexes combinés)")
  
  plot <- ggplot(DATAogive, aes(x = ltm, y = maturite)) +
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), fill = "blue", alpha = 0.1) +
    annotate("segment", x = l50, xend = l50, y = 0, yend = 0.5, lty = 2) +
    annotate("segment", x = min(df$ltm), xend = l50, y = 0.5, yend = 0.5, lty = 2) +
    theme_classic() +
    labs(title = "Ogive de maturité (TLO_probit, sexes combinés)",
         x = "Longueur (mm)", y = "Proportion mature")
  
  return(list(
    minitable = minitable,
    minitable_flextable = minitable_flextable,
    commentaire = commentaire,
    plot = plot,
    DATAogive = DATAogive
  ))
}
