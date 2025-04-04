#' Ajuste un modèle de maturité et retourne le résumé
#'
#' @param data Un data.frame contenant au minimum les colonnes `ltm` et `maturite`
#' @param modele_type Type de modèle à ajuster, ex: "TLO_probit", "TLO_logit", "TLO_cloglog"
#' @param nboot Nombre de tirages pour le calcul des IC (méthode Monte Carlo)
#'
#' @return Un list contenant : `minitable`, `plot`, `L50_manual`, `L50_coeff`, `commentaire`
#' @export
modele_maturite_l50 <- function(data, modele_type = "TLO_probit", nboot = 10000) {
  
  data <- prepare_maturite_l50_data(data)
  
  # Vérification des colonnes nécessaires
  stopifnot(all(c("ltm", "maturite") %in% colnames(data)))
  
  # Formule et lien selon le type demandé
  lien <- dplyr::case_when(
    modele_type == "TLO_logit"   ~ "logit",
    modele_type == "TLO_probit"  ~ "probit",
    modele_type == "TLO_cloglog" ~ "cloglog",
    TRUE ~ stop("Type de modèle non supporté : ", modele_type)
  )
  
  # Re-niveau et conversion
  data <- data %>%
    mutate(maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
           ltm = as.numeric(ltm))
  
  # Ajustement du modèle
  modele <- glm(maturite ~ ltm, family = binomial(link = lien), data = data)
  
  # Tests d'ajustement
  pval_fit <- o.r.test(modele)
  eta2 <- predict(modele)^2
  modele_eta2 <- update(modele, . ~ . + eta2)
  pval_link <- anova(modele, modele_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  commentaire <- NA_character_
  if (!modele$converged) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (pval_fit < 0.05 || pval_link < 0.05) {
    commentaire <- "Ce modèle ne s'ajuste pas bien aux données."
  }
  
  # AICc
  aicc <- MuMIn::AICc(modele)
  
  # Coefficients
  b0 <- coef(modele)["(Intercept)"]
  b1 <- coef(modele)["ltm"]
  
  # Estimation de L50 avec IC
  ic <- confint_L(modele, method = "montecarlo", interval_type = "bca", nboot = nboot)
  l50 <- round(ic[2], 0)
  li <- round(ic[1], 0)
  ls <- round(ic[3], 0)
  
  # L50 manuel
  newDF <- tibble(ltm = seq(min(data$ltm), max(data$ltm), by = 1))
  pred <- predict(modele, newdata = newDF, type = "link", se.fit = TRUE)
  DATAogive <- newDF %>%
    mutate(maturite = plogis(pred$fit),
           lim_inf = plogis(pred$fit - 1.96 * pred$se.fit),
           lim_sup = plogis(pred$fit + 1.96 * pred$se.fit))
  
  L50_manual <- DATAogive %>%
    filter(abs(maturite - 0.5) == min(abs(maturite - 0.5))) %>%
    pull(ltm) %>%
    mean()
  
  L50_coeff <- round(-b0 / b1)
  
  # Résumé tabulaire
  minitable <- tibble::tibble(
    modele = modele_type,
    L50 = l50,
    IC95 = glue::glue("[{li}-{ls}]"),
    b0 = round(b0, 3),
    b1 = round(b1, 3),
    AICc = round(aicc, 2),
    pval_fit = signif(pval_fit, 3),
    pval_link = signif(pval_link, 3)
  )
  
  # Graphique
  plot <- ggplot(DATAogive, aes(x = ltm, y = maturite)) +
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), fill = "blue", alpha = 0.1) +
    annotate("segment", x = l50, xend = l50, y = 0, yend = 0.5, lty = 2) +
    annotate("segment", x = min(data$ltm), xend = l50, y = 0.5, yend = 0.5, lty = 2) +
    theme_classic() +
    labs(title = glue::glue("Ogive de maturité ({modele_type})"),
         x = "Longueur (mm)", y = "Proportion mature")
  
  return(list(
    minitable = minitable,
    plot = plot,
    L50_manual = L50_manual,
    L50_coeff = L50_coeff,
    commentaire = commentaire
  ))
}
