relation_masse_longueur <- function(data, espece) {
  # Filtrer les données pour l'espèce sélectionnée
  dfAllometrie <- data %>%
    filter(sp == espece) %>%
    droplevels() %>%
    filter(!is.na(ltm), !is.na(masse)) %>% # Retirer les valeurs manquantes
    mutate(
      logW = log10(masse), # Variable réponse en log10
      logL = log10(ltm) # Variable explicative en log10
    )
  
  # Ajustement du modèle de régression
  fit1 <- lm(logW ~ logL, data = dfAllometrie)
  
  # Extraction des coefficients avec erreurs standard et IC95%
  coef_summary <- summary(fit1)$coefficients
  a <- coef_summary[1, 1] %>% round(3) # Intercept (log10(a))
  b <- coef_summary[2, 1] %>% round(3) # Pente (b)
  se_a <- coef_summary[1, 2] %>% round(3) # SE de log10(a)
  se_b <- coef_summary[2, 2] %>% round(3) # SE de b
  
  # Calcul des intervalles de confiance à 95%
  conf_int <- confint(fit1)
  ic_a <- paste0("[", round(conf_int[1, 1], 3), " - ", round(conf_int[1, 2], 3), "]")
  ic_b <- paste0("[", round(conf_int[2, 1], 3), " - ", round(conf_int[2, 2], 3), "]")
  
  # Création d'un tableau des coefficients
  coef_table <- data.frame(
    Coefficient = c("log10(a)", "b"),
    Estimate = c(a, b),
    SE = c(se_a, se_b),
    IC95 = c(ic_a, ic_b)
  )
  
  # Prédiction pour la courbe ajustée
  tmp <- range(dfAllometrie$logL)
  xs <- seq(tmp[1], tmp[2], length.out = 99)
  ys <- predict(fit1, data.frame(logL = xs))
  
  # Correction pour la transformation inverse
  cf <- FSA::logbtcf(fit1, 10)
  btys <- cf * 10 ^ predict(fit1, data.frame(logL = xs), interval = "prediction")
  btxs <- 10 ^ xs
  PREDICT <- data.frame(btxs, btys)
  
 
  # Création du graphique
  ggRelationML <- suppressWarnings(
    ggplot() +
      geom_point(data = dfAllometrie, aes(
        x = ltm, y = masse,
        text = paste0("<b># spécimen:</b> ", no_specimen, "<br>")
      )) +
      geom_line(data = PREDICT, aes(x = btxs, y = fit), color = "blue") +
      geom_line(data = PREDICT, aes(x = btxs, y = lwr), linetype = 2, color = "red") +
      geom_line(data = PREDICT, aes(x = btxs, y = upr), linetype = 2, color = "red") +
      theme_classic() +
      labs(
        title = paste("Relation masse-longueur pour", espece),
        x = "Longueur totale maximale (mm)",
        y = "Masse (g)"
      ) 
  )
  
  return(list(graph = ggRelationML, table = coef_table))
}
