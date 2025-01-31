ggmodel_ADD_A50 <- function(df, model, minitable) {
  ageminM <-
    Summarize(age ~ sexe, data = df) %>% filter(sexe == "M") %>% dplyr::select("min") %>% as.numeric()
  agemaxM <-
    Summarize(age ~ sexe, data = df) %>% filter(sexe == "M") %>% dplyr::select("max") %>% as.numeric()
  ageminF <-
    Summarize(age ~ sexe, data = df) %>% filter(sexe == "F") %>% dplyr::select("min") %>% as.numeric()
  agemaxF <-
    Summarize(age ~ sexe, data = df) %>% filter(sexe == "F") %>% dplyr::select("max") %>% as.numeric()
  
  
  newDFM <-
    data.frame(sexe = "M",
               age = seq(from = ageminM, to = agemaxM, by = 1))
  newDFF <-
    data.frame(sexe = "F",
               age = seq(from = ageminF, to = agemaxF, by = 1))
  
  newDF <- rbind(newDFM, newDFF)
  
  newDFpred <- predict(model, newDF, type = "link", se.fit = TRUE)
  maturite <- plogis(newDFpred$fit)
  LL <- plogis(newDFpred$fit - (1.96 * newDFpred$se.fit))
  UL <- plogis(newDFpred$fit + (1.96 * newDFpred$se.fit))
  DATAogive <- cbind(newDF, maturite, LL, UL)
  colnames(DATAogive)[3] <- "maturite"
  colnames(DATAogive)[4] <- "LL"
  colnames(DATAogive)[5] <- "UL"
  
  a <- minitable[3, 2] %>% as.numeric()
  b <- minitable[4, 2] %>% as.numeric()
  A50_M <- minitable[1, 2] %>% as.numeric()
  A50_F <- minitable[2, 2] %>% as.numeric()
  
  
  
  ggplot(data = DATAogive, aes(x = age, y = maturite, color = sexe)) +
    scale_color_manual(values = c("#636363", "#bdbdbd")) +
    geom_line() +
    geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.1) +
    scale_x_continuous(expand = c(0, 0.1),
                       breaks = breaks_extended(only.loose = TRUE)) +
    annotate(
      "segment",
      x = A50_M,
      xend = A50_M,
      y = 0,
      yend = 0.5,
      color = "#bdbdbd",
      lty = 2
    ) +
    annotate(
      "segment",
      x = min(DATAogive$age),
      xend = A50_M,
      y = 0.5,
      yend = 0.5,
      colour = "#bdbdbd",
      lty = 2
    ) + theme_classic() +
    annotate(
      "segment",
      x = A50_F,
      xend = A50_F,
      y = 0,
      yend = 0.5,
      color = "#636363",
      lty = 2
    ) +
    annotate(
      "segment",
      x = min(DATAogive$age),
      xend = A50_F,
      y = 0.5,
      yend = 0.5,
      colour = "#636363",
      lty = 2
    ) + theme_classic() +
    labs(x = "Âge",
         y = "Proportion reproducteur actif") +
    geom_point(data = df,
               mapping = aes(
                 x = age,
                 y = as.numeric(maturite) - 1,
                 color = sexe
               )) +
    labs(color = "sexe") +
    theme(panel.background = element_rect(fill = "white", colour = "black"))
  
  #Notez que le modèle contenant l’interaction .INT permettrait normalement d’obtenir les mêmes estimations de la A50 que
  #l’analyse séparée des données, mais ce n’est pas le cas pour .ADD. Pour quantifier l’incertitude entourant
  #les estimations de A50 des mâles et des femelles du modèle m.sanaAUPA.logit.ADD, on va encore
  #avoir recours à la méthode Delta.
  
}
