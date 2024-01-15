ggmodel_L_L50 <- function(df, model, minitable) {
  ltmminM <-
    Summarize(ltm ~ sexe, data = df) %>% filter(sexe == "M") %>% dplyr::select("min") %>% as.numeric()
  ltmmaxM <-
    Summarize(ltm ~ sexe, data = df) %>% filter(sexe == "M") %>% dplyr::select("max") %>% as.numeric()
  ltmminF <-
    Summarize(ltm ~ sexe, data = df) %>% filter(sexe == "F") %>% dplyr::select("min") %>% as.numeric()
  ltmmaxF <-
    Summarize(ltm ~ sexe, data = df) %>% filter(sexe == "F") %>% dplyr::select("max") %>% as.numeric()
  
  newDFM <-
    data.frame(sexe = "M",
               ltm = seq(from = ltmminM, to = ltmmaxM, by = 1))
  newDFF <-
    data.frame(sexe = "F",
               ltm = seq(from = ltmminF, to = ltmmaxF, by = 1))
  
  newDF <- rbind(newDFM, newDFF)
  
  newDFpred <- predict(model, newDF, type = "link", se.fit = TRUE)
  maturite <- plogis(newDFpred$fit)
  LL <- plogis(newDFpred$fit - (1.96 * newDFpred$se.fit))
  UL <- plogis(newDFpred$fit + (1.96 * newDFpred$se.fit))
  DATAogive <- cbind(newDF, maturite, LL, UL)
  
  colnames(DATAogive)[3] <- "maturite"
  colnames(DATAogive)[4] <- "LL"
  colnames(DATAogive)[5] <- "UL"
  
  a <- minitable[2, 2] %>% as.numeric()
  b <- minitable[3, 2] %>% as.numeric()
  L50 <- minitable[1, 2] %>% as.numeric()
  
  ggplot(data = DATAogive, aes(x = ltm, y = maturite)) +
    geom_line() +
    geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.1) +
    scale_x_continuous(expand = c(0, 0.1),
                       breaks = breaks_extended(only.loose = TRUE)) +
    annotate(
      "segment",
      x = L50,
      xend = L50,
      y = 0,
      yend = 0.5,
      colour = "black",
      lty = 2
    ) +
    annotate(
      "segment",
      x = min(DATAogive$ltm),
      xend = L50,
      y = 0.5,
      yend = 0.5,
      colour = "black",
      lty = 2
    ) +
    theme_classic() +
    labs(x = "Longueur totale maximale (mm)",
         y = "Proportion reproducteur actif") +
    geom_point(data = df,
               mapping = aes(
                 x = ltm,
                 y = as.numeric(maturite) - 1,
                 color = sexe
               )) +
    scale_color_manual(values = c("#636363", "#bdbdbd")) +
    labs(color = "sexe") +
    theme(panel.background = element_rect(fill = "white", colour = "black"))
}
