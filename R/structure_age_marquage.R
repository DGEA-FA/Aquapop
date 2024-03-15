structure_age_marquage <- function(dfspecimen, espece) {
  
  df <- dfspecimen %>% filter(sp == espece) %>% droplevels() #sélectionner slmt les sp
  df <- subset(df, !is.na(age))  #removing all records where mesures were missing
  max_age <- max(df$age) # définir la plus grande valeur de age
  
  df <- df %>% select(age, marquage)
  
  
  df$marquage <-
    factor(
      df$marquage,
      levels = c(
        "MA",
        "NMA" 
      )
    )
  
  
  # Création de la ligne avec marquage "MA"
  new_row_MA <- data.frame(
    age = max_age,
    marquage = "MA"
  )
  
  # Création de la ligne avec marquage "NMA"
  new_row_NMA <- data.frame(
    age = max_age,
    marquage = "NMA"
  )
  
  # Ajout des nouvelles lignes au dataframe
  dfnew <- rbind(df, new_row_MA, new_row_NMA)
  
  
  
  
  #pour qu'il n'y ait pas de fautes d'orthographes dans le titre de l'axe y du graphique
  nomsp <-  if (espece == "SANA") {
    paste0("touladis")
  } else if (espece == "SAFO") {
    paste0("ombles de fontaine")
  } else if (espece == "SAVI") {
    paste0("dorés jaunes")
  } else {
    NULL
  }
  
  
  
  axeY <- paste0("N des ", nomsp, " échantillonnés")
  
  # Création de la couche geom_histogram sans aes() pour obtenir les informations sur les bins
  hist_data <- ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 1) +
    coord_cartesian(clip = "off") # Ajout pour empêcher la suppression des données en dehors de la plage
  
  # Obtention des limites des bins
  bin_limits <- layer_data(hist_data)$x
  
  
  ggplot(df, aes(x = age, fill = marquage)) +
    geom_histogram(
      binwidth = 1,
      closed = "right",
      color = "white",
      alpha = 1,
      position = position_stack(reverse = TRUE),  na.rm = TRUE 
    ) +
    geom_histogram(data = dfnew, binwidth = 1,aes(x = age, fill = marquage), alpha = 0,  na.rm = TRUE ) +
    scale_fill_manual(
      values = c("#084594", "#99CCFF"),
      name = "",
      labels = c("Marqué", "Non marqué"),
      drop = FALSE ,
      guide = guide_legend(override.aes = list(
        alpha = 1,
        color ="white"  ))) +
    
    
    xlab("Âge") +
    ylab(axeY) +
    theme_classic() +
    theme(
      panel.background = element_rect(
        fill = "white",
        colour = "black",
        linewidth = 0.5
      ),
      panel.grid = element_blank(),
      axis.text.y.left = element_text(color = "black"),
      axis.text.x = element_text(
        color = "black",
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title.y.left = element_text(color = "black",
                                       hjust = 0.5),
      axis.title.x = element_text(color = "black",
                                  hjust = 0.5),
      plot.margin = unit(c(0.5, 0.1, 0.2, 0.1), "cm"),
      legend.position = "right",
      axis.line = element_line(colour = "black"),
      legend.key = element_rect( colour = "white")
    ) +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, max_age + (1 * 2)),
                       breaks = seq(from = 0, to = max_age + (1 * 2), by = 1)) +
    
    
    scale_y_continuous(
      expand = c(0, 0.2),
      limits = c(0, max(table(cut(df$age, breaks = bin_limits))) * 1.1), # Ajout d'un break supplémentaire
      breaks = function(y)
        unique(floor(pretty(seq(
          0, (max(y) + 1) * 1.1
        ))))
    )
}
