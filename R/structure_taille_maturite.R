structure_taille_maturite <- function(dfspecimen, espece) {
  #largeur des ticks de l'axe x
  if (espece == "SANA") {
    binwidth <- 50
  } else if (espece == "SAFO" || espece == "SAVI") {
    binwidth <- 20
  }  
  
  df <- dfspecimen %>% filter(sp == espece) %>% droplevels() #sélectionner slmt les sp
  
  # Check if all ltm are NA
  if(all(is.na(df$ltm))) {
    return(NULL)  # Return nothing and exit the function
  }
  
  df <- subset(df, !is.na(ltm))  #removing all records where mesures were missing
  max_ltm <- max(df$ltm) # définir la plus grande valeur de ltm
  
  df <- df %>% select(ltm, maturite)
  
  
  df$maturite <-
    factor(
      df$maturite,
      levels = c(
        "O",
        "N",
        "IND"
      )
    )
  
  #creer pour avoir legende complete even if on a pas tous les niveaux dans notre jeu de donnees
    # Création de la ligne avec maturite "O"
    new_row_O <- data.frame(
      ltm = max_ltm,
      maturite = "O"
    )
    
    # Création de la ligne avec maturite "N"
    new_row_N <- data.frame(
      ltm = max_ltm,
      maturite = "N"
    )
    
    # Création de la ligne avec maturite "IND"
    new_row_IND <- data.frame(
      ltm = max_ltm,
      maturite = "IND"
    )
    
    # Ajout des nouvelles lignes au dataframe
    dfnew <- rbind(df, new_row_O, new_row_N, new_row_IND)
    

  
  
  
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
  hist_data <- ggplot(df, aes(x = ltm)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(clip = "off") # Ajout pour empêcher la suppression des données en dehors de la plage
  
  # Obtention des limites des bins
  bin_limits <- layer_data(hist_data)$x
  
  
  
  
  ggplot(df, aes(x = ltm, fill = maturite)) +
    geom_histogram(
      binwidth = binwidth,
      closed = "right",
      color = "white",
      alpha = 1,
      position = position_stack(reverse = TRUE),  na.rm = TRUE 
    ) +
    geom_histogram(data = dfnew, binwidth = binwidth,aes(x = ltm, fill = maturite), alpha = 0,  na.rm = TRUE ) +
    scale_fill_manual(
      values = c("#084594", "#99CCFF", "#4d4d4d"),
      name = "",
      labels = c("Mature", "Immature", "Indéterminé"),
      drop = FALSE ,
      guide = guide_legend(override.aes = list(
        alpha = 1,
        color ="white"  ))) +
    
    
    xlab("Longueur totale maximale (mm)") +
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
        angle = 0,
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
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_ltm + (binwidth * 2)),
      breaks = seq(from = 0, to = max_ltm + (binwidth * 2), by = binwidth)
    ) +
    scale_y_continuous(
      expand = c(0, 0.2),
      limits = c(0, max(table(cut(df$ltm, breaks = bin_limits))) * 1.1), # Ajout d'un break supplémentaire
      breaks = function(y)
        unique(floor(pretty(seq(
          0, (max(y) + 1) * 1.1
        ))))
    )
}
