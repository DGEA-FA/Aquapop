structure_taille <- function(data, espece, regroupement) {
  #largeur des ticks de l'axe x
  if (espece == "SANA") {
    binwidth <- 50
  } else if (espece == "SAFO" || espece == "SAVI") {
    binwidth <- 20
  }
  
  df <-
    data %>% filter(sp == espece) %>% droplevels() #sélectionner slmt les sp
  df <-
    subset(df,!is.na(ltm)) #removing all records where mesures were missing
  max <- max(df$ltm) # définir la plus grande valeur de ltm
  n <- length(df$no_specimen) %>% as.numeric() #nb de specimens
  
  
  df$sexe <-
    factor(
      df$sexe,
      levels = c(
        "F",
        "M",
        "IND"
      )
    )
  df$maturite <-
    factor(
      df$maturite,
      levels = c(
        "O",
        "N",
        "IND"
      )
    )
  
  df$marquage <-
    factor(
      df$marquage,
      levels = c(
        "MA",
        "NMA" #ou whatever cest quoi le code que vous voulez
      )
    )
  
  # Vérifier si la colonne 'marquage' contient plus d'un niveau
  if (length(levels(df$marquage)) == 1) {
    # Convertir la colonne 'marquage' en caractère
    df$marquage <- as.character(df$marquage)
    # Factorisation avec deux niveaux distincts
    df$marquage <- factor(df$marquage, levels = c("MA", "NMA"))
  }
  
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
  
  
  colorchart <- if (regroupement == "tous") {
      c("#084594")
    } else if (regroupement == "sexe") {
      c("#084594", "#99CCFF", "#4d4d4d")
    } else if (regroupement == "maturite") {
      c("#084594", "#99CCFF", "#4d4d4d")
    } else if (regroupement == "marquage") {
      c("#084594", "#99CCFF")
    }
  
  fillval <- if (regroupement == "tous") {
      df$espece
    } else if (regroupement == "sexe") {
    df$sexe
    } else if (regroupement == "maturite") {
      df$maturite
    } else if (regroupement == "marquage") {
      df$marquage
    }
  
  labelsval <- if (regroupement == "tous") {
      NULL
    } else if (regroupement == "sexe") {
      c("Femelle", "Mâle", "Indéterminé")
    } else if (regroupement == "maturite") {
      c("Mature", "Immature", "Indéterminé")
    } else if (regroupement == "marquage") {
      c("Marqué", "Non marqué")
    }

  
  fillval2 <- if (regroupement == "tous") {
      NULL
    } else if (regroupement == "sexe") {
      guide_legend(reverse = FALSE)
    } else if (regroupement == "maturite") {
      guide_legend(reverse = FALSE)
    } else if (regroupement == "marquage") {
      # guide_legend(reverse = FALSE)
      guide_legend(override.aes = list(alpha = 1,fill=c('lightskyblue1', 'lightpink')), reverse = FALSE)
      
    }
  
  legend.positionval <-if (regroupement == "tous") {
      "none"
    } else if (regroupement == "sexe") {
      "right"
    } else if (regroupement == "maturite") {
      "right"
    } else if (regroupement == "marquage") {
      "right"
    }
  
  
  
  axeY <- paste0("N des ", nomsp, " échantillonnés")
  
ggplot(df, aes(x = ltm, fill = fillval)) +
    geom_histogram(
      binwidth = binwidth,
      closed = "right",
      color = "white",
      alpha = 0.6,
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(
      values = colorchart,
      
      name = "",
      labels = labelsval,
      drop = FALSE #Mettre la légende même s’il n’y a pas deux couleurs
    ) +
    guides(fill = fillval2) +
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
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title.y.left = element_text(color = "black",
                                       hjust = 0.5),
      axis.title.x = element_text(color = "black",
                                  hjust = 0.5),
      plot.margin = unit(c(0.5, 0.1, 0.2, 0.1), "cm"),
      legend.position = legend.positionval,
      axis.line = element_line(colour = "black")
    ) +
    scale_x_continuous(expand = c(0.1, 0.1),
                       breaks = seq(
                         from = 0,
                         to = max + (binwidth * 2),
                         by = binwidth
                       )) +
    scale_y_continuous(
      expand = c(0, 0.2) ,
      breaks = function(x)
        unique(floor(pretty(seq(
          0, (max(x) + 1) * 1.1
        ))))
    )
  
}