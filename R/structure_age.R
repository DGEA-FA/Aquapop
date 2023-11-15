structure_age <- function(data, espece, regroupement) {
  df <-
    data %>% filter(sp == espece) %>% droplevels() #sélectionner slmt les sp
  df <-
    subset(df,!is.na(age))# removing all records where mesures were missing
  max <- max(df$age) # définir la plus grande valeur de age
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
  
  ggplot(df, aes(x = age, fill = {
    if (regroupement == "tous") {
      espece
    } else if (regroupement == "sexe") {
      sexe
    } else if (regroupement == "maturite") {
      maturite
    } else if (regroupement == "marquage") {
      marquage
    }
  })) +
    geom_histogram(
      binwidth = 1,
      closed = "right",
      color = "white",
      alpha = 0.6,
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(
      values = {
        if (regroupement == "tous") {
          c("#084594")
        } else if (regroupement == "sexe") {
          c("#084594", "#99CCFF", "#4d4d4d")
        } else if (regroupement == "maturite") {
          c("#084594", "#99CCFF", "#4d4d4d")
        } else if (regroupement == "marquage") {
          c("#084594", "#99CCFF")
        }
      },
      name = "",
      labels = {
        if (regroupement == "tous") {
          NULL
        } else if (regroupement == "sexe") {
          c("Femelle", "Mâle", "Indéterminé")
        } else if (regroupement == "maturite") {
          c("Mature", "Immature", "Indéterminé")
        } else if (regroupement == "marquage") {
          c("Marqué", "Non marqué")
        }
      },
      drop = FALSE #Mettre la légende même s’il n’y a pas deux couleurs
    ) +
    guides(fill = {
      if (regroupement == "tous") {
        NULL
      } else if (regroupement == "sexe") {
        guide_legend(reverse = FALSE)
      } else if (regroupement == "maturite") {
        guide_legend(reverse = FALSE)
      } else if (regroupement == "marquage") {
        guide_legend(reverse = FALSE)
      }
    }) +
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
      legend.position = {
        if (regroupement == "tous") {
          "none"
        } else if (regroupement == "sexe") {
          "right"
        } else if (regroupement == "maturite") {
          "right"
        } else if (regroupement == "marquage") {
          "right"
        }
      },
      
      axis.line = element_line(colour = "black")
    ) +
    scale_x_continuous(expand = c(0.1, 0.1),
                       breaks = seq(
                         from = 0,
                         to = max + (1 * 2),
                         by = 1
                       )) +
    scale_y_continuous(
      expand = c(0, 0.2) ,
      breaks = function(x)
        unique(floor(pretty(seq(
          0, (max(x) + 10) * 1.1
        ))))
    )
}