structure_taille <- function(dfspecimen, espece, binwidth, nomsp, groupement) {
  df <- dfspecimen %>%
    filter(sp == espece) %>%
    droplevels() %>%
    mutate(ltm = as.numeric(ltm)) %>%
    filter(!is.na(ltm))
  
  if (nrow(df) == 0) return(NULL)
  
  max_ltm <- max(df$ltm, na.rm = TRUE)
  breaks <- seq(0, max_ltm + binwidth, by = binwidth)
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "[")
  df$ltm_interval <- cut(df$ltm, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)
  df$ltm_interval <- factor(df$ltm_interval, levels = labels, ordered = TRUE)
  df <- df %>% filter(!is.na(ltm_interval))
  max_y <- ceiling(max(table(df$ltm_interval), na.rm = TRUE) * 1.1)
  
  if (groupement == "tous") {
    p <- ggplot(df, aes(x = ltm_interval)) +
      geom_bar(
        fill = "#084594",
        color = "white",
        alpha = 1,
        na.rm = TRUE
      ) +
      xlab("Longueur totale maximale (mm)") +
      ylab(paste0("Nb. ", nomsp, " échantillonnés")) +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line = element_line(colour = "black")
      ) +
      scale_x_discrete(name = "Longueur totale maximale (mm)", drop = FALSE, limits = labels) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
    
    return(p)
  }
  
  group_labels <- list(
    "sexe" = c("F" = "Femelle", "M" = "Mâle", "IND" = "Indéterminé"),
    "maturite" = c("O" = "Mature", "N" = "Immature", "IND" = "Indéterminé"),
    "marquage" = c("MA" = "Marqué", "NMA" = "Non marqué")
  )
  
  group_colors <- list(
    "sexe" = c("F" = "#084594", "M" = "#99CCFF", "IND" = "#4d4d4d"),
    "maturite" = c("O" = "#084594", "N" = "#99CCFF", "IND" = "#4d4d4d"),
    "marquage" = c("MA" = "#084594", "NMA" = "#99CCFF")
  )
  
  if (!(groupement %in% names(group_labels))) {
    stop("Groupement invalide. Choisir parmi : 'tous', 'sexe', 'maturite', 'marquage'.")
  }
  
  df[[groupement]] <- fct_expand(as.factor(df[[groupement]]), names(group_labels[[groupement]]))
  df[[groupement]] <- factor(df[[groupement]], levels = names(group_labels[[groupement]]), ordered = TRUE)
  
  # Création de df_legende avec noms complets et couleurs
  df_legende <- data.frame(
    categorie = factor(names(group_labels[[groupement]]), levels = names(group_labels[[groupement]])),
    label = unname(group_labels[[groupement]]),
    color = unname(group_colors[[groupement]])
  )

  p <- ggplot(df, aes(x = ltm_interval, fill = !!sym(groupement))) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      color = "white",
      alpha = 1,
      na.rm = TRUE
    ) +
    
    # Ajout d'une couche invisible pour forcer la légende
    geom_bar(data = df_legende, aes(x = categorie, fill = categorie), 
             alpha = 1, width = 0, show.legend = TRUE, na.rm = TRUE) +
    
    
    xlab("Longueur totale maximale (mm)") +
    ylab(paste0("Nb. ", nomsp, " échantillonnés")) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(colour = "white")
    ) +
    scale_x_discrete(name = "Longueur totale maximale (mm)", drop = FALSE, limits = labels) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
    
    scale_fill_manual(
      values = setNames(df_legende$color, df_legende$categorie),
      name = "",
      labels = setNames(df_legende$label, df_legende$categorie),
      drop = FALSE
    )
  
  
  return(p)
}
