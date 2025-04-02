structure_age <- function(dfspecimen, espece, nomsp, groupement) {
  # Filtrer les données pour l'espèce sélectionnée
  df <- dfspecimen %>%
    mutate(age = as.numeric(age)) %>%
    filter(!is.na(age))
  
  if (nrow(df) == 0) return(NULL)
  
  max_age <- max(df$age, na.rm = TRUE)
  max_y <- ceiling(max(table(df$age), na.rm = TRUE) * 1.1)
  
  # Cas où aucun groupement n'est appliqué
  if (groupement == "tous") {
    p <- ggplot(df, aes(x = age)) +
      geom_histogram(
        binwidth = 1,
        closed = "right",
        color = "white",
        alpha = 1,
        na.rm = TRUE
      ) +
      xlab("Âge") +
      ylab(paste0("Nb. ", nomsp, " échantillonnés")) +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.line = element_line(colour = "black")
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, max_age + 2),
        breaks = seq(0, max_age + 2, 1)
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
    
    return(p)
  }
  
  # Définition des catégories et des couleurs pour chaque type de groupement
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
  
  # S'assurer que toutes les catégories du groupement sont présentes dans df
  df[[groupement]] <- factor(df[[groupement]], levels = names(group_labels[[groupement]]))
  
  # Ajouter une ligne factice pour garantir que toutes les catégories apparaissent dans la légende
  missing_levels <- setdiff(names(group_labels[[groupement]]), unique(df[[groupement]]))
  if (length(missing_levels) > 0) {
    df_add <- data.frame(
      age = 0, 
      temp_fill = factor(missing_levels, levels = names(group_labels[[groupement]]))
    )
    names(df_add)[2] <- groupement
    
    # Ajouter les colonnes manquantes à df_add
    missing_cols <- setdiff(names(df), names(df_add))
    df_add[missing_cols] <- NA
    
    # Ajouter les lignes fictives au dataset
    df <- rbind(df, df_add)
  }
  
  # Création du graphique avec ggplot2
  p <- ggplot(df, aes(x = age, fill = !!sym(groupement))) +
    geom_histogram(
      binwidth = 1,
      closed = "right",
      color = "white",
      alpha = 1,
      position = position_stack(reverse = TRUE),
      na.rm = TRUE
    ) +
    xlab("Âge") +
    ylab(paste0("Nb. ", nomsp, " échantillonnés")) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(colour = "white")
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_age + 2),
      breaks = seq(0, max_age + 2, 1)
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
    scale_fill_manual(
      values = group_colors[[groupement]],
      name = "",
      labels = group_labels[[groupement]],
      drop = FALSE
    )
  
  return(p)
}
