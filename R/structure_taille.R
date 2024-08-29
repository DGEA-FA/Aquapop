structure_taille <- function(dfspecimen, espece, binwidth, nomsp, groupement) {
  df <- dfspecimen %>% filter(sp == espece) %>% droplevels()  # select only the species
  
  # Check if all `ltm` are NA
  if(all(is.na(df$ltm))) {
    return(NULL)  # Return nothing and exit the function
  }
  
  df <- subset(df, !is.na(ltm))  # removing all records where measurements were missing
  max_ltm <- max(df$ltm)  # define the maximum value of ltm
  
  # Handle the grouping
  if (groupement == "tous") {
    fill_var <- NULL
    fill_labels <- NULL
    fill_values <- NULL
    df_plot <- df %>% select(ltm)
    aes_params <- aes(x = ltm)
    geom_hist_addition <- NULL
  } else {
    if (groupement == "sexe") {
      fill_var <- "sexe"
      fill_labels <- c("F" = "Femelle", "M" = "Mâle", "IND" = "Indéterminé")
      fill_values <- c("#084594", "#99CCFF", "#4d4d4d")
    } else if (groupement == "maturite") {
      fill_var <- "maturite"
      fill_labels <- c("O" = "Mature", "N" = "Immature", "IND" = "Indéterminé")
      fill_values <- c("#084594", "#99CCFF", "#4d4d4d")
    } else if (groupement == "marquage") {
      fill_var <- "marquage"
      fill_labels <- c("MA" = "Marqué", "NMA" = "Non marqué")
      fill_values <- c("#084594", "#99CCFF")
    }
    
    df[[fill_var]] <- factor(df[[fill_var]], levels = names(fill_labels))
    df_plot <- df %>% select(ltm, !!sym(fill_var))
    aes_params <- aes(x = ltm, fill = !!sym(fill_var))
    
    # Add rows to ensure all levels appear in the legend
    # Assuming `fill_var` is the name of the column as a string, and `lvl` is the value for that column
    new_rows <- lapply(names(fill_labels), function(lvl) {
      # Create a data frame with the correct column name
      df_temp <- data.frame(ltm = max_ltm, stringsAsFactors = FALSE)
      df_temp[[fill_var]] <- lvl
      return(df_temp)
    })
    
    # Combine all the rows into a single data frame
    dfnew <- do.call(rbind, new_rows)
    
    # Create the geom_histogram layer
    geom_hist_addition <- geom_histogram(
      data = dfnew, 
      binwidth = binwidth, 
      aes(x = ltm, fill = !!sym(fill_var)), 
      alpha = 0, 
      na.rm = TRUE
    )
  }
  
  axeY <- paste0("Nb. ", nomsp, " échantillonnés")
  
  # Create histogram to get bin limits
  hist_data <- ggplot(df_plot, aes(x = ltm)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(clip = "off")  # prevent clipping of data outside the range
  
  bin_limits <- layer_data(hist_data)$x
  
  p <- ggplot(df_plot, aes_params) +
    geom_histogram(
      binwidth = binwidth,
      closed = "right",
      color = "white",
      alpha = 1,
      position = position_stack(reverse = TRUE),
      na.rm = TRUE
    ) +
    xlab("Longueur totale maximale (mm)") +
    ylab(axeY) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
      panel.grid = element_blank(),
      axis.text.y.left = element_text(color = "black"),
      axis.text.x = element_text(color = "black", angle = 90, vjust = 0.5, hjust = 1),
      axis.title.y.left = element_text(color = "black", hjust = 0.5),
      axis.title.x = element_text(color = "black", hjust = 0.5),
      plot.margin = unit(c(0.5, 0.1, 0.2, 0.1), "cm"),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(colour = "white")
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_ltm + (binwidth * 2)),
      breaks = seq(from = 0, to = max_ltm + (binwidth * 2), by = binwidth)
    ) +
    scale_y_continuous(
      expand = c(0, 0.2),
      limits = c(0, max(table(cut(df$ltm, breaks = bin_limits))) * 1.1),  # add a break
      breaks = function(y) unique(floor(pretty(seq(0, (max(y) + 1) * 1.1))))
    )
  
  # Add fill scale and additional histogram layer if groupement is not 'tous'
  if (!is.null(fill_var)) {
    p <- p + 
      geom_hist_addition +
      scale_fill_manual(values = fill_values, name = "", labels = fill_labels, drop = FALSE)
  } else {
    p <- p + scale_fill_manual(values = c("#084594"), name = "", labels = c("Tous"), drop = FALSE)
  }
  
  return(p)
}
