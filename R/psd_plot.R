psd_plot <- function(data) {
  colnames(data)[4] <- "freq"
  data$freq <- as.numeric(data$freq)
  
  ggpsd1plot  <- ggplot(data,
                        aes(x = Classe, y = freq)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = freq,
                  label = paste0("n = ", n)),
              nudge_y = 3) +
    xlab("Classe de taille") +
    ylab("Fréquence relative (%)") +
    theme(
      panel.background = element_rect(
        fill = "white",
        colour = "white",
        linewidth = 0.5
      ),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y.left = element_text(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.title.y.left = element_text(color = "black",
                                       hjust = 0.5),
      axis.title.x = element_text(color = "black",
                                  hjust = 0.5),
      plot.margin = unit(c(0.5, 0.1, 0.2, 0.1), "cm"),
      axis.line = element_line(colour = "black")
    ) +
    scale_y_continuous(expand = c(0, 0.1),
                       limit = c(0, 100)) +
    scale_x_discrete(
      limits = c(
        "Sous-stock",
        "Stock",
        "Qualité",
        "Préférée",
        "Mémorable",
        "Trophée"
      ),
      labels = c(
        "Sous-stock",
        "Stock",
        "Qualité",
        "Préférée",
        "Mémorable",
        "Trophée"
      )
    )
  
  
  ggpsd1plot
  
}