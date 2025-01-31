get_df_from_plot_tous <- function(plot) {
  temp <- plot
  
  temp <- ggplot_build(temp)$data[[1]]
  temp <- temp %>% select(fill, count, x)
  temp$categorie <- NA

  temp <- temp %>% mutate(categorie = ifelse(fill=="#084594", "Tous", categorie))
  temp <- temp %>% select(categorie, count, x)
  temp
}