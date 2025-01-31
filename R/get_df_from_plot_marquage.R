get_df_from_plot_marquage <- function(plot) {
  temp <- plot
  
  temp <- ggplot_build(temp)$data[[1]]
  temp <- temp %>% select(fill, count, x)
  temp$categorie <- NA
  temp <- temp %>% mutate(categorie = ifelse(fill=="#99CCFF", "Non marqué", categorie))
  temp <- temp %>% mutate(categorie = ifelse(fill=="#084594", "Marqué", categorie))
  temp <- temp %>% select(categorie, count, x)
  temp
}