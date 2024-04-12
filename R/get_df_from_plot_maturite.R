get_df_from_plot_maturite <- function(plot) {
  temp <- plot
  
  temp <- ggplot_build(temp)$data[[1]]
  temp <- temp %>% select(fill, count, x)
  temp$categorie <- NA
  temp <- temp %>% mutate(categorie = ifelse(fill=="#99CCFF", "Immature", categorie))
  temp <- temp %>% mutate(categorie = ifelse(fill=="#4d4d4d", "IND", categorie))
  temp <- temp %>% mutate(categorie = ifelse(fill=="#084594", "Mature", categorie))
  temp <- temp %>% select(categorie, count, x)
  temp
}