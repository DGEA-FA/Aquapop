get_df_from_plot <- function(plot, groupement) {
  temp <- ggplot_build(plot)$data[[1]]
  temp <- temp %>% select(fill, count, x)
  temp$categorie <- NA
  
  # Define the fill-to-category mapping based on the groupement
  fill_to_category <- switch(groupement,
                             "marquage" = c("#99CCFF" = "Non marqué", "#084594" = "Marqué"),
                             "maturite" = c("#99CCFF" = "Immature", "#4d4d4d" = "IND", "#084594" = "Mature"),
                             "sexe" = c("#99CCFF" = "M", "#4d4d4d" = "IND", "#084594" = "F"),
                             "tous" = c("#084594" = "Tous"),
                             stop("Invalid groupement")
  )
  
  # Apply the mapping
  temp$categorie <- fill_to_category[temp$fill]
  
  # Select the relevant columns
  temp <- temp %>% select(categorie, count, x)
  return(temp)
}
