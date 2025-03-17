create_df_maturiteltm <- function(specimen_data, species) {
  specimen_data %>%
    filter(sp == species,
           maturite != "IND",
           sexe != "IND", 
           !is.na(ltm)) %>%
    droplevels() %>%
    mutate(maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
           sexe = relevel(factor(sexe), ref = "F")  # Assurer que "FEMELLE" est la catégorie de référence
    ) 
}
