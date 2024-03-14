create_specimen <- function(data_specimen, data_station) {
 
    # Joindre les dataframes
  specimen <- left_join(
    x = data_specimen,
    y = data_station,
    by = c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) %>%
    droplevels() %>%
    distinct()

  
}
