#' Verifie les doublons dans le dataframe des stations
#' 
#' Cette fonction vérifie si le dataframe des stations contient des lignes en double.
#' 
#' @param data_station Un dataframe représentant les données de la station.
#' @return Un message indiquant s'il y a des doublons dans le dataframe des stations.
#' @export
verifier_doublons_data_station <- function(data_station) {
  # Sélection des colonnes pertinentes
  data_stationverif <- data_station %>% select(c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"))
  
  # Suppression des doublons et comptage des lignes
  nrow_data_station_unique <- nrow(unique(data_stationverif))
  
  # Vérification si les nombres de lignes originaux et uniques sont identiques
  if (nrow(data_stationverif) != nrow_data_station_unique) {
    return(
      HTML(paste("<div style='font-weight: bold; color: #CB381F;'>Attention : La base de donnée Stations contient des lignes en double en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station' et 'typ_pech'. Erreur à corriger avant de poursuivre.</div>"))
    )
  } else {
    return("Aucun doublon trouvé dans la base de données Stations en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station' et 'typ_pech'. ")
  }
}
