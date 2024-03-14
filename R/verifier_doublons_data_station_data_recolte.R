#' Verifie les doublons dans les dataframes
#' 
#' Cette fonction vérifie si les dataframes passés en argument contiennent des lignes en double.
#' 
#' @param data_station Un dataframe représentant les données de la station.
#' @param data_recolte Un dataframe représentant les données de la récolte.
#' @return Un message indiquant s'il y a des doublons dans les dataframes.
#' @export
verifier_doublons_data_station_data_recolte <- function(data_station, data_recolte) {
  # Sélection des colonnes pertinentes
  data_stationverif <- data_station %>% select(c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"))
  data_recolteverif <- data_recolte %>% select(c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"))
  
  # Suppression des doublons et comptage des lignes
  nrow_data_station_unique <- nrow(unique(data_stationverif))
  nrow_data_recolte_unique <- nrow(unique(data_recolteverif))
  
  # Vérification si les nombres de lignes originaux et uniques sont identiques
  if (nrow(data_stationverif) != nrow_data_station_unique) {
    return(
      HTML(paste("<div style='font-weight: bold; color: #CB381F;'>Attention : La base de donnée Stations contient des lignes en double en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station' et 'typ_pech'. Erreur à corriger avant de poursuivre.</div>"))
    )
  } else if (nrow(data_recolteverif) != nrow_data_recolte_unique) {
    return(
      HTML(paste("<div style='font-weight: bold; color: #CB381F;'>Attention : La base de donnée Récolte contient des lignes en double en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station' et 'typ_pech'. Erreur à corriger avant de poursuivre.</div>"))
    )
  } else {
    return("Aucun doublon trouvé dans les bases de données Stations et Récolte en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station' et 'typ_pech'. ")
  }
  
}
