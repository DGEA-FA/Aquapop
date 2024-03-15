#' Verifie les doublons dans le dataframe de récolte
#' 
#' Cette fonction vérifie si le dataframe de récolte contient des lignes en double.
#' 
#' @param data_recolte Un dataframe représentant les données de la récolte.
#' @return Un message indiquant s'il y a des doublons dans le dataframe de récolte.
#' @export
verifier_doublons_data_recolte <- function(data_recolte) {
  # Sélection des colonnes pertinentes
  data_recolteverif <- data_recolte %>% select(c("no_station", "annee", "nom_lac", "no_lac", "typ_pech", "sp"))
  
  # Suppression des doublons et comptage des lignes
  nrow_data_recolte_unique <- nrow(unique(data_recolteverif))
  
  # Vérification si les nombres de lignes originaux et uniques sont identiques
  if (nrow(data_recolteverif) != nrow_data_recolte_unique) {
    return(
      HTML(paste("<div style='font-weight: bold; color: #CB381F;'>Attention : La base de donnée Récolte contient des lignes en double en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station', 'typ_pech' et 'sp'. Erreur à corriger avant de poursuivre.</div>"))
    )
  } else {
    return("Aucun doublon trouvé dans la base de données Récolte en considérant les colonnes 'no_lac', 'nom_lac', 'annee', 'no_station', 'typ_pech' et 'sp'. ")
  }
}