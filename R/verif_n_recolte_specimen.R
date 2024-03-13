# Définition de la fonction verif_n_recolte_specimen
verif_n_recolte_specimen <- function(capture, specimen, espece) {
  
  # Filtrer les données de capture pour l'espèce spécifiée et supprimer les niveaux de facteur inutilisés
  datacapt <- capture %>% dplyr::filter(sp == espece) %>% droplevels()
  
  # Sélectionner les colonnes pertinentes dans les données de capture
  datacapt <- datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese")
  
  # Calculer le nombre total de captures pour l'espèce spécifiée
  n_capture <- sum(datacapt$nb_capture)
  
  # Filtrer les données de specimen pour l'espèce spécifiée et supprimer les niveaux de facteur inutilisés
  dataspec <- specimen %>% dplyr::filter(sp == espece) %>% droplevels()
  
  # Calculer le nombre total de spécimens pour l'espèce spécifiée
  n_specimen <- length(specimen$no_specimen) %>% as.numeric()
  
  # Créer un tableau contenant les nombres de captures et de spécimens pour l'espèce spécifiée
  tableau <- cbind("Data" = c("Recolte", "Specimen"), "N" = c(n_capture, n_specimen))
  
  # Retourner un tableau
  tableau
}