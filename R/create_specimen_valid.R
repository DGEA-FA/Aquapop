create_specimen_valid <- function(data_specimen, data_station) {
  
  # Filtrer les stations valides avant la jointure (ne pas exclure les stations dirigées)
  data_station <- data_station %>% filter(st_valide == "O") 
  
  # Joindre les spécimens avec les stations filtrées (sans `nom_lac` pour éviter les erreurs)
  specimen_valid <- left_join(
    x = data_specimen,
    y = data_station,
    by = c("no_station", "annee", "no_lac", "typ_pech"),  # Suppression de "nom_lac"
    relationship = "many-to-one"
  ) %>%
    droplevels() %>%
    distinct()
  
  # Comparer `nom_lac.x` (data_station) et `nom_lac.y` (data_specimen) et garder celui de data_station si différent
  if ("nom_lac.x" %in% names(specimen_valid) && "nom_lac.y" %in% names(specimen_valid)) {
    specimen_valid <- specimen_valid %>%
      mutate(nom_lac = ifelse(nom_lac.x != nom_lac.y, nom_lac.x, nom_lac.y)) %>%
      select(-nom_lac.x, -nom_lac.y)  # Supprimer les colonnes en double
  }
  
  return(specimen_valid)
}
