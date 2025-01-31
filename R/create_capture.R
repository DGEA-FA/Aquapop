create_capture <- function(data_station, data_recolte) {
 capture <- full_join(
   x = data_station %>% filter(st_valide == "O", st_hasard == "O"),  # Sélectionner les stations valides
   y = data_recolte,
    by = c("no_station", "annee", "no_lac", "typ_pech"), # Suppression de "nom_lac"
    relationship = "one-to-many"
  ) %>%
    droplevels() %>%
    distinct()
  
 # Si "nom_lac" existe dans les deux dataframes, comparer et garder celui de data_station si différent
 if ("nom_lac.x" %in% names(capture) && "nom_lac.y" %in% names(capture)) {
   capture <- capture %>%
     mutate(nom_lac = ifelse(nom_lac.x != nom_lac.y, nom_lac.x, nom_lac.y)) %>%
     select(-nom_lac.x, -nom_lac.y)  # Supprimer les colonnes en double
 }
 
  # Vérifier si les colonnes comments.x et comments.y existent
  if ("comments.x" %in% names(capture) && "comments.y" %in% names(capture)) {
    # Renommer les colonnes
    capture <- capture %>%
      rename(comments_recolte = comments.y,
             comments_station = comments.x)
  }
  
 # Ajouter récolte = 0 pour les stations valides sans récolte
 capture <- capture %>%
   mutate(
     nb_capture = tidyr::replace_na(nb_capture, 0),  # Remplacer NA par 0
     nb_pese = tidyr::replace_na(nb_pese, 0)
   )
  
  return(capture)
}
