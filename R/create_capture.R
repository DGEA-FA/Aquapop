create_capture <- function(data_station, data_recolte) {
  # Joindre les dataframes
  capture <- left_join(
    x = data_station,
    y = data_recolte,
    by = c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"),
    relationship = "one-to-many"
  ) %>%
    droplevels() %>%
    distinct()
  
  # Vérifier si les colonnes comments.x et comments.y existent
  if ("comments.x" %in% names(capture) && "comments.y" %in% names(capture)) {
    # Renommer les colonnes
    capture <- capture %>%
      rename(comments_recolte = comments.y,
             comments_station = comments.x)
  }
  
  
  # Ajouter nb_capture=0 et nb_pese=0 pour les stations qui ont des NA pour ces valeurs
  capture <- capture %>%
    mutate(nb_capture = ifelse(is.na(nb_capture), 0, nb_capture),
           nb_pese = ifelse(is.na(nb_pese), 0, nb_pese))
  
  return(capture)
}
