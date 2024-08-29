create_capture <- function(data_station, data_recolte) {
  # Joindre les dataframes en utilisant des colonnes communes
  # On inclut "nom_lac" dans le "by" car il s'agit d'une colonne commune entre les deux dataframes.
  # Cela permet d'éviter la duplication de cette colonne dans le dataframe final et garantit que les lignes sont correctement appariées.
  # À l'inverse, on n'inclut pas "st_valide" ni "st_hasard" car ces colonnes se trouvent seulement dans data_station. 
  # Toutes les colonnes des deux dataframes sont quand même présentent dans capture après le join.
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
  
  
  # Filtrer pour ne retenir que les stations valides et au hasard
  # Après la jointure, on applique un filtre pour ne conserver que les lignes où st_valide == "O" et st_hasard == "O"
  # Cela garantit que seules les données pertinentes sont conservées pour les analyses ultérieures.
  capture <- capture %>%
    filter(st_valide == "O", st_hasard == "O")
  
  
  # Ajouter nb_capture=0 et nb_pese=0 pour les stations qui ont des NA pour ces valeurs
  capture <- capture %>%
    mutate(nb_capture = ifelse(is.na(nb_capture), 0, nb_capture),
           nb_pese = ifelse(is.na(nb_pese), 0, nb_pese))
  
  return(capture)
}
