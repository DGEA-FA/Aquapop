create_specimen <- function(data_specimen, data_station) {
  
  # Remplacer les NA par "IND" ou "NMA" dans les colonnes pertinentes de data_specimen
  data_specimen <- data_specimen %>%
    mutate(
      maturite = ifelse(is.na(maturite), "IND", maturite),
      sexe = ifelse(is.na(sexe), "IND", sexe),
      marquage = ifelse(is.na(marquage), "NMA", marquage)
    )
  
  # Convertir les colonnes en facteurs après traitement des NA
  data_specimen <- data_specimen %>%
    mutate_at(
      vars(sexe, maturite, marquage),
      factor
    )
  
 
  # Joindre les dataframes en utilisant des colonnes communes
  # On inclut "nom_lac" dans le "by" car il s'agit d'une colonne commune entre les deux dataframes.
  # Cela permet d'éviter la duplication de cette colonne dans le dataframe final et garantit que les lignes sont correctement appariées.
  # À l'inverse, on n'inclut pas "st_valide" ni "st_hasard" car ces colonnes se trouvent seulement dans data_station. 
  # Toutes les colonnes des deux dataframes sont quand même présentent dans capture après le join.
  # Joindre les dataframes
  specimen <- left_join(
    x = data_specimen,
    y = data_station,
    by = c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) %>%
    droplevels() %>%
    distinct()

  
  # Filtrer pour ne retenir que les stations valides et au hasard
  # Après la jointure, on applique un filtre pour ne conserver que les lignes où st_valide == "O" et st_hasard == "O"
  # Cela garantit que seules les données pertinentes sont conservées pour les analyses ultérieures.
  specimen <- specimen %>%
    filter(st_valide == "O", st_hasard == "O")

    return(specimen)
  
  
}
