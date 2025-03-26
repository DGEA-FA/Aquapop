load_specimen <- function(path, namesheet) {
  # Charger les données à partir du fichier Excel
  specimen <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"), # Considérer ces valeurs comme NA
    col_types = "text"  # Toutes les colonnes en tant que texte
  ) %>%
    as.data.frame() # Convertir en data.frame pour une manipulation plus facile
  
  # Renommer les colonnes
  colnames(specimen) <- c(
    'no_lac',        # 1ère colonne : No plan d'eau fusionné
    'nom_lac',       # 2ème colonne : Nom plan d'eau fusionné
    'typ_pech',      # 3ème colonne : Type de pêche
    'annee',         # 4ème colonne : Année début inventaire
    'no_station',    # 5ème colonne : No station
    'no_specimen',   # 6ème colonne : No spécimen
    'sp',            # 7ème colonne : Espèce code
    'ltm',           # 8ème colonne : Long. totale max (mm)
    'lf',            # 9ème colonne : Longueur à la fourche (mm)
    'masse',         # 10ème colonne : Masse (g)
    'sexe',          # 11ème colonne : Sexe
    'maturite',      # 12ème colonne : Maturité sexuelle
    'age',           # 13ème colonne : Âge 1
    'ind_insec',     # 14ème colonne : Ind. insecte
    'ind_benth',     # 15ème colonne : Ind. benthos
    'ind_planc',     # 16ème colonne : Ind. plancton
    'ind_chyme',     # 17ème colonne : Ind. chyme
    'ind_vide',      # 18ème colonne : Ind. vide
    'ind_poiss',     # 19ème colonne : Ind. poisson
    'poiss1',        # 20ème colonne : Contenu - Poisson 1
    'poiss2',        # 21ème colonne : Contenu - Poisson 2
    'marquage',      # 22ème colonne : Statut marquage
    'comments'       # 23ème colonne : Commentaires
  )
  
  # Remplacer les NA par "IND" pour sexe et maturité, et marquage
  specimen <- specimen %>%
    mutate(
      maturite = tidyr::replace_na(maturite, "IND"),
      sexe = tidyr::replace_na(sexe, "IND"),
      marquage = tidyr::replace_na(marquage, "NMA")
    )
  
  # Convertir en facteur, y compris `sexe` et `maturite` , et marquage
  # specimen <- specimen %>%
  #   mutate_at(
  #     vars(
  #       no_lac, nom_lac, typ_pech, no_station, no_specimen, sp, 
  #       ind_insec, ind_benth, ind_planc, ind_chyme, ind_vide, 
  #       ind_poiss, poiss1, poiss2, sexe, maturite, marquage
  #     ),
  #     factor
  #   )
  
  specimen <- specimen %>%  # Transformer certaines colonnes en facteurs , y compris `sexe` et `maturite` , et marquage
    mutate(across(
      c(no_lac, nom_lac, typ_pech, no_station, no_specimen, sp, 
        ind_insec, ind_benth, ind_planc, ind_chyme, ind_vide, 
        ind_poiss, poiss1, poiss2, sexe, maturite, marquage),
      as.factor
    ))
  
  # Définir les niveaux fixes pour `sexe` et `maturite`, et marquage
  specimen <- specimen %>%
    mutate(
      sexe = factor(sexe, levels = c("F", "M", "IND")),  
      maturite = factor(maturite, levels = c("O", "N", "IND")),
      marquage = factor(marquage, levels = c("MA", "NMA")),
      
    )
  
  specimen$annee <- specimen$annee %>% as.integer()
  specimen$annee <- ifelse(nchar(specimen$annee) == 5, 
                      as.integer(lubridate::year(as.Date(specimen$annee, origin = "1899-12-30"))), 
                      specimen$annee)  
  
  # Convertir d'autres colonnes en numérique / texte
  specimen <- specimen %>%
    mutate(
      ltm = as.numeric(ltm),
      lf = as.numeric(lf),
      masse = as.numeric(masse),
      age = as.numeric(age),
      comments = as.character(comments)
    )
  
  # Trier par no_specimen en ordre croissant
  specimen <- specimen %>%
    mutate(no_specimen_numeric = as.numeric(as.character(no_specimen))) %>%
    arrange(no_specimen_numeric) %>%
    select(-no_specimen_numeric)
  
  # Filtrer si nécessaire les filets expérimentaux
  # specimen <- specimen %>% filter(type_maill %in% c("G", NA)) %>% droplevels()
  
  
  # Supprimer les doublons
  specimen <- specimen %>% dplyr::distinct()
  
  return(specimen)
  
}