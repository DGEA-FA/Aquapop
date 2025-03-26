load_station <- function(path, namesheet) {
  # Charger les données à partir du fichier Excel
  station <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"), # Considérer ces valeurs comme NA
    col_types = "text"  # Toutes les colonnes en tant que texte
  ) %>%
    as.data.frame() # Convertir en data.frame pour une manipulation plus facile
  
  # Renommer les colonnes
  colnames(station) <- c(
    'no_lac',          # 1ère colonne : No plan d'eau
    'nom_lac',         # 2ème colonne : Nom plan d'eau
    'typ_pech',        # 3ème colonne : Type de pêche
    'annee',           # 4ème colonne : Année
    'no_station',      # 5ème colonne : No station
    'lat_dd.dec',      # 6ème colonne : Latitude (degré, décimales)
    'long_dd.dec',     # 7ème colonne : Longitude (degré, décimales)
    'heure_pose',      # 8ème colonne : Heure de pose du filet
    'min_pose',        # 9ème colonne : Minute - Pose de filet
    'date_leve',       # 10ème colonne : Date de levée du filet
    'heure_leve',      # 11ème colonne : Heure de levée du filet
    'min_leve',        # 12ème colonne : Minute - Levée de filet
    'st_hasard',       # 13ème colonne : Hasard
    'st_valide',       # 14ème colonne : Station valide
    'prof_deb',        # 15ème colonne : Profondeur début (m)
    'prof_fin',        # 16ème colonne : Profondeur fin (m)
    'type_maill',      # 17ème colonne : Type mailles en rive
    'comments'         # 18ème colonne : Commentaires
  )
  

# Remplacer les NA,IND et"-" par "O" dans les colonnes "st_valide" et "st_hasard"
  station <- station %>%
    mutate(
      st_valide = dplyr::case_when(
        is.na(st_valide) | st_valide %in% c("IND", "-") ~ "O",
        TRUE ~ st_valide
      ),
      st_hasard = dplyr::case_when(
        is.na(st_hasard) | st_hasard %in% c("IND", "-") ~ "O",
        TRUE ~ st_hasard
      )
    )

  station <- station %>%  # Transformer certaines colonnes en facteurs
    mutate(across(
      c(no_lac, nom_lac, typ_pech, st_hasard, st_valide, type_maill, no_station),
      as.factor
    ))
  
  # station <- station %>% mutate_at(vars(no_lac, nom_lac, typ_pech, st_hasard, st_valide, type_maill, no_station), factor)

   # Trier les stations par numéro de station
   station <- station[order(station$no_station, decreasing = FALSE),]
   
   # Convertir certaines colonnes 
   station$annee <- station$annee %>% as.integer()
   station$annee <- ifelse(nchar(station$annee) == 5, 
                       as.integer(lubridate::year(as.Date(station$annee, origin = "1899-12-30"))), 
                       station$annee)   
   
      station <- station %>%
     mutate(
       lat_dd.dec = as.numeric(lat_dd.dec),  # Latitude en numérique
       long_dd.dec = as.numeric(long_dd.dec), # Longitude en numérique
       prof_deb = as.numeric(prof_deb),     # Profondeur début en numérique
       prof_fin = as.numeric(prof_fin),     # Profondeur fin en numérique
       comments = as.character(comments)    # Commentaires en caractère
     )
  
  
   # Convertir la colonne 'date_leve' en format Date à partir du format Excel
   station$date_leve <- station$date_leve %>% 
     as.numeric() %>% 
     as.Date(origin = "1899-12-30")
   
   # Calculer la date de pose en supposant que c'est la veille de la levée
   station$date_pose <- station$date_leve - as.difftime(1, unit = "days")
   
   
   # Ajouter des zéros pour les heures et minutes si nécessaire
   station <- station %>%
     mutate(
       min_pose = ifelse(!is.na(min_pose), stringr::str_pad(min_pose, 2, pad = "0"), min_pose),
       heure_pose = ifelse(!is.na(heure_pose), stringr::str_pad(heure_pose, 2, pad = "0"), heure_pose),
       heure_leve = ifelse(!is.na(heure_leve), stringr::str_pad(heure_leve, 2, pad = "0"), heure_leve),
       min_leve = ifelse(!is.na(min_leve), stringr::str_pad(min_leve, 2, pad = "0"), min_leve)
     )
  
   # Construire les colonnes de temps (h_pose et h_leve) en ajoutant les secondes
   station <- station %>%
     mutate(
       h_pose = ifelse(!is.na(heure_pose) & !is.na(min_pose), paste0(heure_pose, ":", min_pose, ":00"), NA),
       h_leve = ifelse(!is.na(heure_leve) & !is.na(min_leve), paste0(heure_leve, ":", min_leve, ":00"), NA)
     )

   # Combiner date et heure pour les colonnes "pose" et "leve"
   station <- station %>%
     mutate(
       pose = as.POSIXct(paste(date_pose, h_pose), format = "%Y-%m-%d %H:%M:%S"),
       leve = as.POSIXct(paste(date_leve, h_leve), format = "%Y-%m-%d %H:%M:%S")
     )
   
   
   # Calculer la durée entre la pose et la levée du filet
   station$duree <- difftime(station$leve, station$pose, units = "auto")
   
   # Supprimer les doublons
   station <- station %>% dplyr::distinct()
   
   return(station)
   
}