load_parametres <- function(path, namesheet) {
  # Charger les données à partir du fichier Excel
  parametres <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = c("text", "text", "date", "text", "text", "text", "text", "text")
  ) %>%
    as.data.frame()
  
  # Renommer les colonnes
  colnames(parametres) <- c(
    'no_lac',     # 1ère colonne : No plan d'eau fusionné
    'nom_lac',    # 2ème colonne : Nom plan d'eau fusionné
    'date',       # 3ème colonne : Date
    'typ_pech',   # 4ème colonne : Type de pêche
    'no_station', # 5ème colonne : No station
    'nom_param',  # 6ème colonne : Nom paramètre
    'resultats',    # 7ème colonne : Résultat
    'commentaires'    # 8ème colonne : Commentaires
  )
  
  # Transformer certaines colonnes en facteurs
  parametres <- parametres %>%
    mutate_at(vars(no_lac, nom_lac, typ_pech, no_station, nom_param), factor)
  
  # Transformer la colonne 'resultats' en numérique
  parametres <- parametres %>%
    mutate(resultats = as.numeric(resultats))
  
  # Isoler l'année à partir de la date
  parametres <- parametres %>%
    mutate(annee = format(date, format = "%Y"))
  
  # Transformer la colonne 'commentaires' en caractère
  parametres <- parametres %>%
    mutate(commentaires = as.character(commentaires))
  
  # Supprimer les doublons
  parametres <- parametres %>%
    dplyr::distinct()
  
  return(parametres)
}