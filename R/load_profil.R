load_profil <- function(path, namesheet) {
  # Charger les données à partir du fichier Excel
  profil <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = rep("text", 12)  # Toutes les colonnes en tant que texte
  ) %>%
    as.data.frame()
  
  # Renommer les colonnes
  colnames(profil) <- c(
    'no_lac',         # 1ère colonne : No plan d'eau fusionné
    'nom_lac',        # 2ème colonne : Nom plan d'eau fusionné
    'date',           # 3ème colonne : Date
    'typ_pech',       # 4ème colonne : Type de pêche
    'no_station',     # 5ème colonne : No station
    'lat_dd',     # 6ème colonne : Latitude (degré, décimales)
    'long_dd',    # 7ème colonne : Longitude (degré, décimales)
    'prof',           # 8ème colonne : Profondeur (m)
    'temperature',    # 9ème colonne : Température (°C)
    'oxygene',        # 10ème colonne : Oxygène (mg/L)
    'ph',             # 11ème colonne : pH
    'conductivite'    # 12ème colonne : Conductivité
  )
  
  # Transformer certaines colonnes en facteurs
  profil <- profil %>%
    mutate_at(vars(no_lac, nom_lac, typ_pech, no_station), factor)
  
  # Transformer certaines colonnes en numérique
  profil <- profil %>%
    mutate(
      lat_dd = as.numeric(lat_dd),
      long_dd = as.numeric(long_dd),
      prof = as.numeric(prof),
      temperature = as.numeric(temperature),
      oxygene = as.numeric(oxygene),
      ph = as.numeric(ph),
      conductivite = as.numeric(conductivite)
    )
  
  
  # Convertir la colonne 'date' en format Date à partir du format Excel
  profil$date <- profil$date %>% 
    as.numeric() %>% 
    as.Date(origin = "1899-12-30")
  
  # Isoler l'année à partir de la date
  profil <- profil %>%
    mutate(annee = format(date, format = "%Y"))
  
  # Supprimer les doublons
  profil <- profil %>% 
    dplyr::distinct()
  
  return(profil)
}