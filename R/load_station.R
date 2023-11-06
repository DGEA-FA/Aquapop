load_station <- function(path, namesheet) {
  station <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text"
    )
  ) %>%
    as.data.frame()
  colnames(station)[1] <-
    'no_lac' #renommer la 1 colonne "No plan d'eau"
  colnames(station)[2] <-
    'nom_lac' #renommer la 2 colonne "Nom plan d'eau"
  colnames(station)[3] <-
    'typ_pech' #renommer la 3 colonne "Type de pêche"
  colnames(station)[4] <- 'annee' #renommer la 4 colonne "Année"
  colnames(station)[5] <-
    'no_station' #renommer la 5 colonne "No station"
  colnames(station)[6] <-
    'lat_dd.dec' #renommer la 6 colonne  "Latitude (degré,décimales)"
  colnames(station)[7] <-
    'long_dd.dec' #renommer la 7 colonne "Longitude (degré,décimales)"
  colnames(station)[8] <-
    'heure_pose' #renommer la 8 colonne "Heure de pose du filet"
  colnames(station)[9] <-
    'min_pose' #renommer la 9 colonne "Minute - Pose de filet"
  colnames(station)[10] <-
    'date_leve' #renommer la 10 colonne "Date de levée du filet"
  colnames(station)[11] <-
    'heure_leve' #renommer la 11 colonne "Heure de levée du filet"
  colnames(station)[12] <-
    'min_leve' #renommer la 12 colonne "Minute - Levée de filet (MM)"
  colnames(station)[13] <-
    'st_hasard' #renommer la 13 colonne "Hasard"
  colnames(station)[14] <-
    'st_valide' #renommer la 14 colonne "Station valide"
  colnames(station)[15] <-
    'prof_deb' #renommer la 15 colonne  "Profondeur début (m)"
  colnames(station)[16] <-
    'prof_fin' #renommer la 16 colonne  "Profondeur fin (m)"
  colnames(station)[17] <-
    'type_maill' #renommer la 17 colonne  "Type mailles en rive"
  colnames(station)[18] <-
    'comments' #renommer la 18 colonne "Commentaires"
  station <- mutate_at(
    station,
    vars(
      no_lac,
      nom_lac,
      typ_pech,
      annee,
      st_hasard,
      st_valide,
      type_maill
    ),
    factor
  ) #transformer en factor
  station$no_station <-
    as.factor(as.numeric(station$no_station))#transformer en numeric d'abord pour ordre
  station$lat_dd.dec <-
    as.numeric(station$lat_dd.dec)#transformer en numeric
  station$long_dd.dec <-
    as.numeric(station$long_dd.dec)#transformer en numeric
  station$prof_deb <-
    as.numeric(station$prof_deb)#transformer en numeric
  station$prof_fin <-
    as.numeric(station$prof_fin)#transformer en numeric
  station$comments <-
    as.character(station$comments)#transformer en character
  station$date_leve <-
    as.POSIXct(station$date_leve, format = "%Y-%m-%d", optional = TRUE) # convertir en format date
  station$date_pose <-
    station$date_leve - as.difftime(1, unit = "days") # je suppose que pose le jour d'avant UPDATE OUI
  station$min_pose <-
    stringr::str_pad(station$min_pose,
                     2,
                     side = c("left"),
                     pad = "0") # ajouter des 0 avant si le nombre est seulement 1 caractere ex: 1 min devient 01 min
  station$heure_pose <-
    stringr::str_pad(station$heure_pose,
                     2,
                     side = c("left"),
                     pad = "0") # ajouter des 0 avant si le nombre est seulement 1 caractere ex: 1 min devient 01 min
  station$heure_leve <-
    stringr::str_pad(station$heure_leve,
                     2,
                     side = c("left"),
                     pad = "0") # ajouter des 0 avant si le nombre est seulement 1 caractere ex: 1 min devient 01 min
  station$min_leve <-
    stringr::str_pad(station$min_leve,
                     2,
                     side = c("left"),
                     pad = "0") # ajouter des 0 avant si le nombre est seulement 1 caractere ex: 1 min devient 01 min
  station$h_pose <-
    paste(station$heure_pose , station$min_pose , sep = ":") #construire un format d'heure qui se tient
  station$h_leve <-
    paste(station$heure_leve , station$min_leve , sep = ":") #construire un format d'heure qui se tient
  station$h_pose <-
    chron::times(paste0(station$h_pose, ":00")) #convertir en format heure et ajouter les secondes
  station$h_leve <-
    chron::times(paste0(station$h_leve, ":00"))#convertir en format heure et ajouter les secondes
  station$pose <-
    as.POSIXct(
      paste(station$date_pose, station$h_pose),
      format = "%Y-%m-%d %H:%M:%S",
      optional = TRUE
    ) #merge la date et l'heure en un seul objet (facilite les calculs de difference de temps)
  station$leve <-
    as.POSIXct(
      paste(station$date_leve, station$h_leve),
      format = "%Y-%m-%d %H:%M:%S",
      optional = TRUE
    ) #merge la date et l'heure en un seul objet (facilite les calculs de difference de temps)
  station$duration <-
    difftime(station$leve , station$pose, units = "auto") # revoir les units selon les calculs subsequents
  station %>% dplyr::distinct()
}