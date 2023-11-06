load_profil <- function(path, namesheet) {
  profil <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = c(
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
      "text",
      "text"
    )
  ) %>%
    as.data.frame()
  colnames(profil)[1] <-
    'no_lac' #renommer la 1 colonne "No plan d'eau fusionné"
  colnames(profil)[2] <-
    'nom_lac' #renommer la 2 colonne "Nom plan d'eau fusionné"
  colnames(profil)[3] <- 'date' #renommer la 3 colonne "date"
  colnames(profil)[4] <-
    'typ_pech' #renommer la 4 colonne "Type de pêche"
  colnames(profil)[5] <-
    'no_station' #renommer la 5 colonne "No station"
  colnames(profil)[6] <-
    'lat_dd.dec' #renommer la 6 colonne "Latitude (degré,décimales)"
  colnames(profil)[7] <-
    'long_dd.dec' #renommer la 7 colonne "Longitude (degré,décimales)"
  colnames(profil)[8] <-
    'prof' #renommer la 8 colonne  "profondeur (m)"
  colnames(profil)[9] <-
    'temperature' #renommer la 9 colonne  "Température (°C)"
  colnames(profil)[10] <-
    'oxygene' #renommer la 10 colonne  "Oxygène (mg/L)"
  colnames(profil)[11] <- 'ph' #renommer la 11 colonne  "ph"
  colnames(profil)[12] <-
    'conductivite' #renommer la 12 colonne   "Conductivité"
  profil <- mutate_at(profil,
                      vars(no_lac,
                           nom_lac,
                           typ_pech,
                           no_station),
                      factor) #transformer en factor
  profil$lat_dd.dec <-
    as.numeric(profil$lat_dd.dec)#transformer en numeric
  profil$long_dd.dec <-
    as.numeric(profil$long_dd.dec)#transformer en numeric
  profil$prof <- as.numeric(profil$prof)#transformer en numeric
  profil$temperature <-
    as.numeric(profil$temperature)#transformer en numeric
  profil$oxygene <-
    as.numeric(profil$oxygene)#transformer en numeric
  profil$ph <- as.numeric(profil$ph)#transformer en numeric
  profil$conductivite <-
    as.numeric(profil$conductivite)#transformer en numeric
  profil$date <-
    as.POSIXct(profil$date, format = "%Y-%m-%d", optional = TRUE) # convertir en format date
  profil <-
    profil %>% mutate(annee = format(date, format = "%Y"))# isoler annee
  profil %>% dplyr::distinct()
}