load_lac <- function(path, namesheet) {
  lac <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = "text"
  ) %>%
    as.data.frame()
  colnames(lac)[1] <-
    'region_admin' #renommer la 1 colonne "Région administrative"
  colnames(lac)[2] <-
    'no_lac' #renommer la 2 colonne "No plan d'eau"
  colnames(lac)[3] <- 'nom_lac' #renommer la 3 colonne "Nom du lac"
  colnames(lac)[4] <-
    'typ_pech' #renommer la 4 colonne "Type de pêche"
  colnames(lac)[5] <- 'annee' #renommer la 5 colonne "Année"
  colnames(lac)[6] <-
    'sp_pen' #renommer la 6 colonne "Espèce visée code"
  colnames(lac)[7] <-
    'long_dd.dec' #renommer la 7 colonne "Longitude (DD.déc.)"
  colnames(lac)[8] <-
    'lat_dd.dec' #renommer la 8 colonne "Latitude (DD.déc.)"
  colnames(lac)[9] <-
    'terr_faun' #renommer la 9 colonne "Territoire faunique"
  colnames(lac)[10] <-
    'zon_pech' #renommer la 10 colonne "Zone de pêche"
  colnames(lac)[11] <-
    'superficie_ha' #renommer la 11 colonne "Superficie (ha)"
  colnames(lac)[12] <-
    'perimetre_km' #renommer la 12 colonne "Périmètre (km)"
  colnames(lac)[13] <-
    'prof_max_m' #renommer la 13 colonne "Prof. max (m)"
  colnames(lac)[14] <-
    'prof_moy_m' #renommer la 14 colonne "Prof. moy (m)"
  colnames(lac)[15] <-
    'comments' #renommer la 15 colonne "Commentaires généraux"
  lac <-
    dplyr::mutate(lac, ID = paste0(nom_lac, " - ", annee, " - ", typ_pech))
  lac <- mutate_at(
    lac,
    vars(
      region_admin,
      no_lac,
      nom_lac,
      typ_pech,
      sp_pen,
      terr_faun,
      zon_pech,
      annee
    ),
    factor
  ) #transformer en factor
  lac$long_dd.dec <-
    as.numeric(lac$long_dd.dec)#transformer en numeric
  lac$lat_dd.dec <-
    as.numeric(lac$lat_dd.dec)#transformer en numeric
  lac$superficie_ha <-
    as.numeric(lac$superficie_ha)#transformer en numeric
  lac$perimetre_km <-
    as.numeric(lac$perimetre_km)#transformer en numeric
  lac$prof_max_m <-
    as.numeric(lac$prof_max_m)#transformer en numeric
  lac$prof_moy_m <-
    as.numeric(lac$prof_moy_m)#transformer en numeric
  lac$comments <- as.character(lac$comments)#transformer en numeric
  lac %>% dplyr::distinct()
}