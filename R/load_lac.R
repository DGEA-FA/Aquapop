load_lac <- function(path, namesheet) {
  lac <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"),
    col_types = "text"
  ) %>%
    as.data.frame()
  
  
  # Renommer les colonnes
  colnames(lac) <- c(
    'region_admin', 'no_lac', 'nom_lac', 'typ_pech', 'annee',
    'sp_pen', 'long_dd.dec', 'lat_dd.dec', 'terr_faun', 'zon_pech',
    'superficie_ha', 'perimetre_km', 'prof_max_m', 'prof_moy_m', 'comments'
  )
  
  lac$annee <- lac$annee %>% as.integer()
  lac$annee <- ifelse(nchar(lac$annee) == 5, 
                      as.integer(lubridate::year(as.Date(lac$annee, origin = "1899-12-30"))), 
                      lac$annee)  
  
  
  lac <-
    dplyr::mutate(lac, ID = paste0(nom_lac, " - ", annee, " - ", typ_pech))
  lac <- lac %>%
    mutate(across(
      c(region_admin, no_lac, nom_lac, typ_pech, sp_pen, terr_faun, zon_pech),
      as.factor
    ))
  
  # lac <- dplyr::mutate_at(
  #   lac,
  #   vars(
  #     region_admin,
  #     no_lac,
  #     nom_lac,
  #     typ_pech,
  #     sp_pen,
  #     terr_faun,
  #     # annee,
  #     zon_pech
  #   ),
  #   factor
  # ) #transformer en factor
  
  
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
  lac$comments <- as.character(lac$comments)#transformer en character
  lac %>% dplyr::distinct()
}