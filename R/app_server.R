#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom DT renderDT
#' @importFrom reactable colDef reactable getReactableState colFormat
#' @import shiny 
app_server <- function(input, output, session) {

# Telechargement des donnees ----

  telech <- mod_telechargement_server("telechargement")
  
  # Variables à transmettre aux autres modules
  data_lac              <- telech$data_lac
  data_station          <- telech$data_station
  station_valide       <- telech$station_valide
  station_hasard_valide <- telech$station_hasard_valide
  specimen_tous              <- telech$specimen_tous
  specimen_valide        <- telech$specimen_valide
  specimen_hasard_valide        <- telech$specimen_hasard_valide
  capture               <- telech$capture
  filename_suffix       <- telech$filename_suffix
  nom_lac               <- telech$nom_lac
  
  
  
  # CPUE - Abondance ----
  mod_abondance_cpue_server(
    id = "cpue",
    specimen = specimen_hasard_valide,
    capture = capture,
    filename_suffix = filename_suffix
  )
  
  # BPUE - Biomasse ----
 
  mod_biomasse_bpue_server(
    id = "biomasse",
    specimen = specimen_hasard_valide,
    station = station_hasard_valide,
    filename_suffix = filename_suffix
  )
  
  
  # Taille, masse, age ----
  
  mod_taille_masse_age_server(
    id = "taille_masse_age_1",
    specimen = specimen_valide,
    filename_suffix = filename_suffix
  )
  
  # Structure de taille ----
  
  mod_structure_taille_server("structure_taille_1",
                              specimen = specimen_valide,
                              filename_suffix = filename_suffix)
  
  # Structure d'age ----
  
  mod_structure_age_server("structure_age_1",
                           specimen = specimen_valide,
                           filename_suffix = filename_suffix)
  
  # PSD ----
  mod_psd_server(
    id = "psd_1",
    specimen = specimen_valide,             
    filename_suffix = filename_suffix      
  )
  
  # Relation masse-longueur ----
  
  mod_masse_longueur_server(
    id = "masselongueur_1",
    specimen = specimen_tous,
    filename_suffix = filename_suffix
  )
  
  # Indice de condition ----
  mod_wri_server("wri_1",
                 specimen = specimen_tous,
                 filename_suffix = filename_suffix)
  
  # Croissance ----

  mod_croissance_server("croissance_1",
                        specimen = specimen_tous,
                        filename_suffix = filename_suffix)
  
  # # Mortalite ----

  mod_mortalite_server(
    id = "mortalite_1",
    specimen = specimen_valide,
    filename_suffix = filename_suffix
  )
  # Maturite sexuelle ----
  ## Longueur a maturite ----
  
  mod_maturite_l50_server("maturite_l50_1",
                          specimen = specimen_tous,
                          filename_suffix = filename_suffix)
  
  ## Age a maturite ----
  mod_maturite_a50_server("maturite_a50_1",
                          specimen = specimen_tous,
                          filename_suffix = filename_suffix)
  
  
  
}
