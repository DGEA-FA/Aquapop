#' UI - Module BPUE Biomasse
#'
#' @description Panneau affichant les résultats de biomasse (BPUE) dans l'application.
#'
#' @param id Identifiant du module
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_biomasse_bpue_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "BPUE",
    p("Le tableau ci-dessous présente la répartition de la biomasse capturée."),
    withSpinner(uiOutput(ns("table_ui")), type = myspinner),
    download_button_ui(ns("download_table"))
  )
}

#' Server - Module BPUE Biomasse
#'
#' @param id Identifiant du module
#' @param specimen Données de spécimens (reactive)
#' @param station Données de stations (reactive)
#' @param filename_suffix Suffixe de nom de fichier (reactive)
#'
#' @noRd
mod_biomasse_bpue_server <- function(id, specimen, station, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    res <- reactive({
      req(specimen(), station())
      bpue_generate_biomasse(data_specimen = specimen(), data_station = station())
    })
    
    render_table_flextable("table_ui", reactive(res()$flextable))
    
    render_download_table(
      "download_table",
      data = reactive(res()$data),
      filename = reactive(build_export_filename("biomasse", filename_suffix()))
    )
  })
}
