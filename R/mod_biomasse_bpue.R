#' UI - Module BPUE Biomasse
#'
#' @description Panneau affichant les résultats de biomasse (BPUE) dans l'application.
#'
#' @param id Identifiant du module.
#'
#' @noRd
mod_biomasse_bpue_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "BPUE",
    p(
      "Le tableau ci-dessous présente ",
      "la biomasse par unité d’effort (BPUE) par groupe biologique."
    ),
    uiOutput(ns("table_message")),
    withSpinner(uiOutput(ns("table_ui")), type = myspinner),
    download_button_ui(ns("download_table"))
  )
}


#' Server - Module BPUE Biomasse
#'
#' @param id Identifiant du module.
#' @param specimen Données de spécimens (reactive).
#' @param station Données de stations (reactive).
#' @param filename_suffix Suffixe de nom de fichier (reactive).
#'
#' @noRd
#' @importFrom shiny moduleServer req renderUI div
mod_biomasse_bpue_server <- function(id, specimen, station, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    
    res <- reactive({
      req(specimen(), station())
      
      if (nrow(station()) == 0) {
        return(list(
          success = FALSE,
          message = "Aucune station valide n’est disponible pour calculer la biomasse par unité d’effort.",
          data = NULL,
          flextable = NULL
        ))
      }
      
      if (nrow(specimen()) == 0) {
        return(list(
          success = FALSE,
          message = "Aucun spécimen disponible pour calculer la biomasse.",
          data = NULL,
          flextable = NULL
        ))
      }
      
      result <- bpue_generate_biomasse(
        specimen = specimen(),
        station = station()
      )
      
      list(
        success = TRUE,
        message = NULL,
        data = result$data,
        flextable = result$flextable
      )
    })
    
    output$table_message <- renderUI({
      req(res())
      
      if (isTRUE(res()$success)) {
        return(NULL)
      }
      
      div(
        class = "alert-message alert-message-danger",
        res()$message
      )
    })
    
    render_table_flextable(
      "table_ui",
      reactive({
        req(res())
        req(isTRUE(res()$success))
        res()$flextable
      })
    )
    
    render_download_table(
      "download_table",
      data = reactive({
        req(res())
        req(isTRUE(res()$success))
        req(res()$data)
        res()$data
      }),
      filename = reactive(
        build_export_filename("bpue_biomasse", filename_suffix())
      )
    )
  })
}