#' taille_masse_age UI Function
#'
#' @description Module Shiny pour l'affichage des statistiques de taille, masse et âge.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel
mod_taille_masse_age_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Taille, masse et âge moyens",
    uiOutput(ns("taille_masse_age_message")),
    uiOutput(ns("taille_masse_age_table_section"))
  )
}

#' taille_masse_age Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_taille_masse_age_server <- function(id, specimen, filename_suffix) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # Résultat du module ----
    
    taille_masse_age_res <- reactive({
      
      req(specimen())
      
      taille_masse_age(data = specimen())
      
    })
    
    
    # Message utilisateur ----
    
    output$taille_masse_age_message <- renderUI({
      
      res <- taille_masse_age_res()
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      titre_message <- if (isTRUE(res$success)) {
        "Attention"
      } else {
        "Analyse non disponible"
      }
      
      couleur_bordure <- if (isTRUE(res$success)) {
        "#d97706"
      } else {
        "#c0392b"
      }
      
      couleur_fond <- if (isTRUE(res$success)) {
        "#fff7e6"
      } else {
        "#fdf2f2"
      }
      
      div(
        style = paste(
          "margin: 15px 0;",
          "padding: 12px 16px;",
          "border-left: 4px solid", couleur_bordure, ";",
          "background-color:", couleur_fond, ";",
          "color: #333;"
        ),
        
        tags$h4(
          style = "margin-top: 0; margin-bottom: 8px;",
          titre_message
        ),
        
        tags$p(
          style = "margin: 0;",
          res$message
        )
      )
    })
    
    
    # Tableau de résultats ----
    
    table_taille_masse_age <- reactive({
      
      res <- taille_masse_age_res()
      
      req(res$success)
      req(!is.null(res$data))
      
      res$data
    })
    
    
    # Section tableau ----
    
    output$taille_masse_age_table_section <- renderUI({
      
      res <- taille_masse_age_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$flextable)) {
        return(NULL)
      }
      
      tagList(
        p(
          "Ce tableau présente, par groupe biologique, le nombre de spécimens mesurés (N), ",
          "la moyenne, l'écart-type (ÉT), ainsi que les valeurs minimale et maximale de ",
          "la longueur totale (LTMax), de la masse et de l'âge."
        ),
        
        withSpinner(
          uiOutput(ns("table")),
          type = myspinner
        ),
        
        download_button_ui(ns("table_dl"))
      )
    })
    
    
    # Affichage du tableau flextable ----
    
    render_table_flextable(
      "table",
      reactive({
        res <- taille_masse_age_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$flextable))
        
        res$flextable
      })
    )
    
    
    # Téléchargement ----
    
    render_download_table(
      id = "table_dl",
      data = table_taille_masse_age,
      filename = reactive(
        build_export_filename("taille_masse_age", filename_suffix())
      ),
      label = "Télécharger le tableau (.xlsx)"
    )
    
  })
}
