#' masse_longueur UI Function
#'
#' @description Module Shiny pour afficher la relation masse-longueur.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel
mod_masse_longueur_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Relation masse-longueur",
    uiOutput(ns("masse_longueur_message")),
    uiOutput(ns("masse_longueur_plot_section")),
    uiOutput(ns("masse_longueur_table_section"))
  )
}

#' masse_longueur Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_masse_longueur_server <- function(id, specimen, filename_suffix) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Résultat du module ----
    masse_longueur_res <- reactive({
      req(specimen())
      masse_longueur_fit(data = specimen())
    })
    
    # Message utilisateur ----
    output$masse_longueur_message <- renderUI({
      
      res <- masse_longueur_res()
      
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
    
    # Données du tableau ----
    table_masse_longueur <- reactive({
      
      res <- masse_longueur_res()
      
      req(isTRUE(res$success))
      req(!is.null(res$data))
      
      res$data
    })
    
    # Section graphique ----
    output$masse_longueur_plot_section <- renderUI({
      
      res <- masse_longueur_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$plot)) {
        return(NULL)
      }
      
      tagList(
        p(
          "La figure suivante représente la relation allométrique entre la longueur totale ",
          "maximale (mm) et la masse (g). L'équation et les paramètres estimés sont indiqués ",
          "sur le graphique."
        ),
        h3("Relation masse-longueur"),
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("plot"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(ns("download_plot"), "Téléchargement du graphique")
        )
      )
    })
    
    # Section tableau ----
    output$masse_longueur_table_section <- renderUI({
      
      res <- masse_longueur_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$flextable)) {
        return(NULL)
      }
      
      tagList(
        br(),
        h3("Tableau des coefficients"),
        uiOutput(ns("table_ui")),
        download_button_ui(ns("download_table"))
      )
    })
    
    # Affichage du graphique ----
    render_plot_ggplot(
      output_id = "plot",
      plot = reactive({
        res <- masse_longueur_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      })
    )
    
    # Téléchargement du graphique ----
    render_download_plot(
      id = "download_plot",
      plot = reactive({
        res <- masse_longueur_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      }),
      filename_suffix = filename_suffix()
    )
    
    # Affichage du tableau ----
    render_table_flextable(
      "table_ui",
      reactive({
        res <- masse_longueur_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$flextable))
        
        res$flextable
      })
    )
    
    # Téléchargement du tableau ----
    render_download_table(
      id = "download_table",
      data = table_masse_longueur,
      filename = reactive(
        build_export_filename("masselongueur", filename_suffix())
      )
    )
  })
}