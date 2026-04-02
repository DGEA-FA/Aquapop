#' psd UI Function
#'
#' @description Un module Shiny pour afficher l'indice PSD et les répartitions par classe.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel p h3 div br plotOutput downloadButton
mod_psd_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "PSD",
    
    uiOutput(ns("psd_message")),
    uiOutput(ns("psd_content"))
  )
}

#' psd Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_psd_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultats PSD ----
    
    psd_q_res <- reactive({
      req(specimen())
      psd_q(data = specimen())
    })
    
    psd_byclass_res <- reactive({
      req(specimen())
      psd_byclass(data = specimen())
    })
    
    # Message utilisateur ----
    
    output$psd_message <- renderUI({
      res_q <- psd_q_res()
      res_byclass <- psd_byclass_res()
      
      messages <- c(res_q$message, res_byclass$message)
      messages <- messages[!is.null(messages) & messages != ""]
      
      if (length(messages) == 0) {
        return(NULL)
      }
      
      success_global <- isTRUE(res_q$success) && isTRUE(res_byclass$success)
      
      titre_message <- if (success_global) {
        "Attention"
      } else {
        "Analyse non disponible"
      }
      
      couleur_bordure <- if (success_global) {
        "#d97706"
      } else {
        "#c0392b"
      }
      
      couleur_fond <- if (success_global) {
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
          paste(unique(messages), collapse = " ")
        )
      )
    })
    
    # Contenu principal ----
    
    output$psd_content <- renderUI({
      res_q <- psd_q_res()
      res_byclass <- psd_byclass_res()
      
      if (!isTRUE(res_q$success) || !isTRUE(res_byclass$success)) {
        return(NULL)
      }
      
      tagList(
        p(
          "Autrefois appelé Proportional stock density, l'indice Proportional size distribution ",
          "est un descripteur numérique de la distribution de fréquence des longueurs. ",
          "Il permet de comparer de manière objective la structure de taille de deux populations ",
          "d'une même espèce, ou d'une même population lors de deux inventaires distincts. ",
          "Les classes de taille sont établies en fonction de la taille record enregistrée ",
          "pour une espèce et les autres classes sont dérivées à partir de celle-ci (Gabelhouse 1984)."
        ),
        
        h3("Indice PSD (Q)"),
        withSpinner(
          uiOutput(ns("psd_indice_ui")),
          type = myspinner
        ),
        
        h3("Tableau des répartitions par classe de taille"),
        uiOutput(ns("psd_byclass_table")),
        download_button_ui(ns("psd_byclass_table_dl")),
        
        h3("Distribution de fréquence de longueurs avec les classes de PSD"),
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("psd_byclass_plot"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_psd_byclass_plot"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    # Rendu tableau indice Q ----
    
    render_table_flextable(
      "psd_indice_ui",
      reactive({
        res <- psd_q_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$flextable))
        
        res$flextable
      })
    )
    
    # Rendu tableau par classe ----
    
    render_table_flextable(
      "psd_byclass_table",
      reactive({
        res <- psd_byclass_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$flextable))
        
        res$flextable
      })
    )
    
    # Téléchargement tableau ----
    
    render_download_table(
      id = "psd_byclass_table_dl",
      data = reactive({
        res <- psd_byclass_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$data))
        
        res$data
      }),
      filename = reactive(
        build_export_filename("psd_byclass", filename_suffix())
      )
    )
    
    # Rendu graphique ----
    
    render_plot_ggplot(
      "psd_byclass_plot",
      reactive({
        res <- psd_byclass_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      })
    )
    
    # Téléchargement graphique ----
    
    render_download_plot(
      id = "download_psd_byclass_plot",
      plot = reactive({
        res <- psd_byclass_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      }),
      filename_suffix = filename_suffix()
    )
  })
}
