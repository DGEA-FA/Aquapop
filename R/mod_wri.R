#' wri UI Function
#'
#' @description Un module Shiny pour afficher l'indice de condition (Wr).
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel
mod_wri_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Indice de condition",
    uiOutput(ns("wri_message")),
    uiOutput(ns("wri_table_section")),
    uiOutput(ns("wri_plot_tous_section")),
    uiOutput(ns("wri_plot_byclass_section"))
  )
}

#' wri Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_wri_server <- function(id, specimen, filename_suffix) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # Résultat du module ----
    
    wri_res <- reactive({
      
      req(specimen())
      
      wri(data = specimen())
    })
    
    
    # Message utilisateur ----
    
    output$wri_message <- renderUI({
      
      res <- wri_res()
      
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
    
    
    # Tableau brut ----
    
    table_wri <- reactive({
      
      res <- wri_res()
      
      req(isTRUE(res$success))
      req(!is.null(res$data))
      
      res$data
    })
    
    
    # Section tableau ----
    
    output$wri_table_section <- renderUI({
      
      res <- wri_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$flextable)) {
        return(NULL)
      }
      
      tagList(
        p(
          "Le tableau ci-dessous présente l'indice de masse relative (Wr) et son intervalle ",
          "de confiance à 95 % pour les individus à partir d'une taille minimale déterminée ",
          "selon l'espèce, par sexe et par classe de PSD (classes selon Gabelhouse 1984)."
        ),
        
        withSpinner(
          uiOutput(ns("wri_table")),
          type = myspinner
        ),
        
        download_button_ui(ns("wri_table_dl"))
      )
    })
    
    
    # Affichage du tableau flextable ----
    
    render_table_flextable(
      "wri_table",
      reactive({
        res <- wri_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$flextable))
        
        res$flextable
      })
    )
    
    
    # Téléchargement du tableau ----
    
    render_download_table(
      id = "wri_table_dl",
      data = table_wri,
      filename = reactive(
        build_export_filename("wri", filename_suffix())
      ),
      label = "Télécharger le tableau (.xlsx)"
    )
    
    
    # Section graphique Wr selon la longueur et le sexe ----
    
    output$wri_plot_tous_section <- renderUI({
      
      res <- wri_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$plot_tous)) {
        return(NULL)
      }
      
      tagList(
        br(),
        
        h3("Indice de condition (Wr) selon la longueur et le sexe"),
        
        p(
          "Le graphique suivant illustre, pour chaque spécimen capturé, l'indice de condition en ",
          "fonction de la longueur totale maximale et du sexe. La valeur moyenne est indiquée par une ",
          "ligne pointillée en rouge (tous), en bleu foncé (femelles) et en bleu pâle (mâles). ",
          "La ligne grise représente la référence standard pour l'espèce selon Hyatt et Hubert 2011 (SAFO), ",
          "Murphy et al. 1990 (SAVI) et Piccolo et al. 1993 (SANA)."
        ),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("wri_plot_tous"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_wri_plot_tous"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    
    # Affichage du graphique Wr tous ----
    
    render_plot_ggplot(
      "wri_plot_tous",
      reactive({
        res <- wri_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot_tous))
        
        res$plot_tous
      })
    )
    
    
    # Téléchargement du graphique Wr tous ----
    
    render_download_plot(
      id = "download_wri_plot_tous",
      plot = reactive({
        res <- wri_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot_tous))
        
        res$plot_tous
      }),
      filename_suffix = filename_suffix()
    )
    
    
    # Section graphique Wr par classe de taille ----
    
    output$wri_plot_byclass_section <- renderUI({
      
      res <- wri_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$plot_byclass)) {
        return(NULL)
      }
      
      tagList(
        br(),
        
        h3("Indice de condition (Wr) moyen par classe de taille"),
        
        p(
          "Ce graphique présente la variation de l'indice de condition selon les classes de PSD. ",
          "Les valeurs moyennes et les intervalles de confiance sont illustrés."
        ),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("wri_plot_byclass"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_wri_plot_byclass"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    
    # Affichage du graphique Wr par classe ----
    
    render_plot_ggplot(
      "wri_plot_byclass",
      reactive({
        res <- wri_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot_byclass))
        
        res$plot_byclass
      })
    )
    
    
    # Téléchargement du graphique Wr par classe ----
    
    render_download_plot(
      id = "download_wri_plot_byclass",
      plot = reactive({
        res <- wri_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot_byclass))
        
        res$plot_byclass
      }),
      filename_suffix = filename_suffix()
    )
    
  })
}