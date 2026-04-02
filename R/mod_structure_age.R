#' structure_age UI Function
#'
#' @description Un module Shiny pour afficher la structure d'âge des spécimens.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel sidebarPanel mainPanel radioButtons p h3 div br downloadButton plotOutput
mod_structure_age_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Structure d'âge",
    
    sidebarPanel(
      radioButtons(
        inputId = ns("groupeageplot"),
        label = "Grouper des poissons",
        choices = c(
          "Tous" = "tous",
          "Origine (marqué ou non-marqué)" = "marquage",
          "Sexe" = "sexe",
          "Statut reproducteur" = "maturite"
        )
      )
    ),
    
    mainPanel(
      uiOutput(ns("structure_age_message")),
      uiOutput(ns("structure_age_plot_section"))
    )
  )
}

#' structure_age Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_structure_age_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultat combiné ----
    res_structure_age <- reactive({
      req(specimen(), input$groupeageplot)
      
      structure_age(
        data = specimen(),
        groupement = input$groupeageplot
      )
    })
    
    # Message utilisateur ----
    output$structure_age_message <- renderUI({
      res <- res_structure_age()
      
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
    
    # Données téléchargeables ----
    data_structure_age <- reactive({
      res <- res_structure_age()
      
      req(isTRUE(res$success))
      req(!is.null(res$data))
      
      res$data
    })
    
    # Section graphique ----
    output$structure_age_plot_section <- renderUI({
      res <- res_structure_age()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$plot)) {
        return(NULL)
      }
      
      tagList(
        p(
          "L'histogramme de fréquence d'âge permettant de caractériser la structure ",
          "d'âge de la population est réalisé avec la fonction geom_histogram de la ",
          "librairie ggplot2 (Chang et al. 2021)."
        ),
        
        h3("Histogramme de fréquence des âges"),
        
        p(
          "La figure ci-dessous représente l'histogramme de fréquences des âges ",
          "selon le groupement sélectionné à gauche."
        ),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("structureageplot"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_groupeageplot"),
            "Téléchargement du graphique"
          )
        ),
        
        br(),
        
        download_button_ui(
          ns("download_data4plot_age"),
          label = "Téléchargement des données du graphique"
        )
      )
    })
    
    # Graphique ----
    render_plot_ggplot(
      "structureageplot",
      reactive({
        res <- res_structure_age()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      })
    )
    
    # Téléchargement du graphique ----
    render_download_plot(
      "download_groupeageplot",
      plot = reactive({
        res <- res_structure_age()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      }),
      filename_suffix = filename_suffix()
    )
    
    # Téléchargement des données ----
    render_download_table(
      "download_data4plot_age",
      data = data_structure_age,
      filename = reactive(
        build_export_filename("structure_age", filename_suffix())
      )
    )
  })
}