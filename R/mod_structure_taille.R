#' structure_taille UI Function
#'
#' @description Un module Shiny pour afficher la structure de taille des spécimens.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel sidebarPanel mainPanel radioButtons p h3 div br downloadButton plotOutput
mod_structure_taille_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Structure de taille",
    
    sidebarPanel(
      radioButtons(
        inputId = ns("groupetailleplot"),
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
      uiOutput(ns("structure_taille_message")),
      uiOutput(ns("structure_taille_plot_section"))
    )
  )
}

#' structure_taille Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_structure_taille_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultat combiné ----
    res_structure_taille <- reactive({
      req(specimen(), input$groupetailleplot)
      
      structure_taille(
        data = specimen(),
        groupement = input$groupetailleplot
      )
    })
    
    # Message utilisateur ----
    output$structure_taille_message <- renderUI({
      res <- res_structure_taille()
      
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
    data_structure_taille <- reactive({
      res <- res_structure_taille()
      
      req(isTRUE(res$success))
      req(!is.null(res$data))
      
      res$data
    })
    
    # Section graphique ----
    output$structure_taille_plot_section <- renderUI({
      res <- res_structure_taille()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$plot)) {
        return(NULL)
      }
      
      tagList(
        p(
          "La sélection des intervalles pour les classes de taille est basée sur les ",
          "recommandations de Anderson et Neumann (1996) et Neumann et al. (2012). ",
          "Ainsi, des intervalles de 20 mm sont utilisés pour l’omble de fontaine, ",
          "alors qu’ils sont de 50 mm pour le doré jaune et le touladi."
        ),
        
        h3("Histogramme de fréquence des longueurs"),
        
        p(
          "La figure ci-dessous représente l’histogramme de fréquence des longueurs ",
          "selon le groupement sélectionné à gauche."
        ),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("structuretailleplot"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_groupetailleplot"),
            "Téléchargement du graphique"
          )
        ),
        
        br(),
        
        download_button_ui(
          ns("download_data4plot_taille"),
          label = "Téléchargement des données du graphique"
        )
      )
    })
    
    # Graphique ----
    render_plot_ggplot(
      "structuretailleplot",
      reactive({
        res <- res_structure_taille()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      })
    )
    
    # Téléchargement du graphique ----
    render_download_plot(
      "download_groupetailleplot",
      plot = reactive({
        res <- res_structure_taille()
        
        req(isTRUE(res$success))
        req(!is.null(res$plot))
        
        res$plot
      }),
      filename_suffix = filename_suffix()
    )
    
    # Téléchargement des données ----
    render_download_table(
      "download_data4plot_taille",
      data = data_structure_taille,
      filename = reactive(
        build_export_filename("structure_taille", filename_suffix())
      )
    )
  })
}