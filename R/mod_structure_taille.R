#' structure_taille UI Function
#'
#' @description Un module Shiny pour afficher la structure de taille des spécimens.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_structure_taille_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Structure de taille",
    
    sidebarPanel(
      radioButtons(
        inputId = ns("groupetailleplot"),
        label = "Grouper des poissons",
        choices  = c(
          "Tous" = "tous",
          "Origine (marqué ou non-marqué)" = "marquage",
          "Sexe" = "sexe",
          "Statut reproducteur" = "maturite"
        )
      )
    ),
    
    mainPanel(
      p("La sélection des intervalles pour
        les classes de taille est basée sur les recommandations de Anderson et Neumann (1996) 
        et Neumann et al. (2012). Ainsi, des intervalles de 20 mm sont utilisés pour l’omble 
        de fontaine, alors qu’ils sont de 50 mm pour le doré jaune et le touladi."),
      
      h3("Histogramme de fréquence des longueurs"),
      p("La figure ci-dessous représente l’histogramme de fréquence des
        longueurs selon le groupement sélectionné à gauche."),
      
      div(
        style = "max-width: 900px; margin: auto;",
        withSpinner(plotOutput(ns("structuretailleplot"), height = "500px"), type = myspinner),
        br(),
        downloadButton(ns("download_groupetailleplot"), "Téléchargement du graphique")
      ),
      
      br(),
      
      download_button_ui(ns("download_data4plot_taille"),
                         label = "Téléchargement des données du graphique")
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
    
    # Résultat combiné : graphique + données
    res_structure_taille <- reactive({
      req(specimen(), input$groupetailleplot)
      structure_taille(
        data = specimen(),
        groupement = input$groupetailleplot
      )
    })
    
    # Graphique
    render_plot_ggplot("structuretailleplot", reactive(res_structure_taille()$plot))
    
    render_download_plot(
      "download_groupetailleplot",
      plot = reactive(res_structure_taille()$plot),
      filename_suffix = filename_suffix()
    )
    
    # Données du graphique
    render_download_table(
      "download_data4plot_taille",
      data = reactive(res_structure_taille()$data),
      filename = reactive(build_export_filename("structure_taille", filename_suffix()))
    )
  })
}

