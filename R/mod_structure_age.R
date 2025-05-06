#' structure_age UI Function
#'
#' @description Un module Shiny pour afficher la structure d'âge des spécimens.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_structure_age_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Structure d'âge",
    
    sidebarPanel(
      radioButtons(
        inputId = ns("groupeageplot"),
        label = "Filtrer des poissons",
        choices  = c(
          "Tous" = "tous",
          "Origine (marqué ou non-marqué)" = "marquage",
          "Sexe" = "sexe",
          "Statut reproducteur" = "maturite"
        )
      )
    ),
    
    mainPanel(
      p("L’histogramme de fréquence d'âge permettant de caractériser la structure d'âge 
         de la population est réalisée avec la fonction geom_histogram de la librairie ggplot2 (Chang et al. 2021)."),
      
      h3("Histogramme de fréquence des âges"),
      p("La figure ci-dessous représente l’histogramme de fréquences des âges 
         selon le filtre sélectionné à gauche."),
      
      div(
        style = "max-width: 900px; margin: auto;",
        withSpinner(plotOutput(ns("structureageplot"), height = "500px"), type = myspinner),
        br(),
        downloadButton(ns("download_groupeageplot"), "Téléchargement du graphique")
      ),
      
      br(),
      
      download_button_ui(ns("download_data4plot_age"),
                         label = "Téléchargement des données du graphique")
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
    
    # Résultat combiné : graphique + données
    res_structure_age <- reactive({
      req(specimen(), input$groupeageplot)
      structure_age(
        data = specimen(),
        groupement = input$groupeageplot
      )
    })
    
    # Graphique (avec message conditionnel si vide)
    render_plot_ggplot(
      output_id = "structureageplot",
      plot = reactive(res_structure_age()$plot),
      message_si_vide = "Aucun graphique n’a pu être généré : données d’âge manquantes ou inexploitables."
    )
    
    # Téléchargement du graphique
    render_download_plot(
      "download_groupeageplot",
      plot = reactive(res_structure_age()$plot),
      filename_suffix = filename_suffix()
    )
    
    # Téléchargement des données
    render_download_table(
      "download_data4plot_age",
      data = reactive(res_structure_age()$data),
      filename = reactive(build_export_filename("structure_age", filename_suffix()))
    )
  })
}
