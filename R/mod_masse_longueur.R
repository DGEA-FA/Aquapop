#' masse_longueur UI Function
#'
#' @description A shiny Module for displaying mass-length relationship.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_masse_longueur_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Relation masse-longueur",
    
    # Texte explicatif
    p("La figure suivante représente la relation allométrique entre la longueur totale
      maximale (mm) et la masse (g). L’équation et la valeur des paramètres sont indiqués sur 
      le graphique."),
    
    # Graphique
    h3("Relation masse-longueur"),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_plot"), "Téléchargement du graphique")
    ),
    
    br(),
    
    # Tableau des coefficients
    h3("Tableau des coefficients"),
    uiOutput(ns("table_ui")),
    download_button_ui(ns("download_table"))
  )
}
#' masse_longueur Server Function
#'
#' @param id Module ID.
#' @param specimen Reactive expression containing specimen data.
#' @param filename_suffix Reactive expression for filename suffix.
#'
#' @noRd 
mod_masse_longueur_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultat combiné
    fit_res <- reactive({
      req(specimen())
      masse_longueur_fit(data = specimen())
    })
    
    # Graphique
    render_plot_ggplot(
      output_id =  "plot", 
      plot =  reactive(fit_res()$plot))
    
    render_download_plot(
      "download_plot",
      reactive(fit_res()$plot),
      filename_suffix = filename_suffix()
    )
    
    # Tableau
    render_table_flextable("table_ui", reactive(fit_res()$flextable))
    
    render_download_table(
      "download_table",
      data = reactive(fit_res()$data),
      filename = reactive(build_export_filename("masselongueur", filename_suffix()))
    )
  })
}
