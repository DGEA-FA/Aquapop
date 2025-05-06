#' psd UI Function
#'
#' @description Un module Shiny pour afficher l’indice PSD et les répartitions par classe.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_psd_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "PSD",
    
    # Texte explicatif
    p("Autrefois appelé *Proportional stock density*, l’indice *Proportional size distribution* est un descripteur 
      numérique de la distribution de fréquence des longueurs. Il permet de comparer de manière objective la 
      structure de taille de deux populations d’une même espèce (ou d’une même population lors de deux inventaires 
      distincts). Les classes de taille sont établies en fonction de la taille record enregistrée pour une espèce 
      et les autres classes sont dérivées à partir de celle-ci (Gabelhouse 1984)."),
    
    # Indice Q
    h3("Indice PSD (Q)"),
    withSpinner(uiOutput(ns("psd_indice_ui")), type = myspinner),
    
    # Tableau des classes
    h3("Tableau des répartitions par classe de taille"),
    uiOutput(ns("psd_byclass_table")),
    download_button_ui(ns("psd_byclass_table_dl")),
    
    # Graphique
    h3("Distribution de fréquence de longueurs avec les classes de PSD"),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("psd_byclass_plot"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_psd_byclass_plot"), "Téléchargement du graphique")
    )
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
    
    # Résultat de l’indice Q
    psd_q_res <- reactive({
      req(specimen())
      psd_q(data = specimen())
    })
    
    # Rendu du tableau Q
    render_table_flextable(
      "psd_indice_ui",
      reactive(psd_q_res()$flextable))
    
    # Résultat des classes
    psd_byclass_res <- reactive({
      req(specimen())
      psd_byclass(data = specimen())
    })
    
    # Rendu du tableau par classe
    render_table_flextable(
      "psd_byclass_table",
      reactive(psd_byclass_res()$flextable))
    
    # Téléchargement du tableau brut
    render_download_table(
      "psd_byclass_table_dl",
      data = reactive(psd_byclass_res()$data),
      filename = reactive(build_export_filename("psd_byclass", filename_suffix()))
    )
    
    # Rendu du graphique
    render_plot_ggplot("psd_byclass_plot", reactive(psd_byclass_res()$plot))
    
    # Téléchargement du graphique
    render_download_plot(
      "download_psd_byclass_plot",
      plot = reactive(psd_byclass_res()$plot),
      filename_suffix = filename_suffix()
    )
  })
}

