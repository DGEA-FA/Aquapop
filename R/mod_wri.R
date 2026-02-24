#' wri UI Function
#'
#' @description Un module Shiny pour afficher l’indice de condition (Wr).
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_wri_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Indice de condition",
    
    # Tableau Wr
    p("Le tableau ci-dessous présente l’indice de masse relative (Wr) et son intervalle de confiance 
       à 95 % pour l’ensemble de la population, par sexe et par classe de PSD (classe selon 
       Gabelhouse 1984)."),
    uiOutput(ns("wri_table")),
    download_button_ui(ns("wri_table_dl")),
    
    br(),
    
    # Graphique Wr par sexe
    h3("Indice de condition (Wr) selon la longueur et le sexe"),
    p("Le graphique suivant illustre, pour chaque spécimen capturé, l’indice de condition en 
       fonction de la longueur totale maximale et du sexe. La valeur moyenne est indiquée par une 
       ligne pointillée en rouge (tous), en bleu foncé (femelles) et en bleu pâle (mâles). La ligne en gris 
       représente la référence standard pour l’espèce selon Hyatt & Hubert 2011 (SAFO), 
       Murphy et al. 1990 (SAVI) et Piccolo et al. 1993 (SANA)."),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("wri_plot_tous"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_wri_plot_tous"), "Téléchargement du graphique")
    ),
    
    br(),
    
    # Graphique Wr par classe de taille
    h3("Indice de condition (Wr) moyen par classe de taille"),
    p("Ce graphique présente la variation de l’indice de condition selon les classes de PSD. 
       Les valeurs moyenne et les intervalles de confiance sont illustrés."),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("wri_plot_byclass"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_wri_plot_byclass"), "Téléchargement du graphique")
    )
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
    
    # Résultat de l’indice de condition
    wri_res <- reactive({
      req(specimen(), nrow(specimen()) > 0)
      wri(data = specimen())
    })
    
    # Rendu du tableau Wr
    render_table_flextable(
      "wri_table", 
      reactive(wri_res()$flextable))
    
    # Bouton de téléchargement du tableau brut
    render_download_table(
      "wri_table_dl",
      data = reactive(wri_res()$data),
      filename = reactive(build_export_filename("wri", filename_suffix()))
    )
    
    # Graphique Wr tous
    render_plot_ggplot(
      "wri_plot_tous", 
      reactive(wri_res()$plot_tous))
    
    render_download_plot(
      "download_wri_plot_tous",
      reactive(wri_res()$plot_tous),
      filename_suffix = filename_suffix()
    )
    
    # Graphique Wr par classe
    render_plot_ggplot(
      "wri_plot_byclass", 
      reactive(wri_res()$plot_byclass))
    render_download_plot(
      "download_wri_plot_byclass",
      reactive(wri_res()$plot_byclass),
      filename_suffix = filename_suffix()
    )
  })
}

