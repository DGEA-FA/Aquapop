#' taille_masse_age UI Function
#'
#' @description A shiny Module for displaying size, mass, and age statistics.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_taille_masse_age_ui <- function(id){
  ns <- NS(id)

    tabPanel(
      title = "Taille, masse et âge moyens",
      p("Le tableau suivant reprend les statistiques descriptives, soit le nombre de spécimens mesurés/pesés/âgés (N) 
        ainsi que la moyenne (Moy.), l’écart-type (ET), les valeurs minimale (Min) et maximale (Max) de la longueur
        totale maximale (LTMax), de la masse et de l’âge des poissons pour différents groupes."),
      h3("Aperçu des données morphologiques"),
      withSpinner(uiOutput(ns("table")), type = myspinner),
      download_button_ui(ns("table_dl"))
    )

}

#' taille_masse_age Server Functions
#'
#' @param id Internal parameters for {shiny}.
#' @param specimen_valid Reactive expression containing valid specimen data.
#' @param filename_suffix Reactive expression for filename suffix.
#'
#' @noRd 
mod_taille_masse_age_server <- function(id, specimen_valid, filename_suffix){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Résultat combiné (data + flextable)
    taille_masse_age_res <- reactive({
      req(specimen_valid())
      taille_masse_age(specimen_valid())
    })
    
    # Affichage du tableau flextable
    render_table_flextable("table", reactive(taille_masse_age_res()$flextable))
    
    # Bouton de téléchargement
    render_download_table(
      "table_dl",
      data = reactive(taille_masse_age_res()$data),
      filename = reactive(build_export_filename("taille_masse_age", filename_suffix()))
    )
  })
}
