#' UI - Module CPUE / Abondance
#'
#' @description Panneau affichant les résultats de CPUE et du tableau d'abondance.
#'
#' @param id Identifiant du module
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_abondance_cpue_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "CPUE",
    
    h3("Validation des captures et des spécimens"),
    
    p(
      "Le tableau ci-dessous présente le nombre de spécimens  de l'espèce visée selon la table ",
      "Récolte et selon la table Spécimens (stations valides et au hasard seulement). ",
      "Si la récolte est plus élevée que le nombre de spécimens, il peut s'agir d'un poisson ",
      "échappé ou trop abîmé pour prendre des mesures. Si le nombre de spécimens est plus ",
      "élevé que la récolte, il y a une erreur à corriger dans la base de données.",
      "Notez que les modèles de CPUE ont été appliqués sur les données provenant de la Récolte alors ",
      "que le tableau d'abondance et les modèles BPUE ont été créés à partir des données de Spécimens."
    ),
    
    uiOutput(ns("capture_specimen_message")),
    withSpinner(uiOutput(ns("capture_specimen_table")), type = myspinner),
    download_button_ui(ns("capture_specimen_table_dl")),
    
    br(),
    
    h3("Modèles de CPUE - Tous les individus"),
    
    ## CPUE - Tous
    withSpinner(uiOutput(ns("cpue_tous_table")), type = myspinner),
    download_button_ui(ns("cpue_tous_table_dl")),
    
    br(),
    
    ## Abondance
    h3("Tableau récapitulatif d'abondance"),
    uiOutput(ns("abondance_table")),
    download_button_ui(ns("abondance_table_dl"))
  )
}


#' Server - Module CPUE / Abondance
#'
#' @param id Identifiant du module
#' @param specimen Données des spécimens (reactive)
#' @param capture Données de récolte (reactive)
#' @param filename_suffix Suffixe pour le nom de fichier (reactive)
#'
#' @noRd
mod_abondance_cpue_server <- function(id, capture, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Validation - Récolte vs Spécimens
    capture_specimen_res <- reactive({
      req(capture(), specimen())
      
      cpue_compare_capture_specimen(
        capture = capture(),
        specimen = specimen()
      )
    })
    
    output$capture_specimen_message <- renderUI({
      req(capture_specimen_res())
      
      p(strong(capture_specimen_res()$message))
    })
    
    render_table_flextable(
      "capture_specimen_table",
      reactive(capture_specimen_res()$flextable)
    )
    
    render_download_table(
      "capture_specimen_table_dl",
      data = reactive(capture_specimen_res()$data),
      filename = reactive(build_export_filename(
        "validation_capture_specimen",
        filename_suffix()
      ))
    )
    
    cpue_modele_tous <- reactive({
      req(capture())
      cpue_compare_modele(capture = capture())
    })
    
    render_table_flextable("cpue_tous_table", reactive(cpue_modele_tous()$flextable))
    
    render_download_table(
      "cpue_tous_table_dl",
      data = reactive(cpue_modele_tous()$data),
      filename = reactive(build_export_filename("cpue_tous", filename_suffix()))
    )
    

    ## Tableau d'abondance
    best_model_tous <- reactive({
      req(cpue_modele_tous())
      cpue_select_best_modele(cpue_modele_tous()$data)
    })
    
    abondance1 <- reactive({
      req(
        specimen(),
        cpue_modele_tous(),
        best_model_tous() #,
      )
      
      cpue_abondance_table(
        data = specimen(),
        cpue_table_tous = cpue_modele_tous()$data,
        best_model_tous = best_model_tous() #,
      )
    })
    
    render_table_flextable("abondance_table", reactive(abondance1()$flextable))
    
    render_download_table(
      "abondance_table_dl",
      data = reactive(abondance1()$data),
      filename = reactive(build_export_filename("abondance", filename_suffix()))
    )
  })
}

