#' maturite_l50 UI Function
#'
#' @description Module Shiny pour la sélection et l'affichage des modèles L50.
#'
#' @param id Identifiant du module.
#'
#' @noRd
mod_maturite_l50_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Longueur à maturité",
    
    # Message explicatif
    h3(HTML("Sélection des modèles L<sub>50</sub>")),
    verbatimTextOutput(ns("message_l50")),
    br(),
    
    # Tableau des modèles évalués
    h3("Tableau interactif des modèles évalués"),
    withSpinner(reactableOutput(ns("table_modeles_l50_table")), type = myspinner),
    download_button_ui(ns("ogive_l50_table_dl")),
    
    br(),
    h3("Résultats du modèle sélectionné"),
    
    # Tableau des résultats
    uiOutput(ns("ogive_l50_table")),
    uiOutput(ns("download_ogive_l50_table_ui")),
    
    # Graphique du modèle sélectionné
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot_ogive_l50"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_ogive_l50_plot"), "Téléchargement du graphique")
    ),
    
    br()
  )
}
#' maturite_l50 Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_maturite_l50_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultat des modèles évalués
    table_modeles_l50_resultats <- reactive({
      req(specimen())
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "ltm"
      )
    })
    
    # Index par défaut pour sélection
    default_model_index_l50 <- reactive({
      table <- table_modeles_l50_resultats()$table$df
      req(nrow(table) > 0)
      idx <- which(table$recommande)
      if (length(idx) == 0) idx <- 1
      idx
    })
    
    # Affichage du tableau interactif
    output$table_modeles_l50_table <- renderReactable({
      req(table_modeles_l50_resultats())
      table <- table_modeles_l50_resultats()$table$df
      idx <- default_model_index_l50()
      
      reactable(
        labelled_data(table),
        selection = "single",
        sortable = FALSE,
        onClick = "select",
        highlight = TRUE,
        defaultPageSize = 20,
        defaultSelected = idx,
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        )
      )
    })
    
    # Message descriptif
    output$message_l50 <- renderText({
      req(table_modeles_l50_resultats())
      table_modeles_l50_resultats()$message
    })
    
    # Modèle sélectionné
    selected_model_info_l50 <- reactive({
      selected <- getReactableState("table_modeles_l50_table", "selected")
      req(!is.null(selected), table_modeles_l50_resultats())
      table <- table_modeles_l50_resultats()$table$df
      model_id <- table[selected, "modele_id", drop = TRUE]
      
      list(
        modele = stringr::str_extract(model_id, "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(model_id, "logit|probit|cloglog"),
        variable = "ltm"
      )
    })
    
    # Résultat du modèle sélectionné
    l50_generate_modele_res <- reactive({
      req(specimen(), selected_model_info_l50())
      maturite_generate_modele(
        data = specimen(),
        variable = selected_model_info_l50()$variable,
        modele = selected_model_info_l50()$modele,
        lien = selected_model_info_l50()$lien
      )
    })
    
    # Tableau de résultats
    render_table_flextable(
      "ogive_l50_table", 
      reactive(l50_generate_modele_res()$table_resultats_flextable))
    
    render_download_table(
      "ogive_l50_table_dl",
      data = reactive(l50_generate_modele_res()$table_resultats),
      filename = reactive(build_export_filename("ogive_maturite", filename_suffix()))
    )
    
    # Graphique
    render_plot_ggplot("plot_ogive_l50", reactive(l50_generate_modele_res()$graphique))
    
    render_download_plot(
      "download_ogive_l50_plot",
      reactive(l50_generate_modele_res()$graphique),
      filename_suffix = filename_suffix()
    )
  })
}
