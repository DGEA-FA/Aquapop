#' maturite_a50 UI Function
#'
#' @description Module Shiny pour la sélection et l'affichage des modèles A50.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel
mod_maturite_a50_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Âge à maturité",
    uiOutput(ns("message_a50")),
    uiOutput(ns("table_modeles_a50_section")),
    uiOutput(ns("resultats_modele_a50_section"))
  )
}

#' maturite_a50 Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_maturite_a50_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Résultat des modèles évalués ----
    table_modeles_a50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "age"
      )
    })
    
    # Message utilisateur ----
    output$message_a50 <- renderUI({
      res <- table_modeles_a50_resultats()
      
      req(!is.null(res))
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      titre_message <- if (isTRUE(res$success)) {
        HTML("Sélection des modèles A<sub>50</sub>")
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
          style = "margin: 0; white-space: pre-line;",
          res$message
        )
      )
    })
    
    # Index par défaut pour sélection ----
    default_model_index_a50 <- reactive({
      res <- table_modeles_a50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table$df))
      req(nrow(res$table$df) > 0)
      
      table <- res$table$df
      idx <- which(table$recommande)
      
      if (length(idx) == 0) {
        idx <- 1
      }
      
      idx[1]
    })
    
    # Section tableau des modèles ----
    output$table_modeles_a50_section <- renderUI({
      res <- table_modeles_a50_resultats()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$table$df) || nrow(res$table$df) == 0) {
        return(NULL)
      }
      
      tagList(
        h3(HTML("Tableau interactif des modèles évalués pour A<sub>50</sub>")),
        withSpinner(
          reactableOutput(ns("table_modeles_a50_table")),
          type = myspinner
        ),
        download_button_ui(ns("table_modeles_a50_dl")),
        br()
      )
    })
    
    # Affichage du tableau interactif ----
    output$table_modeles_a50_table <- renderReactable({
      res <- table_modeles_a50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table$df))
      req(nrow(res$table$df) > 0)
      
      table <- res$table$df
      idx <- default_model_index_a50()
      
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
    
    # Téléchargement du tableau des modèles ----
    render_download_table(
      id = "table_modeles_a50_dl",
      data = reactive({
        res <- table_modeles_a50_resultats()
        
        req(isTRUE(res$success))
        req(!is.null(res$table$df))
        
        res$table$df
      }),
      filename = reactive(
        build_export_filename("modeles_maturite_a50", filename_suffix())
      )
    )
    
    # Modèle sélectionné ----
    selected_model_info_a50 <- reactive({
      res <- table_modeles_a50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table$df))
      req(nrow(res$table$df) > 0)
      
      selected <- getReactableState("table_modeles_a50_table", "selected")
      
      if (is.null(selected) || length(selected) == 0) {
        selected <- default_model_index_a50()
      }
      
      model_id <- res$table$df[selected, "modele_id", drop = TRUE]
      
      req(!is.na(model_id))
      
      list(
        modele = stringr::str_extract(model_id, "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(model_id, "logit|probit|cloglog"),
        variable = "age"
      )
    })
    
    # Résultat du modèle sélectionné ----
    a50_generate_modele_res <- reactive({
      req(specimen())
      req(selected_model_info_a50())
      
      maturite_generate_modele(
        data = specimen(),
        variable = selected_model_info_a50()$variable,
        modele = selected_model_info_a50()$modele,
        lien = selected_model_info_a50()$lien
      )
    })
    
    # Section résultats du modèle ----
    output$resultats_modele_a50_section <- renderUI({
      res <- a50_generate_modele_res()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success)) {
        return(NULL)
      }
      
      tagList(
        h3("Résultats du modèle sélectionné"),
        uiOutput(ns("ogive_a50_table")),
        download_button_ui(ns("ogive_a50_table_dl")),
        br(),
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("plot_ogive_a50"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_ogive_a50_plot"),
            "Téléchargement du graphique"
          )
        ),
        br()
      )
    })
    
    # Tableau de résultats ----
    render_table_flextable(
      "ogive_a50_table",
      reactive({
        res <- a50_generate_modele_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats_flextable))
        
        res$table_resultats_flextable
      })
    )
    
    render_download_table(
      id = "ogive_a50_table_dl",
      data = reactive({
        res <- a50_generate_modele_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats))
        
        res$table_resultats
      }),
      filename = reactive(
        build_export_filename("ogive_maturite_a50", filename_suffix())
      )
    )
    
    # Graphique ----
    render_plot_ggplot(
      "plot_ogive_a50",
      reactive({
        res <- a50_generate_modele_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      })
    )
    
    render_download_plot(
      id = "download_ogive_a50_plot",
      plot = reactive({
        res <- a50_generate_modele_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      }),
      filename_suffix = filename_suffix()
    )
  })
}