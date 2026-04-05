#' maturite_l50 UI Function
#'
#' @description Module Shiny pour l'affichage des modèles L50.
#'
#' @param id Identifiant du module.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput tabPanel
mod_maturite_l50_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Longueur à maturité",
    uiOutput(ns("message_l50")),
    uiOutput(ns("section_l50_femelles")),
    uiOutput(ns("section_l50_males")),
    uiOutput(ns("section_l50_combine"))
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
    
    # Résultat global de comparaison ----
    table_modeles_l50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "ltm"
      )
    })
    
    # Message complémentaire dynamique ----
    message_complementaire_l50 <- reactive({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      best_model <- res$best_model
      
      has_F <- !is.null(best_model$best_model_F)
      has_M <- !is.null(best_model$best_model_M)
      has_comb <- !is.null(best_model$best_model_combined)
      
      if (!has_F && !has_M && !has_comb) {
        return(HTML(
          "Les données disponibles ne permettent pas d'ajuster des modèles de maturité. 
          Vérifiez la quantité et la qualité des données pour cette pêche."
        ))
      }
      
      if (!has_F && !has_M && has_comb) {
        return(HTML(
          "Les modèles séparés par sexe sont à privilégier lorsqu'ils sont disponibles. 
          Dans ce cas, les données ne permettent pas d'ajuster des modèles distincts pour les femelles et les mâles. 
          Un modèle combiné est présenté comme alternative. 
          Tous les modèles sont affichés afin de permettre une exploration complète des résultats."
        ))
      }
      
      if ((has_F && !has_M) || (!has_F && has_M)) {
        return(HTML(
          "Les modèles séparés par sexe sont à privilégier lorsqu'ils sont disponibles. 
          Dans ce cas, un seul des deux sexes permet un ajustement valide. 
          Un modèle combiné est également présenté en complément. 
          Tous les modèles sont affichés afin de permettre une exploration complète des résultats."
        ))
      }
      
      HTML(
        "Les modèles séparés par sexe sont à privilégier lorsqu'ils sont disponibles. 
        Les modèles combinés sont présentés en complément. 
        Tous les modèles sont affichés afin de permettre une exploration complète des résultats."
      )
    })
    
    # Message utilisateur ----
    output$message_l50 <- renderUI({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      titre_message <- if (isTRUE(res$success)) {
        HTML("Sélection des modèles L<sub>50</sub>")
      } else {
        "Analyse non disponible"
      }
      
      couleur_bordure <- if (isTRUE(res$success)) {
        "#4c6ef5"
      } else {
        "#c0392b"
      }
      
      couleur_fond <- if (isTRUE(res$success)) {
        "#f5f7ff"
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
        tagList(
          tags$p(
            style = "margin: 0 0 10px 0; white-space: pre-line;",
            res$message
          ),
          tags$p(
            style = "margin: 0;",
            message_complementaire_l50()
          )
        )
      )
    })
    
    # Tables par section ----
    table_l50_femelles <- reactive({
      res <- table_modeles_l50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table_sep$df))
      
      res$table_sep$df |>
        dplyr::filter(type == "séparé_F")
    })
    
    table_l50_males <- reactive({
      res <- table_modeles_l50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table_sep$df))
      
      res$table_sep$df |>
        dplyr::filter(type == "séparé_M")
    })
    
    table_l50_combine <- reactive({
      res <- table_modeles_l50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table_comb$df))
      
      res$table_comb$df
    })
    
    # Index par défaut ----
    default_model_index_l50_femelles <- reactive({
      table <- table_l50_femelles()
      req(nrow(table) > 0)
      
      idx <- which(table$recommande)
      
      if (length(idx) == 0) {
        idx <- 1
      }
      
      idx[1]
    })
    
    default_model_index_l50_males <- reactive({
      table <- table_l50_males()
      req(nrow(table) > 0)
      
      idx <- which(table$recommande)
      
      if (length(idx) == 0) {
        idx <- 1
      }
      
      idx[1]
    })
    
    default_model_index_l50_combine <- reactive({
      table <- table_l50_combine()
      req(nrow(table) > 0)
      
      idx <- which(table$recommande)
      
      if (length(idx) == 0) {
        idx <- 1
      }
      
      idx[1]
    })
    
    # Section Femelles UI ----
    output$section_l50_femelles <- renderUI({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success)) {
        return(NULL)
      }
      
      table <- table_l50_femelles()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      model_section_card_ui(
        title = "Femelles",
        subtitle = "Modèles ajustés uniquement sur les femelles.",
        
        withSpinner(
          reactableOutput(ns("table_modeles_l50_femelles")),
          type = myspinner
        ),
        
        br(),
        download_button_ui(ns("table_modeles_l50_femelles_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        uiOutput(ns("ogive_l50_femelles_table")),
        br(),
        download_button_ui(ns("ogive_l50_femelles_table_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("plot_ogive_l50_femelles"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_ogive_l50_femelles_plot"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    # Section Mâles UI ----
    output$section_l50_males <- renderUI({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success)) {
        return(NULL)
      }
      
      table <- table_l50_males()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      model_section_card_ui(
        title = "Mâles",
        subtitle = "Modèles ajustés uniquement sur les mâles.",
        
        withSpinner(
          reactableOutput(ns("table_modeles_l50_males")),
          type = myspinner
        ),
        
        br(),
        download_button_ui(ns("table_modeles_l50_males_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        uiOutput(ns("ogive_l50_males_table")),
        br(),
        download_button_ui(ns("ogive_l50_males_table_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("plot_ogive_l50_males"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_ogive_l50_males_plot"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    # Section Combiné UI ----
    output$section_l50_combine <- renderUI({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success)) {
        return(NULL)
      }
      
      table <- table_l50_combine()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      model_section_card_ui(
        title = "Modèles combinés",
        subtitle = "Modèles ajustés sur l’ensemble des données, sans séparation explicite par sexe ou avec effet du sexe selon la structure du modèle.",
        
        withSpinner(
          reactableOutput(ns("table_modeles_l50_combine")),
          type = myspinner
        ),
        
        br(),
        download_button_ui(ns("table_modeles_l50_combine_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        uiOutput(ns("ogive_l50_combine_table")),
        br(),
        download_button_ui(ns("ogive_l50_combine_table_dl")),
        
        tags$hr(style = "margin: 18px 0;"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(
            plotOutput(ns("plot_ogive_l50_combine"), height = "500px"),
            type = myspinner
          ),
          br(),
          downloadButton(
            ns("download_ogive_l50_combine_plot"),
            "Téléchargement du graphique"
          )
        )
      )
    })
    
    # Reactable Femelles ----
    output$table_modeles_l50_femelles <- renderReactable({
      table <- table_l50_femelles()
      idx <- default_model_index_l50_femelles()
      
      req(nrow(table) > 0)
      
      reactable(
        as.data.frame(table),
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
    
    # Reactable Mâles ----
    output$table_modeles_l50_males <- renderReactable({
      table <- table_l50_males()
      idx <- default_model_index_l50_males()
      
      req(nrow(table) > 0)
      
      reactable(
        as.data.frame(table),
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
    
    # Reactable Combiné ----
    output$table_modeles_l50_combine <- renderReactable({
      table <- table_l50_combine()
      idx <- default_model_index_l50_combine()
      
      req(nrow(table) > 0)
      
      reactable(
        as.data.frame(table),
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
    
    # Téléchargement tables de sélection ----
    render_download_table(
      id = "table_modeles_l50_femelles_dl",
      data = table_l50_femelles,
      filename = reactive(
        build_export_filename("modeles_l50_femelles", filename_suffix())
      )
    )
    
    render_download_table(
      id = "table_modeles_l50_males_dl",
      data = table_l50_males,
      filename = reactive(
        build_export_filename("modeles_l50_males", filename_suffix())
      )
    )
    
    render_download_table(
      id = "table_modeles_l50_combine_dl",
      data = table_l50_combine,
      filename = reactive(
        build_export_filename("modeles_l50_combine", filename_suffix())
      )
    )
    
    # Modèle sélectionné Femelles ----
    selected_model_info_l50_femelles <- reactive({
      table <- table_l50_femelles()
      
      req(nrow(table) > 0)
      
      selected <- getReactableState("table_modeles_l50_femelles", "selected")
      
      if (is.null(selected) || length(selected) == 0) {
        selected <- default_model_index_l50_femelles()
      }
      
      model_id <- table[selected, "modele_id", drop = TRUE]
      
      req(!is.na(model_id))
      
      list(
        variable = "ltm",
        modele = "TLO",
        lien = stringr::str_extract(model_id, "logit|probit|cloglog")
      )
    })
    
    # Modèle sélectionné Mâles ----
    selected_model_info_l50_males <- reactive({
      table <- table_l50_males()
      
      req(nrow(table) > 0)
      
      selected <- getReactableState("table_modeles_l50_males", "selected")
      
      if (is.null(selected) || length(selected) == 0) {
        selected <- default_model_index_l50_males()
      }
      
      model_id <- table[selected, "modele_id", drop = TRUE]
      
      req(!is.na(model_id))
      
      list(
        variable = "ltm",
        modele = "TLO",
        lien = stringr::str_extract(model_id, "logit|probit|cloglog")
      )
    })
    
    # Modèle sélectionné Combiné ----
    selected_model_info_l50_combine <- reactive({
      table <- table_l50_combine()
      
      req(nrow(table) > 0)
      
      selected <- getReactableState("table_modeles_l50_combine", "selected")
      
      if (is.null(selected) || length(selected) == 0) {
        selected <- default_model_index_l50_combine()
      }
      
      model_id <- table[selected, "modele_id", drop = TRUE]
      
      req(!is.na(model_id))
      
      list(
        variable = "ltm",
        modele = stringr::str_extract(model_id, "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(model_id, "logit|probit|cloglog")
      )
    })
    
    # Résultats détaillés Femelles ----
    l50_generate_modele_femelles_res <- reactive({
      req(specimen())
      req(selected_model_info_l50_femelles())
      
      maturite_generate_modele(
        data = specimen(),
        variable = selected_model_info_l50_femelles()$variable,
        modele = selected_model_info_l50_femelles()$modele,
        lien = selected_model_info_l50_femelles()$lien
      )
    })
    
    # Résultats détaillés Mâles ----
    l50_generate_modele_males_res <- reactive({
      req(specimen())
      req(selected_model_info_l50_males())
      
      maturite_generate_modele(
        data = specimen(),
        variable = selected_model_info_l50_males()$variable,
        modele = selected_model_info_l50_males()$modele,
        lien = selected_model_info_l50_males()$lien
      )
    })
    
    # Résultats détaillés Combiné ----
    l50_generate_modele_combine_res <- reactive({
      req(specimen())
      req(selected_model_info_l50_combine())
      
      maturite_generate_modele(
        data = specimen(),
        variable = selected_model_info_l50_combine()$variable,
        modele = selected_model_info_l50_combine()$modele,
        lien = selected_model_info_l50_combine()$lien
      )
    })
    
    # Tableau détaillé Femelles ----
    render_table_flextable(
      "ogive_l50_femelles_table",
      reactive({
        res <- l50_generate_modele_femelles_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats_flextable))
        
        res$table_resultats_flextable
      })
    )
    
    # Tableau détaillé Mâles ----
    render_table_flextable(
      "ogive_l50_males_table",
      reactive({
        res <- l50_generate_modele_males_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats_flextable))
        
        res$table_resultats_flextable
      })
    )
    
    # Tableau détaillé Combiné ----
    render_table_flextable(
      "ogive_l50_combine_table",
      reactive({
        res <- l50_generate_modele_combine_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats_flextable))
        
        res$table_resultats_flextable
      })
    )
    
    # Téléchargement tableaux détaillés ----
    render_download_table(
      id = "ogive_l50_femelles_table_dl",
      data = reactive({
        res <- l50_generate_modele_femelles_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats))
        
        res$table_resultats
      }),
      filename = reactive(
        build_export_filename("ogive_l50_femelles", filename_suffix())
      )
    )
    
    render_download_table(
      id = "ogive_l50_males_table_dl",
      data = reactive({
        res <- l50_generate_modele_males_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats))
        
        res$table_resultats
      }),
      filename = reactive(
        build_export_filename("ogive_l50_males", filename_suffix())
      )
    )
    
    render_download_table(
      id = "ogive_l50_combine_table_dl",
      data = reactive({
        res <- l50_generate_modele_combine_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$table_resultats))
        
        res$table_resultats
      }),
      filename = reactive(
        build_export_filename("ogive_l50_combine", filename_suffix())
      )
    )
    
    # Graphiques ----
    render_plot_ggplot(
      "plot_ogive_l50_femelles",
      reactive({
        res <- l50_generate_modele_femelles_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      })
    )
    
    render_plot_ggplot(
      "plot_ogive_l50_males",
      reactive({
        res <- l50_generate_modele_males_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      })
    )
    
    render_plot_ggplot(
      "plot_ogive_l50_combine",
      reactive({
        res <- l50_generate_modele_combine_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      })
    )
    
    # Téléchargement graphiques ----
    render_download_plot(
      id = "download_ogive_l50_femelles_plot",
      plot = reactive({
        res <- l50_generate_modele_femelles_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      }),
      filename_suffix = paste0(filename_suffix(), "_l50_femelles")
    )
    
    render_download_plot(
      id = "download_ogive_l50_males_plot",
      plot = reactive({
        res <- l50_generate_modele_males_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      }),
      filename_suffix = paste0(filename_suffix(), "_l50_males")
    )
    
    render_download_plot(
      id = "download_ogive_l50_combine_plot",
      plot = reactive({
        res <- l50_generate_modele_combine_res()
        
        req(isTRUE(res$success))
        req(!is.null(res$graphique))
        
        res$graphique
      }),
      filename_suffix = paste0(filename_suffix(), "_l50_combine")
    )
  })
}