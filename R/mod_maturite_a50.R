#' maturite_a50 UI Function
#'
#' @description Module Shiny pour l'affichage des modèles A50.
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
    uiOutput(ns("section_a50_femelles")),
    uiOutput(ns("section_a50_males")),
    uiOutput(ns("section_a50_combine"))
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
    
    # Résultat global ----
    table_modeles_a50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "age"
      )
    })
    
    # Message complémentaire ----
    message_complementaire_a50 <- reactive({
      res <- table_modeles_a50_resultats()
      req(!is.null(res))
      
      best_model <- res$best_model
      
      has_F <- !is.null(best_model$best_model_F)
      has_M <- !is.null(best_model$best_model_M)
      has_comb <- !is.null(best_model$best_model_combined)
      
      if (!has_F && !has_M && !has_comb) {
        return(HTML("Les données disponibles ne permettent pas d'ajuster des modèles de maturité."))
      }
      
      if (!has_F && !has_M && has_comb) {
        return(HTML("Les modèles séparés ne peuvent être ajustés. Un modèle combiné est proposé."))
      }
      
      if ((has_F && !has_M) || (!has_F && has_M)) {
        return(HTML("Un seul sexe permet un ajustement valide. Un modèle combiné est aussi proposé."))
      }
      
      HTML("Les modèles séparés sont à privilégier. Les modèles combinés sont présentés en complément.")
    })
    
    # Message UI ----
    output$message_a50 <- renderUI({
      res <- table_modeles_a50_resultats()
      req(!is.null(res))
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      couleur_bordure <- if (isTRUE(res$success)) "#4c6ef5" else "#c0392b"
      couleur_fond <- if (isTRUE(res$success)) "#f5f7ff" else "#fdf2f2"
      
      div(
        style = paste(
          "margin: 15px 0;",
          "padding: 12px 16px;",
          "border-left: 4px solid", couleur_bordure, ";",
          "background-color:", couleur_fond
        ),
        tags$h4("Sélection des modèles A50"),
        tags$p(res$message),
        tags$p(message_complementaire_a50())
      )
    })
    
    # Tables ----
    table_a50_f <- reactive({
      table_modeles_a50_resultats()$table_sep$df |>
        dplyr::filter(type == "séparé_F")
    })
    
    table_a50_m <- reactive({
      table_modeles_a50_resultats()$table_sep$df |>
        dplyr::filter(type == "séparé_M")
    })
    
    table_a50_comb <- reactive({
      table_modeles_a50_resultats()$table_comb$df
    })
    
    # === SECTION FEMELLES ===
    output$section_a50_femelles <- renderUI({
      table <- table_a50_f()
      if (nrow(table) == 0) return(NULL)
      
      model_section_card_ui(
        "Femelles",
        "Modèles ajustés sur les femelles",
        
        reactableOutput(ns("table_a50_f")),
        download_button_ui(ns("dl_a50_f")),
        
        tags$hr(),
        uiOutput(ns("res_a50_f")),
        download_button_ui(ns("dl_res_a50_f")),
        
        tags$hr(),
        plotOutput(ns("plot_a50_f")),
        downloadButton(ns("dl_plot_a50_f"), "Télécharger")
      )
    })
    
    output$table_a50_f <- renderReactable({
      reactable(as.data.frame(table_a50_f()), selection = "single")
    })
    
    selected_f <- reactive({
      sel <- getReactableState("table_a50_f", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_f()[sel, "modele_id"]
    })
    
    res_f <- reactive({
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = "TLO",
        lien = stringr::str_extract(selected_f(), "logit|probit|cloglog")
      )
    })
    
    render_table_flextable("res_a50_f", reactive(res_f()$table_resultats_flextable))
    render_plot_ggplot("plot_a50_f", reactive(res_f()$graphique))
    
    # === SECTION MALES ===
    output$section_a50_males <- renderUI({
      table <- table_a50_m()
      if (nrow(table) == 0) return(NULL)
      
      model_section_card_ui(
        "Mâles",
        "Modèles ajustés sur les mâles",
        
        reactableOutput(ns("table_a50_m")),
        download_button_ui(ns("dl_a50_m")),
        
        tags$hr(),
        uiOutput(ns("res_a50_m")),
        download_button_ui(ns("dl_res_a50_m")),
        
        tags$hr(),
        plotOutput(ns("plot_a50_m")),
        downloadButton(ns("dl_plot_a50_m"), "Télécharger")
      )
    })
    
    output$table_a50_m <- renderReactable({
      reactable(as.data.frame(table_a50_m()), selection = "single")
    })
    
    selected_m <- reactive({
      sel <- getReactableState("table_a50_m", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_m()[sel, "modele_id"]
    })
    
    res_m <- reactive({
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = "TLO",
        lien = stringr::str_extract(selected_m(), "logit|probit|cloglog")
      )
    })
    
    render_table_flextable("res_a50_m", reactive(res_m()$table_resultats_flextable))
    render_plot_ggplot("plot_a50_m", reactive(res_m()$graphique))
    
    # === SECTION COMBINE ===
    output$section_a50_combine <- renderUI({
      table <- table_a50_comb()
      if (nrow(table) == 0) return(NULL)
      
      model_section_card_ui(
        "Modèles combinés",
        "Modèles ajustés sur l'ensemble des données",
        
        reactableOutput(ns("table_a50_comb")),
        download_button_ui(ns("dl_a50_comb")),
        
        tags$hr(),
        uiOutput(ns("res_a50_comb")),
        download_button_ui(ns("dl_res_a50_comb")),
        
        tags$hr(),
        plotOutput(ns("plot_a50_comb")),
        downloadButton(ns("dl_plot_a50_comb"), "Télécharger")
      )
    })
    
    output$table_a50_comb <- renderReactable({
      reactable(as.data.frame(table_a50_comb()), selection = "single")
    })
    
    selected_comb <- reactive({
      sel <- getReactableState("table_a50_comb", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_comb()[sel, "modele_id"]
    })
    
    res_comb <- reactive({
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = stringr::str_extract(selected_comb(), "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(selected_comb(), "logit|probit|cloglog")
      )
    })
    
    render_table_flextable("res_a50_comb", reactive(res_comb()$table_resultats_flextable))
    render_plot_ggplot("plot_a50_comb", reactive(res_comb()$graphique))
    
  })
}