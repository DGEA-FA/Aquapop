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
    
    # ==== Résultat global ----
    table_modeles_a50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "age"
      )
    })
    
    # ==== Règles d'affichage ----
    afficher_modeles_separes_a50 <- reactive({
      res <- table_modeles_a50_resultats()
      req(!is.null(res))
      
      best_model <- res$best_model
      
      has_F <- !is.null(best_model$best_model_F)
      has_M <- !is.null(best_model$best_model_M)
      
      has_F && has_M
    })
    
    afficher_modeles_combines_a50 <- reactive({
      res <- table_modeles_a50_resultats()
      req(!is.null(res))
      
      best_model <- res$best_model
      
      has_F <- !is.null(best_model$best_model_F)
      has_M <- !is.null(best_model$best_model_M)
      has_comb <- !is.null(best_model$best_model_combined)
      
      (!has_F || !has_M) && has_comb
    })
    
    # ==== Message complémentaire ----
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
      
      if (has_F && has_M) {
        return(HTML("Les modèles séparés sont retenus pour les femelles et les mâles."))
      }
      
      if (has_comb) {
        return(HTML("Les modèles séparés ne sont pas retenus, car au moins un des deux sexes ne permet pas un ajustement valide. Seuls les modèles combinés sont affichés."))
      }
      
      HTML("Les données disponibles ne permettent pas d'ajuster des modèles de maturité.")
    })
    
    # ==== Message UI ----
    output$message_a50 <- renderUI({
      res <- table_modeles_a50_resultats()
      req(!is.null(res))
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      couleur_bordure <- if (isTRUE(res$success)) "#4c6ef5" else "#c0392b"
      couleur_fond <- if (isTRUE(res$success)) "#f5f7ff" else "#fdf2f2"
      
      msg <- res$message
      
      if (afficher_modeles_separes_a50()) {
        msg <- gsub("Modèle combiné sélectionné :.*", "", msg)
        msg <- gsub("\n+", "\n", msg)
        msg <- trimws(msg)
      }
      
      div(
        style = paste(
          "margin: 15px 0;",
          "padding: 12px 16px;",
          "border-left: 4px solid", couleur_bordure, ";",
          "background-color:", couleur_fond
        ),
        tags$h4("Sélection des modèles A50"),
        tags$p(msg),
        tags$p(message_complementaire_a50())
      )
    })
    
    # ==== Tables de sélection ----
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
    
    # ==== Sélection des modèles ----
    selected_f <- reactive({
      req(afficher_modeles_separes_a50())
      sel <- getReactableState("table_a50_f", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_f()[sel, "modele_id"]
    })
    
    selected_m <- reactive({
      req(afficher_modeles_separes_a50())
      sel <- getReactableState("table_a50_m", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_m()[sel, "modele_id"]
    })
    
    selected_comb <- reactive({
      req(afficher_modeles_combines_a50())
      sel <- getReactableState("table_a50_comb", "selected")
      if (is.null(sel)) sel <- 1
      table_a50_comb()[sel, "modele_id"]
    })
    
    # ==== Résultats détaillés ----
    res_f <- reactive({
      req(afficher_modeles_separes_a50())
      
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = "TLO",
        lien = stringr::str_extract(selected_f(), "logit|probit|cloglog")
      )
    })
    
    res_m <- reactive({
      req(afficher_modeles_separes_a50())
      
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = "TLO",
        lien = stringr::str_extract(selected_m(), "logit|probit|cloglog")
      )
    })
    
    res_comb <- reactive({
      req(afficher_modeles_combines_a50())
      
      maturite_generate_modele(
        specimen(),
        variable = "age",
        modele = stringr::str_extract(selected_comb(), "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(selected_comb(), "logit|probit|cloglog")
      )
    })
    
    # ==== Section Femelles ----
    output$section_a50_femelles <- renderUI({
      if (!afficher_modeles_separes_a50()) {
        return(NULL)
      }
      
      table <- table_a50_f()
      if (nrow(table) == 0) return(NULL)
      
      tagList(
        tags$h3("Femelles"),
        tags$p("Modèles ajustés sur les femelles"),
        
        reactableOutput(ns("table_a50_f")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_a50_f"))),
        
        tags$hr(),
        
        uiOutput(ns("res_a50_f")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_res_a50_f"))),
        
        tags$hr(),
        
        plotOutput(ns("plot_a50_f"), height = "500px"),
        div(style = "margin-top: 10px;", downloadButton(ns("dl_plot_a50_f"), "Télécharger")),
        
        tags$hr(style = "margin: 30px 0;")
      )
    })
    
    output$table_a50_f <- renderReactable({
      req(afficher_modeles_separes_a50())
      reactable(as.data.frame(table_a50_f()), selection = "single")
    })
    
    # ==== Section Mâles ----
    output$section_a50_males <- renderUI({
      if (!afficher_modeles_separes_a50()) {
        return(NULL)
      }
      
      table <- table_a50_m()
      if (nrow(table) == 0) return(NULL)
      
      tagList(
        tags$h3("Mâles"),
        tags$p("Modèles ajustés sur les mâles"),
        
        reactableOutput(ns("table_a50_m")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_a50_m"))),
        
        tags$hr(),
        
        uiOutput(ns("res_a50_m")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_res_a50_m"))),
        
        tags$hr(),
        
        plotOutput(ns("plot_a50_m"), height = "500px"),
        div(style = "margin-top: 10px;", downloadButton(ns("dl_plot_a50_m"), "Télécharger")),
        
        tags$hr(style = "margin: 30px 0;")
      )
    })
    
    output$table_a50_m <- renderReactable({
      req(afficher_modeles_separes_a50())
      reactable(as.data.frame(table_a50_m()), selection = "single")
    })
    
    # ==== Section combinée ----
    output$section_a50_combine <- renderUI({
      if (!afficher_modeles_combines_a50()) {
        return(NULL)
      }
      
      table <- table_a50_comb()
      if (nrow(table) == 0) return(NULL)
      
      tagList(
        tags$h3("Modèles combinés"),
        tags$p("Modèles ajustés sur l'ensemble des données"),
        
        reactableOutput(ns("table_a50_comb")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_a50_comb"))),
        
        tags$hr(),
        
        uiOutput(ns("res_a50_comb")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_res_a50_comb"))),
        
        tags$hr(),
        
        plotOutput(ns("plot_a50_comb"), height = "500px"),
        div(style = "margin-top: 10px;", downloadButton(ns("dl_plot_a50_comb"), "Télécharger")),
        
        tags$hr(style = "margin: 30px 0;")
      )
    })
    
    output$table_a50_comb <- renderReactable({
      req(afficher_modeles_combines_a50())
      reactable(as.data.frame(table_a50_comb()), selection = "single")
    })
    
    # ==== Affichage des tableaux de résultats ----
    render_table_flextable(
      "res_a50_f",
      reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_f()))
        req(!is.null(res_f()$table_resultats_flextable))
        res_f()$table_resultats_flextable
      })
    )
    
    render_table_flextable(
      "res_a50_m",
      reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_m()))
        req(!is.null(res_m()$table_resultats_flextable))
        res_m()$table_resultats_flextable
      })
    )
    
    render_table_flextable(
      "res_a50_comb",
      reactive({
        req(afficher_modeles_combines_a50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$table_resultats_flextable))
        res_comb()$table_resultats_flextable
      })
    )
    
    # ==== Affichage des graphiques ----
    render_plot_ggplot(
      output_id = "plot_a50_f",
      plot = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_f()))
        req(!is.null(res_f()$graphique))
        res_f()$graphique
      })
    )
    
    render_plot_ggplot(
      output_id = "plot_a50_m",
      plot = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_m()))
        req(!is.null(res_m()$graphique))
        res_m()$graphique
      })
    )
    
    render_plot_ggplot(
      output_id = "plot_a50_comb",
      plot = reactive({
        req(afficher_modeles_combines_a50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$graphique))
        res_comb()$graphique
      })
    )
    
    # ==== Téléchargement des graphiques ----
    render_download_plot(
      id = "dl_plot_a50_f",
      plot = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_f()))
        req(!is.null(res_f()$graphique))
        res_f()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    render_download_plot(
      id = "dl_plot_a50_m",
      plot = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_m()))
        req(!is.null(res_m()$graphique))
        res_m()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    render_download_plot(
      id = "dl_plot_a50_comb",
      plot = reactive({
        req(afficher_modeles_combines_a50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$graphique))
        res_comb()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    # ==== Téléchargement des tableaux de sélection ----
    render_download_table(
      id = "dl_a50_f",
      data = reactive({
        req(afficher_modeles_separes_a50())
        table_a50_f()
      }),
      filename = reactive(
        build_export_filename("a50_modeles_femelles", filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_a50_m",
      data = reactive({
        req(afficher_modeles_separes_a50())
        table_a50_m()
      }),
      filename = reactive(
        build_export_filename("a50_modeles_males", filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_a50_comb",
      data = reactive({
        req(afficher_modeles_combines_a50())
        table_a50_comb()
      }),
      filename = reactive(
        build_export_filename("a50_modeles_combines", filename_suffix())
      )
    )
    
    # ==== Téléchargement des tableaux de résultats ----
    render_download_table(
      id = "dl_res_a50_f",
      data = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_f()))
        req(!is.null(res_f()$table_resultats))
        res_f()$table_resultats
      }),
      filename = reactive(
        build_export_filename("a50_resultats_femelles", filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_res_a50_m",
      data = reactive({
        req(afficher_modeles_separes_a50())
        req(!is.null(res_m()))
        req(!is.null(res_m()$table_resultats))
        res_m()$table_resultats
      }),
      filename = reactive(
        build_export_filename("a50_resultats_males", filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_res_a50_comb",
      data = reactive({
        req(afficher_modeles_combines_a50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$table_resultats))
        res_comb()$table_resultats
      }),
      filename = reactive(
        build_export_filename("a50_resultats_combines", filename_suffix())
      )
    )
  })
}