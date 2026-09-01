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
    
    withSpinner(
      uiOutput(ns("section_l50_separes")),
      type = myspinner
    ),
    
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
    
    # ==== Résultat global ----
    table_modeles_l50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        variable = "ltm"
      )
    })
    
    # ==== Disponibilité des modèles ----
    
    has_model_l50_f <- reactive({
      res <- table_modeles_l50_resultats()
      req(!is.null(res))
      
      !is.null(res$best_model$best_model_F)
    })
    
    has_model_l50_m <- reactive({
      res <- table_modeles_l50_resultats()
      req(!is.null(res))
      
      !is.null(res$best_model$best_model_M)
    })
    
    has_model_l50_comb <- reactive({
      res <- table_modeles_l50_resultats()
      req(!is.null(res))
      
      !is.null(res$best_model$best_model_combined)
    })
    
    # ==== Affichage des modèles combinés ----
    
    afficher_modeles_combines_l50 <- reactive({
      has_model_l50_comb()
    })
    
    # ==== Message complémentaire ----
    
    message_complementaire_l50 <- reactive({
      res <- table_modeles_l50_resultats()
      req(!is.null(res))
      
      best_model <- res$best_model
      
      has_F <- !is.null(best_model$best_model_F)
      has_M <- !is.null(best_model$best_model_M)
      has_comb <- !is.null(best_model$best_model_combined)
      
      if (!has_F && !has_M && !has_comb) {
        return(
          HTML(
            "Les données disponibles ne permettent pas d'ajuster des modèles de maturité."
          )
        )
      }
      
      if (has_F && has_M && has_comb) {
        return(
          HTML(
            "Des modèles séparés ont été retenus pour les femelles et les mâles. Les modèles pour les sexes combinés sont également présentés à titre comparatif."
          )
        )
      }
      
      if (has_F && has_M && !has_comb) {
        return(
          HTML(
            "Des modèles séparés ont été retenus pour les femelles et les mâles. Aucun modèle pour les sexes combinés valide n'est disponible."
          )
        )
      }
      
      if (has_F && !has_M && has_comb) {
        return(
          HTML(
            "Un modèle séparé a été retenu pour les femelles. Aucun modèle valide n'a pu être retenu pour les mâles. Les modèles pour les sexes combinés sont également présentés."
          )
        )
      }
      
      if (!has_F && has_M && has_comb) {
        return(
          HTML(
            "Un modèle séparé a été retenu pour les mâles. Aucun modèle valide n'a pu être retenu pour les femelles. Les modèles pour les sexes combinés sont également présentés."
          )
        )
      }
      
      if (!has_F && !has_M && has_comb) {
        return(
          HTML(
            "Aucun modèle séparé valide n'a pu être retenu. Les modèles pour les sexes combinés sont présentés."
          )
        )
      }
      
      if (has_F && !has_M && !has_comb) {
        return(
          HTML(
            "Un modèle séparé a été retenu pour les femelles. Aucun modèle valide n'a pu être retenu pour les mâles et aucun modèle pour les sexes combinés valide n'est disponible."
          )
        )
      }
      
      if (!has_F && has_M && !has_comb) {
        return(
          HTML(
            "Un modèle séparé a été retenu pour les mâles. Aucun modèle valide n'a pu être retenu pour les femelles et aucun modèle pour les sexes combinés valide n'est disponible."
          )
        )
      }
      
      HTML(
        "Les données disponibles ne permettent pas d'ajuster suffisamment de modèles de maturité."
      )
    })
    
    # ==== Message UI ----
    output$message_l50 <- renderUI({
      res <- table_modeles_l50_resultats()
      req(!is.null(res))
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      couleur_bordure <- if (isTRUE(res$success)) "#4c6ef5" else "#c0392b"
      couleur_fond <- if (isTRUE(res$success)) "#f5f7ff" else "#fdf2f2"
      
      msg <- res$message
      
      div(
        style = paste(
          "margin: 15px 0;",
          "padding: 12px 16px;",
          "border-left: 4px solid", couleur_bordure, ";",
          "background-color:", couleur_fond
        ),
        tags$h4("Sélection des modèles L50"),
        tags$p(msg),
        tags$p(message_complementaire_l50())
      )
    })
    
    # ==== Tables de sélection ----
    table_l50_f <- reactive({
      table_modeles_l50_resultats()$table_sep_F$df
    })
    
    table_l50_m <- reactive({
      table_modeles_l50_resultats()$table_sep_M$df
    })
    
    table_l50_comb <- reactive({
      table_modeles_l50_resultats()$table_comb$df
    })
    
    # ==== Sélection des modèles ----
    default_index_l50_f <- reactive({
      table <- table_l50_f()
      req(nrow(table) > 0)
      
      comparaison <- table_modeles_l50_resultats()
      best_id <- comparaison$best_model$best_model_F
      
      idx <- which(table$modele_id == best_id)
      if (length(idx) == 0) return(1)
      idx[1]
    })
    
    default_index_l50_m <- reactive({
      table <- table_l50_m()
      req(nrow(table) > 0)
      
      comparaison <- table_modeles_l50_resultats()
      best_id <- comparaison$best_model$best_model_M
      
      idx <- which(table$modele_id == best_id)
      if (length(idx) == 0) return(1)
      idx[1]
    })
    
    default_index_l50_comb <- reactive({
      table <- table_l50_comb()
      req(nrow(table) > 0)
      
      comparaison <- table_modeles_l50_resultats()
      best_id <- comparaison$best_model$best_model_combined
      
      idx <- which(table$modele_id == best_id)
      if (length(idx) == 0) return(1)
      idx[1]
    })   
    
    selected_f <- reactive({
      req(has_model_l50_f())
      sel <- getReactableState("table_l50_f", "selected")
     
      if (is.null(sel)) {
        sel <- default_index_l50_f()
      }
      
      table_l50_f()[sel, "modele_id"]
    })
    
    selected_m <- reactive({
      req(has_model_l50_m())
      sel <- getReactableState("table_l50_m", "selected")
      
      if (is.null(sel)) {
        sel <- default_index_l50_m()
      }
      
      table_l50_m()[sel, "modele_id"]
    })
    
    selected_comb <- reactive({
      req(afficher_modeles_combines_l50())
      sel <- getReactableState("table_l50_comb", "selected")
      if (is.null(sel)) sel <- default_index_l50_comb()
      table_l50_comb()[sel, "modele_id"]
    })
    
    # ==== Résultats détaillés ----
    res_f <- reactive({
      req(has_model_l50_f())
      comparaison <- table_modeles_l50_resultats()
      modele_id <- selected_f()$modele_id
      best_glm <- comparaison$models_sep[[modele_id]]
      maturite_generate_modele(
        specimen(),
        variable = "ltm",
        modele = "TLO",
        lien = stringr::str_extract(
          modele_id,
          "logit|probit|cloglog"
        ),
        sexe = "F",
        modele_glm = best_glm
      )
    })
    
    res_m <- reactive({
      req(has_model_l50_m())
      comparaison <- table_modeles_l50_resultats()
      modele_id <- selected_m()$modele_id
      best_glm <- comparaison$models_sep[[modele_id]]
      maturite_generate_modele(
        specimen(),
        variable = "ltm",
        modele = "TLO",
        lien = stringr::str_extract(
          modele_id,
          "logit|probit|cloglog"
        ),
        sexe = "M",
        modele_glm = best_glm
      )
    })
    
    res_comb <- reactive({
      req(afficher_modeles_combines_l50())
      comparaison <- table_modeles_l50_resultats()
      modele_id <- selected_comb()$modele_id
      best_glm <- comparaison$models_comb[[modele_id]]
      maturite_generate_modele(
        specimen(),
        variable = "ltm",
        modele = stringr::str_extract(
          modele_id,
          "TLO|ADD|INT|COM"
        ),
        lien = stringr::str_extract(
          modele_id,
          "logit|probit|cloglog"
        ),
        sexe = NULL,
        modele_glm = best_glm
      )
    })
    
    
    # ==== Section modèles séparés ----
    output$section_l50_separes <- renderUI({
      
      # ------------------------------------------------------------
      # Tableaux de comparaison des modèles
      # ------------------------------------------------------------
      
      table_f <- table_l50_f()
      table_m <- table_l50_m()
      
      tagList(
        
        tags$h3("Modèles séparés"),
        
        tags$p(
          "Comparaison des modèles ajustés séparément pour les femelles et les mâles."
        ),
        
        # ==========================================================
        # FEMELLES
        # ==========================================================
        
        tags$h4("Femelles"),
        
        if (has_model_l50_f()) {
          tagList(
            reactableOutput(ns("table_l50_f")),
            
            div(
              style = "margin: 10px 0 20px 0;",
              download_button_ui(ns("dl_l50_f"))
            )
          )
          
        } else {
          
          div(
            style = paste(
              "margin: 10px 0 20px 0;",
              "padding: 12px 16px;",
              "border-left: 4px solid #c0392b;",
              "background-color: #fdf2f2;"
            ),
            "Aucun modèle valide n'est disponible pour les femelles."
          )
        },
        
        tags$hr(),
        
        # ==========================================================
        # MÂLES
        # ==========================================================
        
        tags$h4("Mâles"),
        
        if (has_model_l50_m()) {
          
          tagList(
            reactableOutput(ns("table_l50_m")),
            
            div(
              style = "margin: 10px 0 20px 0;",
              download_button_ui(ns("dl_l50_m"))
            )
          )
          
        } else {
          
          div(
            style = paste(
              "margin: 10px 0 20px 0;",
              "padding: 12px 16px;",
              "border-left: 4px solid #c0392b;",
              "background-color: #fdf2f2;"
            ),
            "Aucun modèle valide n'est disponible pour les mâles."
          )
        },
        
        tags$hr(),
        
    
        # ==========================================================
        # GRAPHIQUES F / M EN DEUX COLONNES
        # ==========================================================
        
        fluidRow(
          
          # --------------------------------------------------------
          # Femelles
          # --------------------------------------------------------
          
          column(
            width = 6,
            
            if (has_model_l50_f()) {
              
              tagList(
                plotOutput(
                  ns("plot_l50_f"),
                  height = "500px"
                ),
                
                div(
                  style = "margin-top: 10px;",
                  downloadButton(
                    ns("dl_plot_l50_f"),
                    "Télécharger"
                  )
                )
              )
              
            } else {
              
              div(
                style = paste(
                  "margin: 10px 0;",
                  "padding: 12px 16px;",
                  "border-left: 4px solid #c0392b;",
                  "background-color: #fdf2f2;"
                ),
                "Aucun graphique disponible pour les femelles."
              )
            }
          ),
          
          # --------------------------------------------------------
          # Mâles
          # --------------------------------------------------------
          
          column(
            width = 6,
            
            if (has_model_l50_m()) {
              
              tagList(
                plotOutput(
                  ns("plot_l50_m"),
                  height = "500px"
                ),
                
                div(
                  style = "margin-top: 10px;",
                  downloadButton(
                    ns("dl_plot_l50_m"),
                    "Télécharger"
                  )
                )
              )
              
            } else {
              
              div(
                style = paste(
                  "margin: 10px 0;",
                  "padding: 12px 16px;",
                  "border-left: 4px solid #c0392b;",
                  "background-color: #fdf2f2;"
                ),
                "Aucun graphique disponible pour les mâles."
              )
            }
          )
        ),
        
        tags$hr(
          style = "margin: 30px 0;"
        )
      )
    })
    
    
    # ==============================================================
    # Tableau de comparaison des modèles — FEMELLES
    # ==============================================================
    
    output$table_l50_f <- renderReactable({
      
      req(has_model_l50_f())
      
      idx <- default_index_l50_f()
      
      reactable(
        as.data.frame(table_l50_f()),
        
        selection = "single",
        
        defaultSelected = if (is.na(idx)) NULL else idx,
        
        pagination = FALSE,
        showPageInfo = FALSE,
        compact = TRUE,
        outlined = TRUE,
        defaultPageSize = 20,
        
        onClick = "select",
        
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(
            textAlign = "center"
          ),
          na = "-"
        ),
        
        columns = list(
          
          modele_id = colDef(name = "Modèle"),
          
          Convergence = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
              }
            ),
          
          Ajustement = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
            }
          )
          )
        )
      })
    
    
    # ==============================================================
    # Tableau de comparaison des modèles — MÂLES
    # ==============================================================
    
    output$table_l50_m <- renderReactable({
      
      req(has_model_l50_m())
      
      idx <- default_index_l50_m()
      
      reactable(
        as.data.frame(table_l50_m()),
        
        selection = "single",
        
        defaultSelected = if (is.na(idx)) NULL else idx,
        
        pagination = FALSE,
        showPageInfo = FALSE,
        compact = TRUE,
        outlined = TRUE,
        defaultPageSize = 20,
        
        onClick = "select",
        
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(
            textAlign = "center"
          ),
          na = "-"
        ),
        
        columns = list(
          
          modele_id = colDef(name = "Modèle"),
          
          Convergence = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
            }
          ),
          
          Ajustement = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
            }
          )
        )
        )
    })
    
    # ==== Section combinée ----
    output$section_l50_combine <- renderUI({
      if (!afficher_modeles_combines_l50()) {
        return(NULL)
      }
      
      table <- table_l50_comb()
      if (nrow(table) == 0) return(NULL)
      
      tagList(
        tags$h3("Modèles combinés"),
        tags$p("Modèles ajustés sur l'ensemble des données"),
        
        reactableOutput(ns("table_l50_comb")),
        div(style = "margin: 10px 0 20px 0;", download_button_ui(ns("dl_l50_comb"))),
        
        tags$hr(),
        
        plotOutput(ns("plot_l50_comb"), height = "500px"),
        div(style = "margin-top: 10px;", downloadButton(ns("dl_plot_l50_comb"), "Télécharger")),
        
        tags$hr(style = "margin: 30px 0;")
      )
    })
    
    output$table_l50_comb <- renderReactable({
      req(afficher_modeles_combines_l50())
      
      idx <- default_index_l50_comb()
      
      reactable(
        as.data.frame(table_l50_comb()),
        pagination = FALSE,
        showPageInfo = FALSE,
        defaultSelected = if (is.na(idx)) NULL else idx,
        compact = TRUE,
        outlined = TRUE,
        defaultPageSize = 20,
        selection = "single",
        onClick = "select",
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center"),
          na = "-"
        ),
        columns = list(
          
          modele_id = colDef(name = "Modèle"),
          
          Convergence = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
            }
          ),
          
          Ajustement = colDef(
            cell = function(value) {
              if (isTRUE(value)) "\u2713" else "\u2717"
            },
            style = function(value) {
              if (isTRUE(value)) {
                list(color = "#2E7D32", fontWeight = "bold")
              } else {
                list(color = "#D32F2F", fontWeight = "bold")
              }
            }
          )
        )
      )
    })
    
    # ==== Affichage des graphiques ----
    render_plot_ggplot(
      output_id = "plot_l50_f",
      plot = reactive({
        req(has_model_l50_f())
        req(!is.null(res_f()))
        req(!is.null(res_f()$graphique))
        
        res_f()$graphique
      })
    )
    
    render_plot_ggplot(
      output_id = "plot_l50_m",
      plot = reactive({
        req(has_model_l50_m())
        req(!is.null(res_m()))
        req(!is.null(res_m()$graphique))
        
        res_m()$graphique
      })
    )
    
    render_plot_ggplot(
      output_id = "plot_l50_comb",
      plot = reactive({
        req(afficher_modeles_combines_l50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$graphique))
        res_comb()$graphique
      })
    )
    
    # ==== Téléchargement des graphiques ----
    render_download_plot(
      id = "dl_plot_l50_f",
      plot = reactive({
        req(has_model_l50_f())
        req(!is.null(res_f()))
        req(!is.null(res_f()$graphique))
        
        res_f()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    render_download_plot(
      id = "dl_plot_l50_m",
      plot = reactive({
        req(has_model_l50_m())
        req(!is.null(res_m()))
        req(!is.null(res_m()$graphique))
        
        res_m()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    render_download_plot(
      id = "dl_plot_l50_comb",
      plot = reactive({
        req(afficher_modeles_combines_l50())
        req(!is.null(res_comb()))
        req(!is.null(res_comb()$graphique))
        res_comb()$graphique
      }),
      filename_suffix = filename_suffix()
    )
    
    # ==== Téléchargement des tableaux de sélection ----
    render_download_table(
      id = "dl_l50_f",
      data = reactive({
        req(has_model_l50_f())
        table_l50_f()
      }),
      filename = reactive(
        build_export_filename("l50_modeles_femelles",filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_l50_m",
      data = reactive({
        req(has_model_l50_m())
        table_l50_m()
      }),
      filename = reactive(
        build_export_filename("l50_modeles_males",filename_suffix())
      )
    )
    
    render_download_table(
      id = "dl_l50_comb",
      data = reactive({
        req(afficher_modeles_combines_l50())
        table_l50_comb()
      }),
      filename = reactive(
        build_export_filename("l50_modeles_combines", filename_suffix())
      )
    )
    })
}