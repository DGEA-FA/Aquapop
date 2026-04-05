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
    uiOutput(ns("table_modeles_l50_section")),
    uiOutput(ns("resultats_l50_section"))
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
    
    # Résultat des modèles évalués ----
    table_modeles_l50_resultats <- reactive({
      req(specimen())
      
      maturite_compare_modele(
        specimen_data = specimen(),
        prefer_combined = FALSE,
        variable = "ltm"
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
    
    # Section tableau des modèles ----
    output$table_modeles_l50_section <- renderUI({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      if (!isTRUE(res$success) || is.null(res$table$df) || nrow(res$table$df) == 0) {
        return(NULL)
      }
      
      tagList(
        h3(HTML("Tableau interactif des modèles évalués pour L<sub>50</sub>")),
        withSpinner(
          reactableOutput(ns("table_modeles_l50_table")),
          type = myspinner
        ),
        download_button_ui(ns("table_modeles_l50_dl")),
        br()
      )
    })
    
    # Affichage du tableau interactif ----
    output$table_modeles_l50_table <- renderReactable({
      res <- table_modeles_l50_resultats()
      
      req(isTRUE(res$success))
      req(!is.null(res$table$df))
      req(nrow(res$table$df) > 0)
      
      reactable(
        labelled_data(res$table$df),
        sortable = FALSE,
        highlight = TRUE,
        defaultPageSize = 20,
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        )
      )
    })
    
    # Téléchargement du tableau des modèles ----
    render_download_table(
      id = "table_modeles_l50_dl",
      data = reactive({
        res <- table_modeles_l50_resultats()
        
        req(isTRUE(res$success))
        req(!is.null(res$table$df))
        
        res$table$df
      }),
      filename = reactive(
        build_export_filename("modeles_maturite_l50", filename_suffix())
      )
    )
    
    # Déterminer quelles sections afficher ----
    sections_l50 <- reactive({
      res <- table_modeles_l50_resultats()
      
      req(!is.null(res))
      
      best_model <- res$best_model
      
      modele_f <- best_model$best_model_F
      modele_m <- best_model$best_model_M
      modele_comb <- best_model$best_model_combined
      
      sections <- list()
      
      # Cas 1 : les deux modèles séparés existent
      if (!is.null(modele_f) && !is.null(modele_m)) {
        sections$femelles <- list(
          id = "femelles",
          titre = "Femelles",
          modele = modele_f,
          suffixe = "femelles"
        )
        
        sections$males <- list(
          id = "males",
          titre = "Mâles",
          modele = modele_m,
          suffixe = "males"
        )
        
        return(sections)
      }
      
      # Cas 2 : seul le modèle femelles existe
      if (!is.null(modele_f) && is.null(modele_m)) {
        sections$femelles <- list(
          id = "femelles",
          titre = "Femelles",
          modele = modele_f,
          suffixe = "femelles"
        )
        
        if (!is.null(modele_comb)) {
          sections$males <- list(
            id = "males",
            titre = "Mâles (modèle combiné utilisé)",
            modele = modele_comb,
            suffixe = "males"
          )
        }
        
        return(sections)
      }
      
      # Cas 3 : seul le modèle mâles existe
      if (is.null(modele_f) && !is.null(modele_m)) {
        if (!is.null(modele_comb)) {
          sections$femelles <- list(
            id = "femelles",
            titre = "Femelles (modèle combiné utilisé)",
            modele = modele_comb,
            suffixe = "femelles"
          )
        }
        
        sections$males <- list(
          id = "males",
          titre = "Mâles",
          modele = modele_m,
          suffixe = "males"
        )
        
        return(sections)
      }
      
      # Cas 4 : aucun modèle séparé, mais modèle combiné disponible
      if (is.null(modele_f) && is.null(modele_m) && !is.null(modele_comb)) {
        sections$combine <- list(
          id = "combine",
          titre = "Modèle combiné",
          modele = modele_comb,
          suffixe = "combine"
        )
        
        return(sections)
      }
      
      # Cas 5 : rien de disponible
      return(sections)
    })
    
    # Générer les résultats pour chaque section ----
    resultats_l50_sections <- reactive({
      req(specimen())
      
      sections <- sections_l50()
      
      if (length(sections) == 0) {
        return(list())
      }
      
      lapply(sections, function(section_info) {
        res_modele <- maturite_generate_modele(
          data = specimen(),
          variable = section_info$modele$variable,
          modele = section_info$modele$modele,
          lien = section_info$modele$lien
        )
        
        list(
          info = section_info,
          resultat = res_modele
        )
      })
    })
    
    # Section résultats ----
    output$resultats_l50_section <- renderUI({
      sections_res <- resultats_l50_sections()
      
      req(!is.null(sections_res))
      
      if (length(sections_res) == 0) {
        return(NULL)
      }
      
      blocs_ui <- lapply(sections_res, function(section_res) {
        info <- section_res$info
        res <- section_res$resultat
        
        if (!isTRUE(res$success)) {
          return(NULL)
        }
        
        table_output_id <- paste0("ogive_l50_", info$id, "_table")
        table_dl_id <- paste0("ogive_l50_", info$id, "_table_dl")
        plot_output_id <- paste0("plot_ogive_l50_", info$id)
        plot_dl_id <- paste0("download_ogive_l50_", info$id, "_plot")
        
        tagList(
          h3(info$titre),
          uiOutput(ns(table_output_id)),
          download_button_ui(ns(table_dl_id)),
          br(),
          div(
            style = "max-width: 900px; margin: auto;",
            withSpinner(
              plotOutput(ns(plot_output_id), height = "500px"),
              type = myspinner
            ),
            br(),
            downloadButton(
              ns(plot_dl_id),
              "Téléchargement du graphique"
            )
          ),
          br()
        )
      })
      
      do.call(tagList, blocs_ui)
    })
    
    # Sorties dynamiques ----
    observe({
      sections_res <- resultats_l50_sections()
      
      if (length(sections_res) == 0) {
        return()
      }
      
      for (section_res in sections_res) {
        local({
          info <- section_res$info
          res_reactive <- reactive({
            resultats_l50_sections()[[info$id]]$resultat
          })
          
          table_output_id <- paste0("ogive_l50_", info$id, "_table")
          table_dl_id <- paste0("ogive_l50_", info$id, "_table_dl")
          plot_output_id <- paste0("plot_ogive_l50_", info$id)
          plot_dl_id <- paste0("download_ogive_l50_", info$id, "_plot")
          
          render_table_flextable(
            table_output_id,
            reactive({
              res <- res_reactive()
              
              req(isTRUE(res$success))
              req(!is.null(res$table_resultats_flextable))
              
              res$table_resultats_flextable
            })
          )
          
          render_download_table(
            id = table_dl_id,
            data = reactive({
              res <- res_reactive()
              
              req(isTRUE(res$success))
              req(!is.null(res$table_resultats))
              
              res$table_resultats
            }),
            filename = reactive(
              build_export_filename(
                paste0("ogive_maturite_l50_", info$suffixe),
                filename_suffix()
              )
            )
          )
          
          render_plot_ggplot(
            plot_output_id,
            reactive({
              res <- res_reactive()
              
              req(isTRUE(res$success))
              req(!is.null(res$graphique))
              
              res$graphique
            })
          )
          
          render_download_plot(
            id = plot_dl_id,
            plot = reactive({
              res <- res_reactive()
              
              req(isTRUE(res$success))
              req(!is.null(res$graphique))
              
              res$graphique
            }),
            filename_suffix = paste0(filename_suffix(), "_", info$suffixe)
          )
        })
      }
    })
  })
}