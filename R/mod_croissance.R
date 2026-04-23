#' croissance UI Function
#'
#' @description Module Shiny pour la modélisation de la croissance des spécimens.
#'
#' @param id Identifiant du module.
#'
#' @noRd
mod_croissance_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Croissance",
    
    uiOutput(ns("croissance_intro")),
    uiOutput(ns("croissance_message")),
    uiOutput(ns("croissance_table_section")),
    uiOutput(ns("croissance_plot_section"))
    
  )
  
}

#' croissance Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_croissance_server <- function(id, specimen, filename_suffix) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # Résultat du module croissance ----
    
    croissance_res <- reactive({
      
      req(specimen())
      
      croissance_compare_modele(data = specimen())
      
    })
    
    
    # Introduction ----
    
    output$croissance_intro <- renderUI({
      
      res <- croissance_res()
      
      req(res$success)
      req(!is.null(res$data))
      
      aucun_modele_converge <- all(
        res$data$convergence == "Le modèle n'a pas convergé"
      )
      
      if (isTRUE(aucun_modele_converge)) {
        return(NULL)
      }
      
      p(
        "Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc. ",
        "Prenez note également que le modèle de von Bertalanffy utilise la méthode pondérée ",
        "avec t₀ variable. Attention : les IC95 % des prédictions ne peuvent pas être calculés ",
        "à partir des IC95 % des estimations des paramètres L, K et t₀."
      )
    })
    
    # Message utilisateur ----
    
    output$croissance_message <- renderUI({
      
      res <- croissance_res()
      
      if (is.null(res$message) || identical(res$message, "")) {
        return(NULL)
      }
      
      titre_message <- if (isTRUE(res$success)) {
        "Attention"
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
      
      message_parts <- strsplit(res$message, "\n\n", fixed = TRUE)[[1]]
      
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
          lapply(message_parts, function(message_i) {
            tags$p(
              style = "margin: 0 0 8px 0;",
              message_i
            )
          })
        )
        
      )
      
    })
    
    # Table des modèles ----
    
    table_modeles_croissance <- reactive({
      
      res <- croissance_res()
      
      req(res$success)
      req(!is.null(res$data))
      
      res$data
      
    })
    
    
    # Modèle par défaut ----
    
    default_model_index <- reactive({
      
      table <- table_modeles_croissance()
      
      best_model <- croissance_select_best_modele(table)
      idx <- match(best_model, table$methode)
      
      if (is.na(idx)) {
        return(NA_integer_)
      }
      
      idx
      
    })
    
    
    # Section tableau ----
    
    output$croissance_table_section <- renderUI({
      
      res <- croissance_res()
      
      req(res$success)
      req(!is.null(res$data))
      
      tagList(
        h3("Table de sélection du modèle de croissance"),
        
        withSpinner(
          reactableOutput(ns("table_modeles_croissance_table")),
          type = myspinner
        ),
        
        uiOutput(ns("download_table_modeles_croissance_ui")),
        
        br()
      )
      
    })
    
    
    # Tableau interactif ----
    
    output$table_modeles_croissance_table <- renderReactable({
      
      table <- table_modeles_croissance()
      index_defaut <- default_model_index()
      
      reactable(
        as.data.frame(table),
        selection = "single",
        onClick = "select",
        defaultSelected = if (is.na(index_defaut)) NULL else index_defaut,
        
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        ),
        
        columns = list(
          methode = colDef(name = "Modèles"),
          
          l_inf = colDef(
            name = "L∞",
            format = colFormat(digits = 0,  locales = "fr-CA")
          ),
          
          l_inf_ic = colDef(name = "L∞ IC 95%"),
          
          k = colDef(
            name = "K",
            format = colFormat(digits = 3, locales = "fr-CA" )
          ),
          
          k_ic = colDef(name = "K IC 95%"),
          
          t0 = colDef(
            name = "t\u2080",
            format = colFormat(digits = 3, locales = "fr-CA")
          ),
          
          t0_ic = colDef(name = "t\u2080 IC 95%"),
          
          aicc = colDef(
            name = "AICc",
            format = colFormat(digits = 2, locales = "fr-CA")
          ),
          
          delta_aicc = colDef(
            name = "Δ AICc",
            format = colFormat(digits = 2, locales = "fr-CA")
          ),
          
          aiccwt = colDef(
            name = "Poids d'Akaike",
            format = colFormat(digits = 2, locales = "fr-CA")
          ),
          
          convergence = colDef(name = "Convergence")
        )
        
      )
      
    })
    
    
    # Modèle sélectionné ----
    
    selectedmodelcroissance <- reactive({
      
      table <- table_modeles_croissance()
      
      selected <- getReactableState(
        outputId = "table_modeles_croissance_table",
        name = "selected"
      )
      
      if (is.null(selected)) {
        selected <- default_model_index()
      }
      
      if (is.null(selected) || is.na(selected) || selected < 1 || selected > nrow(table)) {
        return(NULL)
      }
      
      modele <- table$methode[[selected]]
      convergence <- table$convergence[[selected]]
      
      if (!identical(convergence, "Convergé")) {
        return(NULL)
      }
      
      modele
    })
    
    
    # Téléchargement table ----
    
    output$download_table_modeles_croissance_ui <- renderUI({
      
      res <- croissance_res()
      
      req(res$success)
      req(!is.null(res$data))
      
      download_button_ui(ns("download_table_modeles_croissance"))
      
    })
    
    
    render_download_table(
      id = "download_table_modeles_croissance",
      data = table_modeles_croissance,
      filename = reactive(build_export_filename("croissance_modeles", filename_suffix()))
    )
    
    
    # Graphique ----
    
    plot_selectedmodelcroissance <- reactive({
      
      req(croissance_res()$success)
      
      modele_selectionne <- selectedmodelcroissance()
      
      req(!is.null(modele_selectionne))
      
      croissance_plot(
        dfspecimen = specimen(),
        tablemodele = table_modeles_croissance(),
        modele = modele_selectionne
      )
      
    })
    
    
    # Disponibilité du graphique ----
    
    plot_croissance_disponible <- reactive({
      
      !is.null(selectedmodelcroissance()) && !is.null(plot_selectedmodelcroissance())
      
    })
    
    
    # Section graphique ----
    
    output$croissance_plot_section <- renderUI({
      
      req(croissance_res()$success)
      
      if (!isTRUE(plot_croissance_disponible())) {
        return(NULL)
      }
      
      tagList(
        p(
          "Le graphique suivant illustre la longueur observée des spécimens en fonction de leur âge, ",
          "ainsi que la courbe de croissance modélisée selon le modèle sélectionné."
        ),
        
        h3("Longueur à l'âge des spécimens capturés et modèle de croissance"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          
          withSpinner(
            plotOutput(ns("selectedmodelcroissanceplot"), height = "500px"),
            type = myspinner
          ),
          
          br(),
          
          uiOutput(ns("download_selectedmodelcroissanceplot_ui"))
        )
      )
      
    })
    
    
    # Affichage du graphique ----
    
    render_plot_ggplot(
      output_id = "selectedmodelcroissanceplot",
      plot = plot_selectedmodelcroissance
    )
    
    
    # Téléchargement graphique ----
    
    output$download_selectedmodelcroissanceplot_ui <- renderUI({
      
      req(croissance_res()$success)
      req(plot_croissance_disponible())
      
      downloadButton(
        outputId = ns("download_selectedmodelcroissanceplot"),
        label = "Téléchargement du graphique"
      )
      
    })
    
    
    render_download_plot(
      id = "download_selectedmodelcroissanceplot",
      plot = plot_selectedmodelcroissance,
      filename = "courbe_croissance"
    )
    
  })
  
}