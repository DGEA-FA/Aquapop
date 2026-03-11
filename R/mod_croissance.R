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
      
      p(
        "Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc. ",
        "Prenez note également que le modèle de von Bertalanffy utilise la méthode pondérée ",
        "avec t₀ variable. Attention : les IC95 % des prédictions ne peuvent pas être calculés ",
        "à partir des IC95 % des estimations des paramètres L, K et t₀."
      )
      
    })
    
    
    # Message analyse non disponible ----
    
    output$croissance_message <- renderUI({
      
      res <- croissance_res()
      
      if (res$success) {
        return(NULL)
      }
      
      div(
        style = paste(
          "margin: 15px 0;",
          "padding: 12px 16px;",
          "border-left: 4px solid #c0392b;",
          "background-color: #fdf2f2;",
          "color: #333;"
        ),
        
        tags$h4(
          style = "margin-top: 0; margin-bottom: 8px;",
          "Analyse non disponible"
        ),
        
        tags$p(
          style = "margin: 0;",
          res$message
        )
        
      )
      
    })
    
    
    # Table des modèles ----
    
    table_modeles_croissance <- reactive({
      
      res <- croissance_res()
      
      req(res$success)
      
      res$data
      
    })
    
    
    # Modèle par défaut ----
    
    default_model_index <- reactive({
      
      table <- table_modeles_croissance()
      
      best_model <- croissance_select_best_modele(table)
      idx <- match(best_model, table$methode)
      
      req(!is.na(idx))
      
      idx
      
    })
    
    
    # Section tableau ----
    
    output$croissance_table_section <- renderUI({
      
      req(croissance_res()$success)
      
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
      
      reactable(
        as.data.frame(table),
        selection = "single",
        onClick = "select",
        defaultSelected = default_model_index(),
        
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        ),
        
        columns = list(
          methode = colDef(name = "Modèles"),
          l_inf = colDef(name = "L∞"),
          l_inf_ic = colDef(name = "L∞ IC 95%"),
          k = colDef(name = "K"),
          k_ic = colDef(name = "K IC 95%"),
          t0 = colDef(name = "t\u2080"),
          t0_ic = colDef(name = "t\u2080 IC 95%"),
          aicc = colDef(name = "AICc"),
          delta_aicc = colDef(name = "Δ AICc"),
          aiccwt = colDef(name = "Poids d’Akaike"),
          converged = colDef(name = "Convergence")
        )
        
      )
      
    })
    
    
    # Modèle sélectionné ----
    
    selectedmodelcroissance <- reactive({
      
      table <- table_modeles_croissance()
      
      selected <- getReactableState("table_modeles_croissance_table", "selected")
      
      req(!is.null(selected), selected >= 1, selected <= nrow(table))
      
      table[selected, 1, drop = TRUE]
      
    })
    
    
    # Téléchargement table ----
    
    output$download_table_modeles_croissance_ui <- renderUI({
      
      req(croissance_res()$success)
      
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
      
      croissance_plot(
        dfspecimen = specimen(),
        tablemodele = table_modeles_croissance(),
        modele = selectedmodelcroissance()
      )
      
    })
    
    
    # Disponibilité du graphique ----
    
    plot_croissance_disponible <- reactive({
      
      req(croissance_res()$success)
      
      !is.null(plot_selectedmodelcroissance())
      
    })
    
    
    # Section graphique ----
    
    output$croissance_plot_section <- renderUI({
      
      req(croissance_res()$success)
      req(plot_croissance_disponible())
      
      tagList(
        
        p(
          "Le graphique suivant illustre la longueur observée des spécimens en fonction de leur âge, ",
          "ainsi que la courbe de croissance modélisée selon le modèle sélectionné."
        ),
        
        h3("Longueur à l’âge des spécimens capturés et modèle de croissance"),
        
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