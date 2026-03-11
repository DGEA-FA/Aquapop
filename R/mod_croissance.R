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
    
    # Résultat sécurisé du module croissance ----
    croissance_res <- reactive({
      req(specimen())
      
      tryCatch(
        croissance_compare_modele(data = specimen()),
        error = function(e) {
          list(
            data = NULL,
            flextable = NULL,
            error = conditionMessage(e)
          )
        }
      )
    })
    
    # Introduction du module (affichée seulement si pas d'erreur) ----
    output$croissance_intro <- renderUI({
      req(is.null(croissance_res()$error))
      
      tagList(
        p(
          "Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc. ",
          "Prenez note également que le modèle de von Bertalanffy utilise la méthode pondérée ",
          "avec t₀ variable. Attention : les IC95 % des prédictions ne peuvent pas être calculés ",
          "à partir des IC95 % des estimations des paramètres L, K et t₀."
        )
      )
    })
    
    # Message d'erreur du module ----
    output$croissance_message <- renderUI({
      res <- croissance_res()
      
      if (is.null(res$error)) {
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
          res$error
        )
      )
    })
    
    # Table des modèles de croissance ----
    table_modeles_croissance <- reactive({
      res <- croissance_res()
      
      if (!is.null(res$error)) {
        return(NULL)
      }
      
      res$data
    })
    
    # Index du meilleur modèle (pour sélection par défaut) ----
    default_model_index <- reactive({
      table <- table_modeles_croissance()
      req(!is.null(table), nrow(table) > 0)
      
      best_model <- croissance_select_best_modele(table)
      idx <- match(best_model, table$methode)
      
      validate(
        need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats.")
      )
      
      idx
    })
    
    # Section tableau ----
    output$croissance_table_section <- renderUI({
      req(is.null(croissance_res()$error))
      
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
      req(!is.null(table))
      
      idx <- default_model_index()
      
      reactable(
        as.data.frame(table),
        selection = "single",
        onClick = "select",
        defaultSelected = idx,
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
      req(!is.null(table))
      
      selected <- getReactableState("table_modeles_croissance_table", "selected")
      req(!is.null(selected))
      
      table[selected, 1, drop = TRUE]
    })
    
    # UI bouton téléchargement table ----
    output$download_table_modeles_croissance_ui <- renderUI({
      req(is.null(croissance_res()$error))
      download_button_ui(ns("download_table_modeles_croissance"))
    })
    
    # Téléchargement de la table des modèles ----
    render_download_table(
      id = "download_table_modeles_croissance",
      data = reactive(table_modeles_croissance()),
      filename = reactive(build_export_filename("croissance_modeles", filename_suffix()))
    )
    
    # Graphique du modèle sélectionné ----
    plot_selectedmodelcroissance <- reactive({
      res <- croissance_res()
      
      if (!is.null(res$error)) {
        return(NULL)
      }
      
      req(selectedmodelcroissance(), specimen(), table_modeles_croissance())
      
      croissance_plot(
        dfspecimen = specimen(),
        tablemodele = table_modeles_croissance(),
        modele = selectedmodelcroissance()
      )
    })
    
    # Section graphique ----
    output$croissance_plot_section <- renderUI({
      req(is.null(croissance_res()$error))
      
      tagList(
        p(
          "Le graphique suivant illustre la longueur observée des spécimens en fonction de leur âge, ",
          "ainsi que la courbe de croissance modélisée selon le modèle sélectionné. Les points représentent ",
          "les données observées, tandis que la ligne montre la prédiction du modèle."
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
      plot = reactive(plot_selectedmodelcroissance())
    )
    
    # UI bouton téléchargement graphique ----
    output$download_selectedmodelcroissanceplot_ui <- renderUI({
      req(is.null(croissance_res()$error))
      downloadButton(
        outputId = ns("download_selectedmodelcroissanceplot"),
        label = "Téléchargement du graphique"
      )
    })
    
    # Téléchargement du graphique ----
    render_download_plot(
      id = "download_selectedmodelcroissanceplot",
      plot = plot_selectedmodelcroissance,
      filename = "courbe_croissance"
    )
  })
}