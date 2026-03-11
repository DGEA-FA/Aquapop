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
    
    # Texte explicatif
    p("Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc. 
       Prenez note également que le modèle de von Bertalanffy utilise la méthode pondérée 
       avec t₀ variable. Attention : les IC95 % des prédictions ne peuvent pas être calculés 
       à partir des IC95 % des estimations des paramètres L, K et t₀."),
    
    h3("Table de sélection du modèle de croissance"),
    withSpinner(reactableOutput(ns("table_modeles_croissance_table")), type = myspinner),
    download_button_ui(ns("download_table_modeles_croissance")),
    
    br(),
    
    # Graphique
    p("Le graphique suivant illustre la longueur observée des spécimens en fonction de leur âge, 
       ainsi que la courbe de croissance modélisée selon le modèle sélectionné. Les points représentent 
       les données observées, tandis que la ligne montre la prédiction du modèle."),
    
    h3("Longueur à l’âge des spécimens capturés et modèle de croissance"),
    
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("selectedmodelcroissanceplot"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_selectedmodelcroissanceplot"), "Téléchargement du graphique")
    )
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
    
    # Table des modèles de croissance
    table_modeles_croissance <- reactive({
      req(specimen()) 
      croissance_compare_modele(data = specimen())$data
    })
    
    # Index du meilleur modèle (pour sélection par défaut)
    default_model_index <- reactive({
      table <- table_modeles_croissance()
      req(nrow(table) > 0)
      best_model <- croissance_select_best_modele(table)
      idx <- match(best_model, table$methode)
      validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats."))
      idx
    })
    
    # Tableau interactif
    output$table_modeles_croissance_table <- renderReactable({
      table <- table_modeles_croissance()
      idx <- default_model_index()
      
      reactable(
        as.data.frame(table),  # sécurise contre les erreurs de format
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
    
    # Modèle sélectionné
    selectedmodelcroissance <- reactive({
      selected <- getReactableState("table_modeles_croissance_table", "selected")
      req(!is.null(selected), table_modeles_croissance())
      table <- table_modeles_croissance()
      table[selected, 1, drop = TRUE]
    })
    
    # Téléchargement de la table des modèles
    render_download_table(
      "download_table_modeles_croissance",
      data = reactive(table_modeles_croissance()),
      filename = reactive(build_export_filename("croissance_modeles", filename_suffix()))
    )
    
    # Graphique du modèle sélectionné
    plot_selectedmodelcroissance <- reactive({
      req(selectedmodelcroissance(), specimen(), table_modeles_croissance())
      croissance_plot(
        dfspecimen = specimen(),
        tablemodele = table_modeles_croissance(),
        modele = selectedmodelcroissance()
      )
    })
    
    # Affichage du graphique
    render_plot_ggplot("selectedmodelcroissanceplot", reactive(plot_selectedmodelcroissance()))
    
    # Téléchargement du graphique
    render_download_plot(
      id = "download_selectedmodelcroissanceplot",
      plot = plot_selectedmodelcroissance,
      filename = "courbe_croissance"
    )
  })
}

