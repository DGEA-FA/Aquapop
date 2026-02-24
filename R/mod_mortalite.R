mod_mortalite_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Mortalité",
    
    h3("Test de sur-dispersion du modèle Poisson"),
    p("Ce test évalue si les données de mortalité par âge violent l’hypothèse d’équidispersion du modèle de Poisson."),
    strong("Interprétation :"),
    verbatimTextOutput(ns("dispersion_msg")),
    br(),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot_dispersion_poisson"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_plot_dispersion_poisson"), "Téléchargement du graphique")
    ),
    br(),
    
    h4("Paramètre avancé : recalcul avec un autre âge de départ"),
    p("Vous pouvez forcer un recalcul avec une autre valeur."),
    uiOutput(ns("ui_custom_peak_plus")),
    actionButton(ns("recalculer_mortalite"), "Recalculer avec cet âge de départ"),
    em(textOutput(ns("texte_pp_utilise"))),
    br(), br(),
    
    h3("Table de sélection du modèle de mortalité"),
    p("Le tableau suivant présente les résultats pour l’ensemble des modèles testés."),
    withSpinner(reactableOutput(ns("comparaison_mortalite_table")), type = myspinner),
    download_button_ui(ns("download_comparaison_mortalite_table")),
    textOutput(ns("phrase_mortalite")),
    br(),
    
    h3("Distribution d'âge et modèle de mortalité retenu"),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot_mortalite"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_plot_mortalite"), "Téléchargement du graphique")
    ),
    br(),
    
    h3("Chapman-Robson"),
    p("La mortalité estimée selon le modèle de Chapman-Robson est présentée à titre comparatif seulement."),
    uiOutput(ns("table_chaprobson")),
    download_button_ui(ns("download_chaprob_df"))
  )
}

mod_mortalite_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Âge max et Peak Plus
    mortalite_get_age_max_res <- reactive({
      req(specimen())
      mortalite_get_age_max(data = specimen())
    })
    
    pp <- reactive({
      req(specimen())
      mortalite_get_peak_plus(data = specimen())
    })
    
    output$ui_custom_peak_plus <- renderUI({
      req(pp(), mortalite_get_age_max_res())
      age_min <- pp()
      age_max <- mortalite_get_age_max_res()
      validate(need(age_max > age_min, "Plage d'âge invalide pour le recalcul"))
      
      selectInput(
        inputId = "custom_peak_plus",
        label = "Recalculer avec un autre age de départ (facultatif)",
        choices = age_min:age_max,
        selected = pp()
      )
    })
    
    pp_utilise <- eventReactive(input$recalculer_mortalite, {
      custom_pp <- input$custom_peak_plus
      age_max <- mortalite_get_age_max_res()
      validate(
        need(!is.null(custom_pp), "Aucun âge sélectionné."),
        need(as.numeric(custom_pp) < age_max, "L'âge de départ doit être inférieur à l’âge maximal.")
      )
      as.numeric(custom_pp)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    peak_plus_final <- reactive({
      if (input$recalculer_mortalite == 0) pp() else pp_utilise()
    })
    
    output$texte_pp_utilise <- renderText({
      req(pp(), peak_plus_final())
      if (input$recalculer_mortalite == 0) {
        glue::glue("Analyse effectuée avec la valeur par défaut d'âge de départ : {pp()}")
      } else {
        glue::glue("Analyse effectuée avec la valeur personnalisée d'âge de départ : {peak_plus_final()}")
      } |> as.character()
    })
    
    df_age_corrigee <- reactive({
      req(specimen(), peak_plus_final(), mortalite_get_age_max_res())
      mortalite_prepare_corr(
        data = specimen(),
        age_peak_plus = peak_plus_final(),
        age_max = mortalite_get_age_max_res()
      )
    })
    
    df_age_etendue <- reactive({
      req(df_age_corrigee(), mortalite_get_age_max_res())
      mortalite_prepare_extended(
        df_corrigee = df_age_corrigee(),
        age_max = mortalite_get_age_max_res()
      )
    })
    
    res_test_surdisp <- reactive({
      req(df_age_corrigee())
      mortalite_test_surdispersion_poisson(df_age_corrigee())
    })
    
    output$dispersion_msg <- renderText({
      req(res_test_surdisp())
      res_test_surdisp()$message
    })
    
    render_plot_ggplot("plot_dispersion_poisson", reactive(res_test_surdisp()$plot))
    
    render_download_plot(
      "download_plot_dispersion_poisson",
      reactive(res_test_surdisp()$plot),
      filename = "dispersion_poisson"
    )
    
    mortalite_compare_modele_res <- reactive({
      req(df_age_etendue())
      mortalite_compare_modele(data = df_age_etendue())
    })
    
    table_modeles_mortalite <- reactive({
      req(mortalite_compare_modele_res())
      mortalite_compare_modele_res()$data
    })
    
    best_model_mortalite <- reactive({
      table <- table_modeles_mortalite()
      req(nrow(table) > 0)
      mortalite_select_best_modele(table)
    })
    
    selected_model_mortalite <- reactive({
      selected <- getReactableState("comparaison_mortalite_table", "selected")
      req(!is.null(selected), table_modeles_mortalite())
      table_modeles_mortalite()[selected, "methode", drop = TRUE]
    })
    
    default_model_index_mortalite <- reactive({
      table <- table_modeles_mortalite()
      req(nrow(table) > 0)
      best_model <- mortalite_select_best_modele(table)
      idx <- match(best_model, table$methode)
      validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
      idx
    })
    
    output$comparaison_mortalite_table <- renderReactable({
      table <- table_modeles_mortalite()
      idx <- default_model_index_mortalite()
      reactable(
        table,
        selection = "single",
        onClick = "select",
        defaultSelected = idx,
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        )
      )
    })
    
    output$phrase_mortalite <- renderText({
      req(table_modeles_mortalite(), best_model_mortalite())
      mortalite_phrase_resume(
        data_comparaison = table_modeles_mortalite(),
        modele_nom = best_model_mortalite()
      )
    })
    
    modele_fit_mortalite <- reactive({
      req(df_age_etendue(), selected_model_mortalite())
      mortalite_fit_best_modele(
        data = df_age_etendue(),
        methode = selected_model_mortalite()
      )
    })
    
    plot_selectedmodel_mortalite <- reactive({
      req(specimen(), modele_fit_mortalite(), table_modeles_mortalite())
      mortalite_plot_modele(
        specimen = specimen(),
        modele = modele_fit_mortalite(),
        info_modele = table_modeles_mortalite()
      )
    })
    
    render_plot_ggplot("plot_mortalite", reactive(plot_selectedmodel_mortalite()))
    
    render_download_plot(
      "download_plot_mortalite",
      plot_selectedmodel_mortalite,
      filename = "courbe_mortalite"
    )
    
    res_chaprob <- reactive({
      req(specimen(), peak_plus_final(), mortalite_get_age_max_res())
      mortalite_chaprob(
        specimen = specimen(),
        pp = peak_plus_final(),
        age_max = mortalite_get_age_max_res()
      )
    })
    
    render_table_flextable(
      "table_chaprobson", 
      reactive(res_chaprob()$flextable))
    
    render_download_table(
      "download_chaprob_df",
      data = reactive(res_chaprob()$data),
      filename = reactive(build_export_filename("chapman_robson", filename_suffix()))
    )
    
    render_download_table(
      "download_comparaison_mortalite_table",
      data = reactive(table_modeles_mortalite()),
      filename = reactive(build_export_filename("mortalite_comparaison", filename_suffix()))
    )
  })
}
