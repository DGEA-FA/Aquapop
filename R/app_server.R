#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom DT renderDT
#' @importFrom reactable colDef reactable getReactableState
#' @import shiny 
app_server <- function(input, output, session) {

# Telechargement des donnees ----

# Upload de la feuille Lac du fichier *.xlsx
    data_temp <- eventReactive(input$upload, {
    load_lac(path = input$upload$datapath, namesheet = "Lac", verbose = FALSE)
  })
  
# UI dynamique - typ_pech (pas de selection initiale)
  output$ui_typ_pech <- renderUI({
    req(data_temp())
    radioButtons(
      inputId = "typ_pech",
      label = "Sélectionner le type de pêche normalisée",
      choices = unique(data_temp()$typ_pech),
      selected = character(0)
    )
  })
  
  # Filtrage 1 : selon typ_pech
  df_filtered1 <- reactive({
    req(data_temp(), input$typ_pech)
    filter_by_pen_lac_annee(data = data_temp(), typ_pech = input$typ_pech)
  })
  
  # UI dynamique - no_lac (aucune selection automatique)
  output$ui_no_lac <- renderUI({
    req(df_filtered1())
    selectInput(
      inputId = "no_lac",
      label = "Sélectionner le numéro de lac",
      choices = sort(unique(df_filtered1()$no_lac)),
      selected = NULL
    )
  })
  
  # Filtrage 2 : selon no_lac
  df_filtered2 <- reactive({
    req(df_filtered1(), input$no_lac)
    filter_by_pen_lac_annee(data = df_filtered1(), no_lac = input$no_lac)
  })
  
  # UI dynamique - annee (aucune selection automatique)
  output$ui_annee <- renderUI({
    req(df_filtered2())
    tagList(
      checkboxGroupInput(
        inputId = "annee",
        label = "Sélectionner les années à considérer dans l'inventaire",
        choices = sort(unique(df_filtered2()$annee))),
      p("Si plus d’une année d’inventaire est sélectionnée, les données seront combinées comme si elles ne constituaient qu’un seul inventaire.",
        style = "font-size: 85%; color: #555;")
    )
  })
  
  # Filtrage 3 : selon annee (et creation de data_lac)
  data_lac <- reactive({
    req(df_filtered2(), input$annee)
    filter_by_pen_lac_annee(data = df_filtered2(), annee = input$annee)
  })
  
  nom_lac_reactif <- reactive({
    req(data_lac(), input$no_lac)
    
    data_filtree <- filter(data_lac(), no_lac == input$no_lac)
    nom <- unique(data_filtree$nom_lac)
    
    # Securite maximale
    if (length(nom) == 1 && !is.na(nom) && nzchar(as.character(nom))) {
      return(as.character(nom))
    }
    
    return(NULL)
  })
  
  
  
  # # Affichage dans la console des filtres effectues (facilite debug)
  # observeEvent(data_lac(), {
  #   cat("\n--- Donnees filtrees data_lac() ---\n")
  #   cat("? Type(s) de peche selectionne(s):\n")
  #   print(unique(data_lac()$typ_pech))
  #   cat("? Numero(s) de lac selectionne(s):\n")
  #   print(unique(data_lac()$no_lac))
  #   cat("? Annee(s) dans data_lac():\n")
  #   print(sort(unique(data_lac()$annee)))
  # })
  
  # Identification de l'espece ciblee
  
  info_pen_reactive <- reactive({
    req(input$typ_pech)
    get_info_pen(input$typ_pech)
  })
  
  # Acces aux elements prepares
  binwidth_reactive <- reactive({ info_pen_reactive()$binwidth })
  nomsp_reactive     <- reactive({ info_pen_reactive()$nom_sp })
  sp_pen <- reactive({ info_pen_reactive()$code_sp })
 

  # Telechargement des autres bases de donnees
  analysis_data <- reactive({
    req(input$upload, input$typ_pech, input$no_lac, input$annee)
    
    get_analysis_data(
      path     = input$upload$datapath,
      typ_pech = input$typ_pech,
      no_lac   = input$no_lac,
      annee    = input$annee, verbose = FALSE
    )
  })
  
  # Acces aux jeux prepares
  data_station <- reactive({ analysis_data()$data_station })
  station_valides <- reactive({ analysis_data()$station_valides })
  station_hasard_valide <- reactive({ analysis_data()$station_hasard_valide })
  
  specimen     <- reactive({ analysis_data()$specimen })
  specimen_valid <- reactive({ analysis_data()$specimen_valid })
  capture      <- reactive({ analysis_data()$capture })
  
  # Tableau de synthese introductif
  
  output$recap_intro_table <- renderTable({
    req(data_lac(), data_station())
    generate_recapitulatif_inventaire(data_lac = data_lac(), data_station = data_station())
  })

  #UI dynamique - visualisation des donnees telechargees

  output$visualiser <- renderUI({
    req(data_lac())
    selectInput(
      inputId = "controller",
      label = "Visualiser les données",
      choices = c(
        "Lac" = "data_lac",
        "Stations" = "data_station",
        "Spécimens" = "specimen",
        "Spécimens valides" = "specimen_valid",
        "Capture" = "capture"
        
      ),       selected = NULL,
      multiple = FALSE
    )
  })
  
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller)
  })
 
  output$table_lac      <- renderDT(data_lac(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_station  <- renderDT(data_station(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_specimen  <- renderDT(specimen(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_specimen_valid  <- renderDT(specimen_valid(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_capture <- renderDT(capture(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  
  filename_suffix <- reactive({
    generate_filename_suffix(
      typ_pech = input$typ_pech,
      annee    = input$annee,
      no_lac   = input$no_lac,
      nom_lac  = nom_lac_reactif()
    )
  })
  
  
  
  # CPUE - Abondance ----
  
  ## Tableau CPUE - Tous ----
  cpue_table_tous <- reactive({
    req(specimen(), capture())
    cpue_prepare(capture = capture(), specimen = specimen(), group = "tous")
  })
  
  cpue_modele_tous <- reactive({
    req(cpue_table_tous())
    cpue_compare_modele(cpue_table_tous())
  })
  
  render_table_flextable("cpue_tous_table", reactive(cpue_modele_tous()$flextable))
  
  
  render_download_table(
    "cpue_tous_table_dl",
    data = reactive(cpue_modele_tous()$data),
    filename = reactive(build_export_filename("cpue_tous", filename_suffix()))
  )
  
  ## Tableau CPUE - Femelles matures ----
  cpue_table_femelles <- reactive({
    req(specimen(), capture())
    cpue_prepare(capture = capture(), specimen = specimen(), group = "femelles")
  })
  
  cpue_modele_femelles <- reactive({
    req(cpue_table_femelles())
    cpue_compare_modele(cpue_table_femelles())
  })
  
  render_table_flextable("cpue_femelles_table", reactive(cpue_modele_femelles()$flextable))
  render_download_table(
    "cpue_femelles_table_dl",
    data = reactive(cpue_modele_femelles()$data),
    filename = reactive(build_export_filename("cpue_femelles", filename_suffix()))
  )
  
  ## Tableau d'abondance ----
  
  best_model_tous <- reactive({
    req(cpue_modele_tous())
    cpue_select_best_modele(cpue_modele_tous()$data)
  })
  
  best_model_femelles <- reactive({
    req(cpue_modele_femelles())
    cpue_select_best_modele(cpue_modele_femelles()$data)
  })
  
  abondance1 <- reactive({
    req(
      specimen(),
      cpue_modele_tous(),
      cpue_modele_femelles(),
      best_model_tous(),
      best_model_femelles()
    )
    
    cpue_abondance_table(
      data = specimen(),
      cpue_table_tous = cpue_modele_tous()$data,
      cpue_table_femelles = cpue_modele_femelles()$data,
      best_model_tous = best_model_tous(),
      best_model_femelles = best_model_femelles()
    )
  })
  
  render_table_flextable("abondance_table", reactive(abondance1()$flextable))
  render_download_table(
    "abondance_table_dl",
    data = reactive(abondance1()$data),
    filename = reactive(build_export_filename("abondance", filename_suffix()))
  )
  
  # BPUE - Biomasse ----
  
  biomasse1 <- reactive({
    req(specimen(), station_hasard_valide())
    bpue_generate_biomasse(
      data_specimen = specimen(),
      data_station  = station_hasard_valide()
    )
  })
  
  render_table_flextable("biomasse_table", reactive(biomasse1()$flextable))
  
  
  render_download_table(
    "biomasse_table_dl",
    data = reactive(biomasse1()$data),
    filename = reactive(build_export_filename("biomasse", filename_suffix()))
  ) 
  
  # Taille, masse, age ----
  
  mod_taille_masse_age_server(
    id = "taille_masse_age_1",
    specimen_valid = specimen_valid,
    filename_suffix = filename_suffix
  )
  
  # Structure de taille ----
  
  mod_structure_taille_server("structure_taille_1", specimen = specimen_valid, filename_suffix = filename_suffix)
  
  # Structure d'age ----
  
  mod_structure_age_server("structure_age_1", specimen = specimen_valid, filename_suffix = filename_suffix)
  
  # PSD ----
  mod_psd_server(
    id = "psd_1",
    specimen = specimen_valid,             
    filename_suffix = filename_suffix      
  )
  
  # Relation masse-longueur ----
  
  mod_masse_longueur_server(
    id = "masselongueur_1",
    specimen = specimen,
    filename_suffix = filename_suffix
  )
  
  # Indice de condition ----
  mod_wri_server("wri_1", specimen = specimen_valid, filename_suffix = filename_suffix)

  # Croissance ----
  ## Tableau de selection de modeles ----
  
  
    # Reactif pour la table de modeles
  table_modeles_croissance <- reactive({
    req(specimen()) 
    croissance_compare_modele(data = specimen())$data
  })
  
  # Reactif pour la ligne par defaut (le meilleur modele)
  default_model_index <- reactive({
    table <- table_modeles_croissance()
    req(nrow(table) > 0)
    best_model <- croissance_select_best_modele(table)
    idx <- match(best_model, table$methode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  # Table interactive avec selection par defaut dynamique
  output$table_modeles_croissance_table <- renderReactable({
    table <- table_modeles_croissance()
    idx <- default_model_index()
    
    reactable(
      labelled_data(table),
      selection = "single",
      onClick = "select",
      defaultSelected = idx,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(textAlign = "center")
      )
    )
  })
  
  # Modele actuellement selectionne
  selectedmodelcroissance <- reactive({
    selected <- getReactableState("table_modeles_croissance_table", "selected")
    req(!is.null(selected), table_modeles_croissance())
    table <- table_modeles_croissance()
    table[selected, 1, drop = TRUE]
  })
  
  
  # Bouton de telechargement de la table des modeles
  render_download_table(
    "download_table_modeles_croissance",
    data = reactive(table_modeles_croissance()),
    filename = reactive(build_export_filename("croissance_modeles", filename_suffix()))
  )
  
   ## Graphique du modele choisi ----
  
  # Reactif : graphique du modele selectionne
  plot_selectedmodelcroissance <- reactive({
    req(selectedmodelcroissance(), specimen(), table_modeles_croissance())
    croissance_plot(
      dfspecimen = specimen(),
      tablemodele = table_modeles_croissance(),
      modele = selectedmodelcroissance()
    )
  })
  
  # Affichage du graphique dans Shiny
  render_plot_ggplot("selectedmodelcroissanceplot", reactive(plot_selectedmodelcroissance()))
  
  
  # Bouton de telechargement du graphique
  render_download_plot(
    id = "download_selectedmodelcroissanceplot",
    plot_selectedmodelcroissance,
    filename = "courbe_croissance"
  )
  
  # Mortalite ----
  
  ## Age maximum
  mortalite_get_age_max_res <- reactive({
    req(specimen())
    mortalite_get_age_max(data = specimen())
  })
  
  ## Peak Plus
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
      label = "Recalculer avec un autre Peak Plus (facultatif)",
      choices = age_min:age_max,
      selected = pp()
    )
  })
  
  pp_choisi_par_utilisateur <- reactive({
    input$custom_peak_plus
  })
  
  
  
  pp_utilise <- eventReactive(input$recalculer_mortalite, {
    custom_pp <- input$custom_peak_plus
    age_max <- mortalite_get_age_max_res()
    
    validate(
      need(!is.null(custom_pp), "Aucun âge sélectionné."),
      need(as.numeric(custom_pp) < age_max, "Le Peak Plus doit être inférieur à l’âge maximal.")
    )
    
    as.numeric(custom_pp)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  peak_plus_final <- reactive({
    if (input$recalculer_mortalite == 0) pp() else pp_utilise()
  })
  
  
  output$texte_pp_utilise <- renderText({
    req(pp(), peak_plus_final())
    
    if (input$recalculer_mortalite == 0) {
      glue("Analyse effectuée avec la valeur par défaut du Peak Plus : {pp()}") |> as.character()
    } else {
      glue("Analyse effectuée avec la valeur personnalisée du Peak Plus : {peak_plus_final()}") |> as.character()
    }
  })
  
  
  
  ## Donnees corrigees pour la mortalite
  df_age_corrigee <- reactive({
    req(specimen(), peak_plus_final(), mortalite_get_age_max_res())
    mortalite_prepare_corr(
      data = specimen(),
      age_peak_plus = peak_plus_final(),
      age_max       = mortalite_get_age_max_res()
    )
  })
  
  ## Donnees etendues
  df_age_etendue <- reactive({
    req(df_age_corrigee(), mortalite_get_age_max_res())
    mortalite_prepare_extended(
      df_corrigee = df_age_corrigee(),
      age_max     = mortalite_get_age_max_res()
    )
  })
  
  ## Test de surdispersion (Poisson)
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
  
  ## Tableau de selection de modeles ----
  mortalite_compare_modele_res <- reactive({
    req(df_age_etendue())
    mortalite_compare_modele(data = df_age_etendue())
  })
  
  # Tableau des modeles (data.frame)
  table_modeles_mortalite <- reactive({
    req(mortalite_compare_modele_res())
    mortalite_compare_modele_res()$data
  })
  
  # Meilleur modele automatique (pour phrase descriptive)
  best_model_mortalite <- reactive({
    table <- table_modeles_mortalite()
    req(nrow(table) > 0)
    mortalite_select_best_modele(table)
  })
  
  # Modele selectionne dans la table (utilise pour graphique)
  selected_model_mortalite <- reactive({
    selected <- getReactableState("comparaison_mortalite_table", "selected")
    req(!is.null(selected), table_modeles_mortalite())
    table_modeles_mortalite()[selected, "Méthode", drop = TRUE]
  })
  
  
  # Index du meilleur modele
  default_model_index_mortalite <- reactive({
    table <- table_modeles_mortalite()
    req(nrow(table) > 0)
    best_model <- mortalite_select_best_modele(table)
    idx <- match(best_model, table$Méthode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  # Reactable des modeles
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
  
  
  # Fit du modele selectionne
  modele_fit_mortalite <- reactive({
    req(df_age_etendue(), selected_model_mortalite())
    mortalite_fit_best_modele(
      data = df_age_etendue(),
      methode = selected_model_mortalite()
    )
  })
  
  
  ## Graphique du modele choisi ----
  plot_selectedmodel_mortalite <- reactive({
    req(specimen(), modele_fit_mortalite(), table_modeles_mortalite())
    
    mortalite_plot_modele(
      specimen    = specimen(),
      modele      = modele_fit_mortalite(),
      info_modele = table_modeles_mortalite()
    )
  })
  
  render_plot_ggplot("plot_mortalite", reactive(plot_selectedmodel_mortalite()))
  
  render_download_plot(
    "download_plot_mortalite",
    plot_selectedmodel_mortalite,
    filename = "courbe_mortalite"
  )
  
  ## Chapman-Robson ----
  res_chaprob <- reactive({
    req(specimen(), peak_plus_final(), mortalite_get_age_max_res())
    mortalite_chaprob(
      specimen = specimen(),
      pp       = peak_plus_final(),
      age_max  = mortalite_get_age_max_res()
    )
  })
  
  render_table_flextable("table_chaprob", reactive(res_chaprob()$flextable))
  
  render_download_table(
    "download_chaprob_df",
    data = reactive(res_chaprob()$data),
    filename = reactive(build_export_filename("chapman_robson", filename_suffix()))
  )
  
  # Telechargement tableau de modeles
  render_download_table(
    "download_comparaison_mortalite_table",
    data = reactive(table_modeles_mortalite()),
    filename = reactive(build_export_filename("mortalite_comparaison", filename_suffix()))
  )
  
  
  # Maturite sexuelle ----
  ## Longueur a maturite ----
  
  mod_maturite_l50_server("maturite_l50_1", specimen = specimen_valid, filename_suffix = filename_suffix)
  
  ## Age a maturite ----
  ### Tableau de selection de modeles ----
  
  ### Tableau du modele choisi ----
  ### Graphique du modele choisi ----
  
  
}
