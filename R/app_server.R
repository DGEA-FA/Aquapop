# ════════════════════════════════════════════════════════════════════════
# SERVER – FONCTION PRINCIPALE app_server()
# ════════════════════════════════════════════════════════════════════════

app_server <- function(input, output, session) {

# Téléchargement des données ----

# Upload de la feuille Lac du fichier *.xlsx
    data_temp <- eventReactive(input$upload, {
    load_lac(path = input$upload$datapath, namesheet = "Lac", verbose = FALSE)
  })
  
# UI dynamique – typ_pech (pas de sélection initiale)
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
  
  # UI dynamique – no_lac (aucune sélection automatique)
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
  
  # UI dynamique – année (aucune sélection automatique)
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
  
  # Filtrage 3 : selon année (et création de data_lac)
  data_lac <- reactive({
    req(df_filtered2(), input$annee)
    filter_by_pen_lac_annee(data = df_filtered2(), annee = input$annee)
  })
  
  nom_lac_reactif <- reactive({
    req(data_lac(), input$no_lac)
    
    data_filtrée <- dplyr::filter(data_lac(), no_lac == input$no_lac)
    nom <- unique(data_filtrée$nom_lac)
    
    # Sécurité maximale
    if (length(nom) == 1 && !is.na(nom) && nzchar(as.character(nom))) {
      return(as.character(nom))
    }
    
    return(NULL)
  })
  
  
  
  # # Affichage dans la console des filtres effectués (facilite debug)
  # observeEvent(data_lac(), {
  #   cat("\n--- Données filtrées data_lac() ---\n")
  #   cat("→ Type(s) de pêche sélectionné(s):\n")
  #   print(unique(data_lac()$typ_pech))
  #   cat("→ Numéro(s) de lac sélectionné(s):\n")
  #   print(unique(data_lac()$no_lac))
  #   cat("→ Année(s) dans data_lac():\n")
  #   print(sort(unique(data_lac()$annee)))
  # })
  
  # Identification de l'espèce ciblée
  
  info_pen_reactive <- reactive({
    req(input$typ_pech)
    get_info_pen(input$typ_pech)
  })
  
  # Accès aux elements préparés
  binwidth_reactive <- reactive({ info_pen_reactive()$binwidth })
  nomsp_reactive     <- reactive({ info_pen_reactive()$nom_sp })
  sp_pen <- reactive({ info_pen_reactive()$code_sp })
 

  # Téléchargement des autres bases de données
  analysis_data <- reactive({
    req(input$upload, input$typ_pech, input$no_lac, input$annee)
    
    get_analysis_data(
      path     = input$upload$datapath,
      typ_pech = input$typ_pech,
      no_lac   = input$no_lac,
      annee    = input$annee, verbose = FALSE
    )
  })
  
  # Accès aux jeux préparés
  data_station <- reactive({ analysis_data()$data_station })
  station_valides <- reactive({ analysis_data()$station_valides })
  station_hasard_valide <- reactive({ analysis_data()$station_hasard_valide })
  
  specimen     <- reactive({ analysis_data()$specimen })
  specimen_valid <- reactive({ analysis_data()$specimen_valid })
  capture      <- reactive({ analysis_data()$capture })
  
  # Tableau de synthèse introductif
  
  output$recap_intro_table <- renderTable({
    req(data_lac(), data_station())
    generate_recapitulatif_inventaire(data_lac = data_lac(), data_station = data_station())
  })

  #UI dynamique – visualisation des données téléchargées

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
 
  output$table_lac      <- renderDataTable(data_lac(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_station  <- renderDataTable(data_station(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_specimen  <- renderDataTable(specimen(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_specimen_valid  <- renderDataTable(specimen_valid(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  output$table_capture <- renderDataTable(capture(), options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  
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
  
  ## Tableau d’abondance ----
  
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
  
  # Taille, masse, âge ----
  
  # Résultat combiné (data + flextable)
  taille_masse_age_res <- reactive({
    req(specimen_valid())
    taille_masse_age(specimen_valid())
  })
  
  # Affichage du tableau flextable
  render_table_flextable("taillemasseage_table", reactive(taille_masse_age_res()$flextable))
  
  # Bouton de téléchargement des données brutes
  render_download_table(
    "taillemasseage_table_dl",
    data = reactive(taille_masse_age_res()$data),
    filename = reactive(build_export_filename("taillemasseage", filename_suffix()))
  )
  
  # Structure de taille ----
  
  # Résultat combiné : graphique + données
  res_structure_taille <- reactive({
    req(specimen_valid(), input$groupetailleplot)
    structure_taille(
      data = specimen_valid(),
      groupement = input$groupetailleplot
    )
  })
  
  # Rendu du graphique dans l'interface
  render_plot_ggplot("structuretailleplot", reactive(res_structure_taille()$plot))
  
  # Téléchargement du graphique (PNG)
  render_download_plot("download_groupetailleplot", reactive(res_structure_taille()$plot), filename_suffix = filename_suffix())
  
  render_download_table(
    "download_data4plot_taille",
    data = reactive(res_structure_taille()$data),
    filename = reactive(build_export_filename("structure_taille", filename_suffix()))
  )
  # Structure d'âge ----
  
  # Résultat combiné : graphique + tableau
  res_structure_age <- reactive({
    req(specimen_valid(), input$groupeageplot)
    structure_age(
      data = specimen_valid(),
      groupement = input$groupeageplot
    )
  })
  
  # Affichage du graphique
  
  render_plot_ggplot(
    "structureageplot",
    reactive(res_structure_age()$plot), 
    message_si_vide = "Aucun graphique n’a pu être généré : données d’âge manquantes ou inexploitables."
  )

  
  
  # Téléchargement PNG
  render_download_plot("download_groupeageplot", reactive(res_structure_age()$plot), filename_suffix = filename_suffix())
  # Téléchargement des données
  render_download_table(
    "download_data4plot_age",
    data = reactive(res_structure_age()$data),
    filename = reactive(build_export_filename("structure_age", filename_suffix()))
  )
  
  # PSD ----
  
  ## Indice Q ----
  
  psd_q_res <- reactive({
    req(specimen_valid())
    psd_q(data = specimen_valid())
  })
  
  # Rendu du tableau flextable dans l'interface
  render_table_flextable("psd_indice_ui", reactive(psd_q_res()$flextable))
  
  
  ## Répartition par classe de taille – Tableau ----
  
  # Reactive : retourne la liste {data, flextable, plot}
  psd_byclass_res <- reactive({
    req(specimen_valid())
    psd_byclass(data = specimen_valid())
  })
  
  # Rendu du tableau flextable dans l'interface
  render_table_flextable("psd_byclass_table", reactive(psd_byclass_res()$flextable))
  
  # Bouton de téléchargement du tableau brut
  render_download_table(
    "psd_byclass_table_dl",
    data = reactive(psd_byclass_res()$data),
    filename = reactive(build_export_filename("psd_byclass", filename_suffix()))
  )  
  ## Répartition par classe de taille – Graphique ----
  
  # Affichage du graphique dans l'interface
  render_plot_ggplot("psd_byclass_plot", reactive(psd_byclass_res()$plot))
  
  # Bouton de téléchargement du graphique (PNG)
  render_download_plot("download_psd_byclass_plot", reactive(psd_byclass_res()$plot), filename_suffix = filename_suffix())
  
  # Relation masse-longueur ----
  
  # Reactive : retourne la liste {data, flextable, plot}
  masse_longueur_fit_res <- reactive({
    req(specimen())
    masse_longueur_fit(data = specimen())
  })
  
  ## Graphique ----
  
  render_plot_ggplot("plot_masselongueur", reactive(masse_longueur_fit_res()$plot))
  
  
  render_download_plot("download_masselongueur_plot", reactive(masse_longueur_fit_res()$plot), filename_suffix = filename_suffix())
  
  
  ## Tableau des coefficients ----
  

  render_table_flextable("table_masselongueur_ui", reactive(masse_longueur_fit_res()$flextable))
  
  render_download_table(
    "download_masselongueur_table",
    data = reactive(masse_longueur_fit_res()$data),
    filename = reactive(build_export_filename("masselongueur", filename_suffix()))
  ) 
  
  
  # Indice de condition ----
  
  # Reactive : retourne la liste {data, flextable, plot_tous, plot_byclass}
  wri_res <- reactive({
    req(specimen_valid())
    wri(data = specimen_valid())
  })
  
  ## Tableau Wr ----
  
  render_table_flextable("wri_table", reactive(wri_res()$flextable))
  render_download_table(
    "wri_table_dl",
    data = reactive(wri_res()$data),
    filename = reactive(build_export_filename("wri", filename_suffix()))
  )  
  ## Graphique Wr par sexe ----
  render_plot_ggplot("wri_plot_tous", reactive(wri_res()$plot_tous))
  
  render_download_plot("download_wri_plot_tous", reactive(wri_res()$plot_tous), filename_suffix = filename_suffix())
  
  ## Graphique Wr par classe de taille ----
  render_plot_ggplot("wri_plot_byclass", reactive(wri_res()$plot_byclass))
  render_download_plot("download_wri_plot_byclass", reactive(wri_res()$plot_byclass), filename_suffix = filename_suffix())
  

  # Croissance ----
  ## Tableau de sélection de modèles ----
  
  
    # Réactif pour la table de modèles
  table_modeles_croissance <- reactive({
    req(specimen()) 
    croissance_compare_modele(data = specimen())$data
  })
  
  # Réactif pour la ligne par défaut (le meilleur modèle)
  default_model_index <- reactive({
    table <- table_modeles_croissance()
    req(nrow(table) > 0)
    best_model <- croissance_select_best_modele(table)
    idx <- match(best_model, table$methode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  # Table interactive avec sélection par défaut dynamique
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
  
  # Modèle actuellement sélectionné
  selectedmodelcroissance <- reactive({
    selected <- getReactableState("table_modeles_croissance_table", "selected")
    req(!is.null(selected), table_modeles_croissance())
    table <- table_modeles_croissance()
    table[selected, 1, drop = TRUE]
  })
  
  
  # Bouton de téléchargement de la table des modèles
  render_download_table(
    "download_table_modeles_croissance",
    data = reactive(table_modeles_croissance()),
    filename = reactive(build_export_filename("croissance_modeles", filename_suffix()))
  )
  
   ## Graphique du modèle choisi ----
  
  # Réactif : graphique du modèle sélectionné
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
  
  
  # Bouton de téléchargement du graphique
  render_download_plot(
    id = "download_selectedmodelcroissanceplot",
    plot_selectedmodelcroissance,
    filename = "courbe_croissance"
  )
  
  # Mortalité ----
  
  ## Âge maximum
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
      glue::glue("Analyse effectuée avec la valeur par défaut du Peak Plus : {pp()}") |> as.character()
    } else {
      glue::glue("Analyse effectuée avec la valeur personnalisée du Peak Plus : {peak_plus_final()}") |> as.character()
    }
  })
  
  
  
  ## Données corrigées pour la mortalité
  df_age_corrigee <- reactive({
    req(specimen(), peak_plus_final(), mortalite_get_age_max_res())
    mortalite_prepare_corr(
      data = specimen(),
      age_peak_plus = peak_plus_final(),
      age_max       = mortalite_get_age_max_res()
    )
  })
  
  ## Données étendues
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
  
  ## Tableau de sélection de modèles ----
  mortalite_compare_modele_res <- reactive({
    req(df_age_etendue())
    mortalite_compare_modele(data = df_age_etendue())
  })
  
  # Tableau des modèles (data.frame)
  table_modeles_mortalite <- reactive({
    req(mortalite_compare_modele_res())
    mortalite_compare_modele_res()$data
  })
  
  # Meilleur modèle automatique (pour phrase descriptive)
  best_model_mortalite <- reactive({
    table <- table_modeles_mortalite()
    req(nrow(table) > 0)
    mortalite_select_best_modele(table)
  })
  
  # Modèle sélectionné dans la table (utilisé pour graphique)
  selected_model_mortalite <- reactive({
    selected <- getReactableState("comparaison_mortalite_table", "selected")
    req(!is.null(selected), table_modeles_mortalite())
    table_modeles_mortalite()[selected, "Méthode", drop = TRUE]
  })
  
  
  # Index du meilleur modèle
  default_model_index_mortalite <- reactive({
    table <- table_modeles_mortalite()
    req(nrow(table) > 0)
    best_model <- mortalite_select_best_modele(table)
    idx <- match(best_model, table$Méthode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  # Reactable des modèles
  output$comparaison_mortalite_table <- renderReactable({
    table <- table_modeles_mortalite()
    idx <- default_model_index_mortalite()
    
    reactable::reactable(
      table,
      selection = "single",
      onClick = "select",
      defaultSelected = idx,
      defaultColDef = reactable::colDef(
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
  
  
  # Fit du modèle sélectionné
  modele_fit_mortalite <- reactive({
    req(df_age_etendue(), selected_model_mortalite())
    mortalite_fit_best_modele(
      data = df_age_etendue(),
      methode = selected_model_mortalite()
    )
  })
  
  
  ## Graphique du modèle choisi ----
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
  
  # Téléchargement tableau de modèles
  render_download_table(
    "download_comparaison_mortalite_table",
    data = reactive(table_modeles_mortalite()),
    filename = reactive(build_export_filename("mortalite_comparaison", filename_suffix()))
  )
  
  
  # Maturité sexuelle ----
  ## Longueur à maturité ----
  
  ### Tableau de sélection de modèles ----
  
  # Résultat complet : modèles et tables
  table_modeles_l50_resultats <- reactive({
    req(specimen())
    maturite_compare_modele(
      specimen_data = specimen(),
      prefer_combined = FALSE,
      variable = "ltm"
    )
  })
  
  # Index du meilleur modèle pour sélection par défaut
  default_model_index_l50 <- reactive({
    table <- table_modeles_l50_resultats()$table$df
    req(nrow(table) > 0)
    idx <- which(table$recommande)
    if (length(idx) == 0) idx <- 1
    idx
  })
  
  # Tableau interactif des modèles
  output$table_modeles_l50_table <- renderReactable({
    req(table_modeles_l50_resultats())
    table <- table_modeles_l50_resultats()$table$df
    idx <- default_model_index_l50()
    
    reactable(
      labelled_data(table),
      selection = "single",
      sortable = FALSE,
      onClick = "select",
      highlight = TRUE,
      defaultPageSize = 20,
      defaultSelected = idx,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(textAlign = "center")
      )
    )
  })
  
  # Modèle actuellement sélectionné
  selected_model_info_l50 <- reactive({
    selected <- getReactableState("table_modeles_l50_table", "selected")
    req(!is.null(selected), table_modeles_l50_resultats())
    table <- table_modeles_l50_resultats()$table$df
    model_id <- table[selected, "modele_id", drop = TRUE]
    
    list(
      modele = stringr::str_extract(model_id, "TLO|ADD|INT|COM"),
      lien = stringr::str_extract(model_id, "logit|probit|cloglog"),
      variable = "ltm"
    )
  })
  
  
  # Message explicatif sur les modèles évalués
  output$message_l50 <- renderText({
    req(table_modeles_l50_resultats())
    table_modeles_l50_resultats()$message
  })
  
  
  # Résultat du modèle sélectionné
  l50_generate_modele_res <- reactive({
    req(specimen(), selected_model_info_l50())
    maturite_generate_modele(
      data = specimen(),
      variable = selected_model_info_l50()$variable,
      modele = selected_model_info_l50()$modele,
      lien = selected_model_info_l50()$lien
    )
  })
  
  
  ### Tableau du modèle choisi ----
  render_table_flextable("ogive_l50_table", reactive(l50_generate_modele_res()$table_resultats_flextable))
  render_download_table(
    "ogive_l50_table_dl",
    data = reactive(l50_generate_modele_res()$table_resultats),
    filename = reactive(build_export_filename("ogive_maturite", filename_suffix()))
  )
  
  ### Graphique du modèle choisi ----
  render_plot_ggplot("plot_ogive_l50", reactive(l50_generate_modele_res()$graphique))
  render_download_plot("download_ogive_l50_plot", reactive(l50_generate_modele_res()$graphique), filename_suffix = filename_suffix())
  
   
  ## Âge à maturité ----
  ### Tableau de sélection de modèles ----
  
  ### Tableau du modèle choisi ----
  ### Graphique du modèle choisi ----
  
  
}