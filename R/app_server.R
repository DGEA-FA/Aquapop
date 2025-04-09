app_server <- function(input, output, session) {

# Téléchargement data_lac -------------------------------------------------

# Upload de la feuille Lac du fichier *.xlsx
    data_temp <- eventReactive(input$upload, {
    load_lac(path = input$upload$datapath, namesheet = "Lac")
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
    filtrer_par_pen_lac_annee(data = data_temp(), typ_pech = input$typ_pech)
  })
  
  # UI dynamique – no_lac (aucune sélection automatique)
  output$ui_no_lac <- renderUI({
    req(df_filtered1())
    selectInput(
      inputId = "no_lac",
      label = "Sélectionner le numéro de lac",
      choices = unique(df_filtered1()$no_lac),
      selected = NULL
    )
  })
  
  # Filtrage 2 : selon no_lac
  df_filtered2 <- reactive({
    req(df_filtered1(), input$no_lac)
    filtrer_par_pen_lac_annee(data = df_filtered1(), no_lac = input$no_lac)
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
    filtrer_par_pen_lac_annee(data = df_filtered2(), annee = input$annee)
  })
  
  # Affichage dans la console des filtres effectués (facilite debug)
  observeEvent(data_lac(), {
    cat("\n--- Données filtrées data_lac() ---\n")
    cat("→ Type(s) de pêche sélectionné(s):\n")
    print(unique(data_lac()$typ_pech))
    cat("→ Numéro(s) de lac sélectionné(s):\n")
    print(unique(data_lac()$no_lac))
    cat("→ Année(s) dans data_lac():\n")
    print(sort(unique(data_lac()$annee)))
  })
  
# Identification de l'espèce ciblée ---------------------------------------
  
  info_pen_reactive <- reactive({
    req(input$typ_pech)
    get_info_pen(input$typ_pech)
  })
  
  # Accès aux elements préparés
  binwidth_reactive <- reactive({ info_pen_reactive()$binwidth })
  nomsp_reactive     <- reactive({ info_pen_reactive()$nom_sp })
  sp_pen <- reactive({ info_pen_reactive()$code_sp })
 

# Téléchargement des autres bases de données ------------------------------

  analysis_data <- reactive({
    req(input$upload, input$typ_pech, input$no_lac, input$annee)
    
    get_analysis_data(
      path     = input$upload$datapath,
      typ_pech = input$typ_pech,
      no_lac   = input$no_lac,
      annee    = input$annee
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
    table_recap(data_lac = data_lac(), data_station = data_station())
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
  
  # Taille masse age -------------------------------------------------------
  # 1. Données brutes
  df_taillemasseage <- reactive({
    req(specimen_valid())
    taille_masse_age(specimen_valid(), format = "data.frame")
  })
  
  # 2. Tableau mis en forme
  ft_taillemasseage <- reactive({
    req(specimen_valid())
    taille_masse_age(specimen_valid(), format = "flextable")
  })
  
  # 3. Affichage du tableau
  render_table_flextable("taillemasseage_ui", ft_taillemasseage)
  
  # 4. Bouton de téléchargement
  render_download_table("dl_taillemasseage", df_taillemasseage())
  

# Structure de taille -----------------------------------------------------
  # Reactive : retourne le ggplot
  plot_structure_taille <- reactive({
    req(specimen_valid(), input$groupetailleplot)
    structure_taille(
      data = specimen_valid(),
      groupement = input$groupetailleplot,
      format = "plot"
    )
  })
  
  # Rendu du graphique dans l'interface
  render_plot_ggplot("structuretailleplot", plot_structure_taille)
  
  # Bouton de téléchargement du graphique (PNG)
  render_download_plot("download_groupetailleplot", plot_structure_taille)
  
  # Reactive : données du graphique (data.frame)
  df_structure_taille <- reactive({
    req(specimen_valid(), input$groupetailleplot)
    structure_taille(
      data = specimen_valid(),
      groupement = input$groupetailleplot,
      format = "data.frame"
    )
  })
  
  # Téléchargement des données
  render_download_table("download_data4plot_taille", df_structure_taille())
  
  
  # Structure d'âge -----------------------------------------------------
  # Reactive : retourne le ggplot
  plot_structure_age <- reactive({
    req(specimen_valid(), input$groupeageplot)
    structure_age(
      data = specimen_valid(),
      groupement = input$groupeageplot,
      format = "plot"
    )
  })
  
  # Rendu du graphique dans l'interface
  render_plot_ggplot("structureageplot", plot_structure_age)
  
  # Bouton de téléchargement du graphique (PNG)
  render_download_plot("download_groupeageplot", plot_structure_age)
  
  # Reactive : données du graphique (data.frame)
  df_structure_age <- reactive({
    req(specimen_valid(), input$groupeageplot)
    structure_age(
      data = specimen_valid(),
      groupement = input$groupeageplot,
      format = "data.frame"
    )
  })
  
  # Téléchargement des données
  render_download_table("download_data4plot_age", df_structure_age())
  
  # === PSD  ===================================
  
  ## --- Indice PSD global : Tableau -----------------------------------------
  
  # Reactive : retourne un tableau flextable avec l'indice PSD et IC95
  ft_psd_indice <- reactive({
    req(specimen_valid())
    psd_indice(data = specimen_valid(), format = "flextable")
  })
  
  # Rendu du tableau dans l'interface
  render_table_flextable("psd_indice_ui", ft_psd_indice)
  
  
  ## --- Répartition par classe de taille : Tableau --------------------------
  
  # Reactive (brut) : pour téléchargement
  df_psd_byclass <- reactive({
    req(specimen_valid())
    psd_byclass(data = specimen_valid(), format = "data.frame")
  })
  
  # Reactive (flextable) : pour affichage
  ft_psd_byclass <- reactive({
    req(specimen_valid())
    psd_byclass(data = specimen_valid(), format = "flextable")
  })
  
  # Rendu du tableau formaté dans l'interface
  render_table_flextable("psd_byclass_ui", ft_psd_byclass)
  
  # Bouton de téléchargement du tableau brut
  render_download_table("dl_psd_byclass", df_psd_byclass())
  
  
  ## --- Histogramme par classe de taille : Graphique ------------------------
  
  # Reactive : retourne un objet ggplot du graphique PSD
  plot_psd <- reactive({
    req(specimen_valid())
    psd_byclass(data = specimen_valid(), format = "plot")
  })
  
  # Affichage du graphique dans l'interface
  render_plot_ggplot("psd_byclass_plot", plot_psd)
  
  # Bouton de téléchargement du graphique (PNG)
  render_download_plot("download_psd_byclass_plot", plot_psd)
  
  # Relation masse-longueur -------------------------------------------------------
  
  # --- Graphique ---
  plot_masselongueur <- reactive({
    req(specimen())
    relation_masse_longueur(data = specimen(), format = "plot")
  })
  
  render_plot_ggplot("plot_masselongueur", plot_masselongueur)
  render_download_plot("download_masselongueur_plot", plot_masselongueur)
  
  # --- Tableau des coefficients ---
  table_masselongueur <- reactive({
    req(specimen())
    relation_masse_longueur(data = specimen(), format = "data.frame")
  })
  
  ft_masselongueur <- reactive({
    req(specimen())
    relation_masse_longueur(data = specimen(), format = "flextable")
  })
  
  render_table_flextable("table_masselongueur_ui", ft_masselongueur)
  render_download_table("download_masselongueur_table", table_masselongueur())
  
  # Indice de condition -------------------------------------------------------
  
  # Tableau Wr 
  ft_wri <- reactive({
    req(specimen_valid())
    indice_condition(data = specimen_valid(), format = "flextable")
  })
  
  # Affichage du tableau flextable
  render_table_flextable("wri_table_ui", ft_wri)
  
  df_wri <- reactive({
    req(specimen_valid())
    indice_condition(data = specimen_valid(), format = "data.frame")
  })
  
  # Téléchargement du tableau
  render_download_table("download_wri_table", df_wri())
  
  # Graphique Wr par sexe
  plot_wri_tous <- reactive({
    req(specimen_valid())
    indice_condition(data = specimen_valid(), format = "plot_tous")
  })
  render_plot_ggplot("wri_plot_tous", plot_wri_tous)
  render_download_plot("download_wri_plot_tous", plot_wri_tous)
  
  # Graphique Wr par classe de taille
  plot_wri_byclass <- reactive({
    req(specimen_valid())
    indice_condition(data = specimen_valid(), format = "plot_byclass")
  })
  render_plot_ggplot("wri_plot_byclass", plot_wri_byclass)
  render_download_plot("download_wri_plot_byclass", plot_wri_byclass)
  # Abondance CPUE ---------------------------------------------------------------
  
  ## Tous les spécimens ------------------------------------------------
  cpue_table_tous <- reactive({
    req(specimen(), capture())
    prepare_cpue_data(capture = capture(), specimen = specimen(), group = "tous")
  })
  
  cpue_modele_tous <- reactive({
    req(cpue_table_tous())
    cpue_modele_comparaison(cpue_table_tous(), format = "data.frame")
  })
  
  cpue_modele_tous_flextable <- reactive({
    req(cpue_table_tous())
    cpue_modele_comparaison(cpue_table_tous(), format = "flextable")
  })
  
  render_table_flextable("cpue_tous_table", cpue_modele_tous_flextable)
  render_download_table(id = "cpue_tous_dl", data_reactive = cpue_modele_tous())
  
  ## Femelles matures --------------------------------------------------
  cpue_table_femelles <- reactive({
    req(specimen(), capture())
    prepare_cpue_data(capture = capture(), specimen = specimen(), group = "femelles")
  })
  
  cpue_modele_femelles <- reactive({
    req(cpue_table_femelles())
    cpue_modele_comparaison(cpue_table_femelles(), format = "data.frame")
  })
  
  cpue_modele_femelles_flextable <- reactive({
    req(cpue_table_femelles())
    cpue_modele_comparaison(cpue_table_femelles(), format = "flextable")
  })
  
  render_table_flextable("cpue_femelles_table", cpue_modele_femelles_flextable)
  render_download_table(id = "cpue_femelles_dl", data_reactive = cpue_modele_femelles())
  
  ## Tableau d’abondance ------------------------------------------------
  # Meilleur modèle pour CPUE "tous" et "femelles"
  best_model_tous <- reactive({
    req(cpue_modele_tous())
    select_best_cpue_model(cpue_modele_tous())
  })
  
  best_model_femelles <- reactive({
    req(cpue_modele_femelles())
    select_best_cpue_model(cpue_modele_femelles())
  })
  
  # Table d’abondance (data.frame)
  abondance1 <- reactive({
    req(
      specimen(),
      cpue_modele_tous(),
      cpue_modele_femelles(),
      best_model_tous(),
      best_model_femelles()
    )
    
    abondance_table(
      data = specimen(),
      cpue_table_tous = cpue_modele_tous(),
      cpue_table_femelles = cpue_modele_femelles(),
      best_model_tous = best_model_tous(),
      best_model_femelles = best_model_femelles(),
      format = "data.frame"
    )
  })
  
  # Table d’abondance (flextable)
  abondance1_flextable <- reactive({
    req(
      specimen(),
      cpue_modele_tous(),
      cpue_modele_femelles(),
      best_model_tous(),
      best_model_femelles()
    )
    
    abondance_table(
      data = specimen(),
      cpue_table_tous = cpue_modele_tous(),
      cpue_table_femelles = cpue_modele_femelles(),
      best_model_tous = best_model_tous(),
      best_model_femelles = best_model_femelles(),
      format = "flextable"
    )
  })
  
  # Affichage UI
  render_table_flextable("abondance1_table", abondance1_flextable)
  
  # Téléchargement
  render_download_table(id = "abondance1_dl", data_reactive = abondance1())
  
  # Biomasse - BPUE -------------------------------------
  biomasse1 <- reactive({
    req(specimen(), station_hasard_valide())
    biomasse_table(
      data_specimen     = specimen(),
      data_station = station_hasard_valide(),
      format       = "flextable"
    )
  })
  
    # Affichage flextable
  render_table_flextable("biomasse1table", biomasse1) #ICI
  
  df_biomasse <- reactive({
    req(specimen(), station_hasard_valide())
    biomasse_table(
      data_specimen     = specimen(),
      data_station = station_hasard_valide(),
      format       = "data.frame"
    )
  })
  

  
  # Téléchargement en .xlsx
  render_download_table(id = "download_biomasse1", data_reactive = df_biomasse())
  
  # Croissance ------------------------------------------------
  # 1. Réactif pour la table de modèles
  table_modeles_croissance <- reactive({
    req(specimen()) 
    courbe_croissance_comparaison(data = specimen(), format = "data.frame")
  })
  
  
  
  # 2. Réactif pour la ligne par défaut (le meilleur modèle)
  default_model_index <- reactive({
    table <- table_modeles_croissance()
    req(nrow(table) > 0)
    best_model <- select_best_croissance_model(table)
    idx <- match(best_model, table$methode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  # 3. Table interactive avec sélection par défaut dynamique
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
  
  # 4. Modèle actuellement sélectionné
  selectedmodelcroissance <- reactive({
    selected <- getReactableState("table_modeles_croissance_table", "selected")
    req(!is.null(selected), table_modeles_croissance())
    table <- table_modeles_croissance()
    table[selected, 1, drop = TRUE]
  })
  
  
  # 8. Bouton de téléchargement de la table des modèles

  
  render_download_table(id = "download_table_modeles_croissance", data_reactive = table_modeles_croissance())
  
  
  # 5. Réactif : graphique du modèle sélectionné
  plot_selectedmodelcroissance <- reactive({
    req(selectedmodelcroissance(), specimen(), table_modeles_croissance())
    courbe_croissance_plot(
      dfspecimen = specimen(),
      tablemodele = table_modeles_croissance(),
      modele = selectedmodelcroissance()
    )
  })
  
  # 6. Affichage du graphique dans Shiny
  render_plot_ggplot(
    output_id = "selectedmodelcroissanceplot",
    plot_reactive = plot_selectedmodelcroissance,
    width = 600, height = 400, res = 96
  )
  
  # 7. Bouton de téléchargement du graphique
  render_download_plot(
    id = "download_selectedmodelcroissanceplot",
    plot_reactive = plot_selectedmodelcroissance,
    filename = "courbe_croissance",  # ou un reactive si tu veux le personnaliser
    width = 7, height = 5, dpi = 300,
    label = "Télécharger le graphique"
  )
  
  
  # Mortalite -------------------------------------------------------
  # Valeur âge maximum
  age_max <- reactive({
    req(specimen())
    get_age_max(specimen())
  })
  
  
  # 1. Valeur PP (Peak Plus)
  pp <- reactive({
    req(specimen())
    get_peak_plus(specimen())
  })
  
  # 3. Données corrigées pour la mortalité
  df_age_corrigee <- reactive({
    req(specimen(), pp(), age_max())
    prepare_age_data_corrigee(specimen(), pp(), age_max())
  })
  
  # 4. Données étendues
  df_age_etendue <- reactive({
    req(df_age_corrigee(), age_max())
    prepare_age_data_etendue(df_corrigee = df_age_corrigee(), age_max = age_max())
  })
  
  # 1. Réactif : test de sur-dispersion Poisson
  res_test_surdisp <- reactive({
    req(df_age_corrigee())  # doit être disponible
    test_surdispersion_poisson(df_age_corrigee())
  })
  
  # 2. Message textuel (interprétation du test)
  output$dispersion_msg <- renderText({
    req(res_test_surdisp())
    res_test_surdisp()$message
  })
  
  # 3. Graphique résidus vs ajustés
  render_plot_ggplot(
    output_id = "plot_dispersion_poisson",
    plot_reactive = reactive(res_test_surdisp()$plot),
    width = 600, height = 400, res = 96
  )
  
  # 4. Téléchargement du graphique
  render_download_plot(
    id = "download_plot_dispersion_poisson",
    plot_reactive = reactive(res_test_surdisp()$plot),
    filename = "dispersion_poisson",
    width = 7, height = 5, dpi = 300,
    label = "Télécharger le graphique"
  )
  # 5. Comparaison des modèles (une seule fois)
  mortalite_compare_modele_res <- reactive({
    req(df_age_etendue())
    mortalite_compare_modele(data = df_age_etendue())
  })
  
  # Affichage dans l'UI
  render_table_flextable("comparaison_mortalite_ui", reactive(mortalite_compare_modele_res()$flextable))
  
  # Téléchargement du tableau brut
  render_download_table("download_comparaison_mortalite_table", mortalite_compare_modele_res()$data)
  
  # 6. Sélection du meilleur modèle
  meilleur_modele <- reactive({
    req(mortalite_compare_modele_res())
    select_best_mortalite_model(mortalite_compare_modele_res()$data)
  })
  
  phrase_mortalite <- reactive({
    req(mortalite_compare_modele_res(), meilleur_modele())
    
    ligne <- mortalite_compare_modele_res()$data |>
      dplyr::filter(Méthode == meilleur_modele())
    
    modele_nom <- meilleur_modele() |> toupper()
    mortalite_A <- ligne$A
    
    glue::glue("Le modèle {modele_nom} décrit le mieux la mortalité de la population. La mortalité annuelle s’élève à {mortalite_A} %.") |> as.character()
  })
  
  output$phrase_mortalite <- renderText({
    phrase_mortalite()
  })
  
  # 7. Graphe du modèle retenu
  plot_mortalite <- reactive({
    req(specimen(), df_age_etendue(), mortalite_compare_modele_res(), meilleur_modele())
    
    modele <- get_best_mortalite_model(
      df_age_etendue(),
      methode = meilleur_modele()
    )
    
    plot_mortalite_modele(
      specimen = specimen(),
      modele = modele,
      info_modele = mortalite_compare_modele_res()$data
    )
  })
  
  
  render_plot_ggplot(
    output_id = "plot_mortalite",
    plot_reactive = plot_mortalite,
    width = 600, height = 400, res = 96
  )
  
  render_download_plot(
    id = "download_plot_mortalite",
    plot_reactive = plot_mortalite,
    filename = "courbe_mortalite",
    width = 7, height = 5, dpi = 300
    
  )
   # 8. Résultats Chapman-Robson (format flextable ou data.frame)
  ft_chaprob <- reactive({
    req(specimen(), pp(), age_max())
    mortalite_chaprob(specimen = specimen(), pp = pp(), age_max = age_max(), format = "flextable")
  })
  
  df_chaprob <- reactive({
    req(specimen(), pp(), age_max())
    mortalite_chaprob(specimen = specimen(), pp = pp(), age_max = age_max(), format = "data.frame")
  })
  
  render_table_flextable("table_chaprob", ft_chaprob)
  
  render_download_table(
    id = "download_chaprob_df",
    data_reactive = df_chaprob()
  )
  
  # Maturité sexuelle -------------------------------------------------------

  
  # -- Résultat complet : modèles et tables
  table_modeles_maturite_resultats <- reactive({
    req(specimen())
    table_maturite_modeles(
      specimen_data = specimen(),
      prefer_combined = FALSE,
      variable = "ltm"
    )
  })
  
  # -- Index du meilleur modèle pour sélection par défaut
  default_model_index_maturite <- reactive({
    table <- table_modeles_maturite_resultats()$table$df
    req(nrow(table) > 0)
    idx <- which(table$recommande)
    if (length(idx) == 0) idx <- 1
    idx
  })
  
  # -- Tableau interactif des modèles
  output$table_modeles_maturite_table <- renderReactable({
    req(table_modeles_maturite_resultats())
    table <- table_modeles_maturite_resultats()$table$df
    idx <- default_model_index_maturite()
    
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
  
  # -- Modèle actuellement sélectionné
  selected_model_info_maturite <- reactive({
    selected <- getReactableState("table_modeles_maturite_table", "selected")
    req(!is.null(selected), table_modeles_maturite_resultats())
    table <- table_modeles_maturite_resultats()$table$df
    model_id <- table[selected, "modele_id", drop = TRUE]
    
    list(
      modele = stringr::str_extract(model_id, "TLO|ADD|INT|COM"),
      lien = stringr::str_extract(model_id, "logit|probit|cloglog"),
      variable = "ltm"
    )
  })
  
  # -- Résultat du modèle sélectionné
  fit_maturite_resultat <- reactive({
    req(specimen(), selected_model_info_maturite())
    best <- selected_model_info_maturite()
    
    fit_maturite(
      data = specimen(),
      variable = best$variable,
      modele = best$modele,
      lien = best$lien
    )
  })
  
  # -- Message explicatif
  output$message_modeles_L50 <- renderText({
    req(table_modeles_maturite_resultats())
    table_modeles_maturite_resultats()$message
  })
  
  # -- Graphique du modèle sélectionné
  plot_ogive_maturite <- reactive({
    req(fit_maturite_resultat())
    fit_maturite_resultat()$graphique
  })
  
  render_plot_ggplot("plot_ogive_maturite", plot_ogive_maturite)
  render_download_plot("download_ogive_maturite_plot", plot_ogive_maturite)
  
  # -- Tableau du modèle sélectionné
  table_ogive_maturite_df <- reactive({
    req(fit_maturite_resultat())
    fit_maturite_resultat()$table_resultats
  })
  
  ft_ogive_maturite <- reactive({
    req(fit_maturite_resultat())
    fit_maturite_resultat()$table_resultats_flextable
  })
  
  render_table_flextable("table_ogive_maturite_ui", ft_ogive_maturite)
  render_download_table("download_ogive_maturite_table", table_ogive_maturite_df())
  
}