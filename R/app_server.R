# ════════════════════════════════════════════════════════════════════════
# SERVER – FONCTION PRINCIPALE app_server()
# ════════════════════════════════════════════════════════════════════════

app_server <- function(input, output, session) {

# Téléchargement des données ----

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
    filter_by_pen_lac_annee(data = data_temp(), typ_pech = input$typ_pech)
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
  render_download_table("cpue_tous_dl", cpue_modele_tous()$data)
  
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
  render_download_table("cpue_femelles_dl", cpue_modele_femelles()$data)
  
  ## Tableau d’abondance ----
  
  # Meilleurs modèles CPUE
  best_model_tous <- reactive({
    req(cpue_modele_tous())
    cpue_select_best_modele(cpue_modele_tous()$data)
  })
  
  best_model_femelles <- reactive({
    req(cpue_modele_femelles())
    cpue_select_best_modele(cpue_modele_femelles()$data)
  })
  
  # Résultat combiné : abondance
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
  
  render_table_flextable("abondance1_table", reactive(abondance1()$flextable))
  render_download_table("abondance1_dl", abondance1()$data)
  
  # BPUE - Biomasse ----
  biomasse1 <- reactive({
    req(specimen(), station_hasard_valide())
    bpue_generate_biomasse(
      data_specimen = specimen(),
      data_station  = station_hasard_valide()
    )
  })
  
  # Affichage flextable
  render_table_flextable("biomasse1table", reactive(biomasse1()$flextable))
  
  # Téléchargement en .xlsx
  render_download_table(id = "download_biomasse1", data_reactive = biomasse1()$data)

  
  # Taille, masse, âge ----
  
  # Résultat combiné (data + flextable)
  taille_masse_age_res <- reactive({
    req(specimen_valid())
    taille_masse_age(specimen_valid())
  })
  
  # Affichage du tableau flextable
  render_table_flextable("taillemasseage_ui", reactive(taille_masse_age_res()$flextable))
  
  # Bouton de téléchargement des données brutes
  render_download_table("dl_taillemasseage", taille_masse_age_res()$data)

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
  render_download_plot("download_groupetailleplot", reactive(res_structure_taille()$plot))
  
  # Téléchargement des données (data.frame)
  render_download_table("download_data4plot_taille", res_structure_taille()$data)
  
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
    output_id = "structureageplot",
    plot_reactive = reactive(res_structure_age()$plot),
    message_si_vide = "Aucun graphique n’a pu être généré : données d’âge manquantes ou inexploitables."
  )
  
  
  
  # Téléchargement PNG
  render_download_plot("download_groupeageplot", reactive(res_structure_age()$plot))
  
  # Téléchargement des données
  render_download_table("download_data4plot_age", res_structure_age()$data)
  
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
  render_table_flextable("psd_byclass_ui", reactive(psd_byclass_res()$flextable))
  
  # Bouton de téléchargement du tableau brut
  render_download_table("dl_psd_byclass", reactive(psd_byclass_res()$data))
  
  ## Répartition par classe de taille – Graphique ----
  
  # Affichage du graphique dans l'interface
  render_plot_ggplot("psd_byclass_plot", reactive(psd_byclass_res()$plot))
  
  # Bouton de téléchargement du graphique (PNG)
  render_download_plot("download_psd_byclass_plot", reactive(psd_byclass_res()$plot))
  
  # Relation masse-longueur ----
  
  # Reactive : retourne la liste {data, flextable, plot}
  masse_longueur_fit_res <- reactive({
    req(specimen())
    masse_longueur_fit(data = specimen())
  })
  
  ## Graphique ----
  
  render_plot_ggplot("plot_masselongueur", reactive(masse_longueur_fit_res()$plot))
  
  render_download_plot(
    id = "download_masselongueur_plot",
    plot_reactive = reactive(masse_longueur_fit_res()$plot)
  )
  
  ## Tableau des coefficients ----
  
  render_table_flextable("table_masselongueur_ui", reactive(masse_longueur_fit_res()$flextable))
  
  render_download_table(
    id = "download_masselongueur_table",
    data_reactive = reactive(masse_longueur_fit_res()$data)
  )
  # Indice de condition ----
  
  # Reactive : retourne la liste {data, flextable, plot_tous, plot_byclass}
  wri_res <- reactive({
    req(specimen_valid())
    wri(data = specimen_valid())
  })
  
  ## Tableau Wr ----
  
  render_table_flextable("wri_table_ui", reactive(wri_res()$flextable))
  render_download_table("download_wri_table", reactive(wri_res()$data))
  
  ## Graphique Wr par sexe ----
  
  render_plot_ggplot("wri_plot_tous", reactive(wri_res()$plot_tous))
  render_download_plot("download_wri_plot_tous", reactive(wri_res()$plot_tous))
  
  ## Graphique Wr par classe de taille ----
  
  render_plot_ggplot("wri_plot_byclass", reactive(wri_res()$plot_byclass))
  render_download_plot("download_wri_plot_byclass", reactive(wri_res()$plot_byclass))
  

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
  render_download_table(id = "download_table_modeles_croissance", data_reactive = table_modeles_croissance())
  
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
  render_plot_ggplot(
    output_id = "selectedmodelcroissanceplot",
    plot_reactive = plot_selectedmodelcroissance,
    width = 600, height = 400, res = 96
  )
  
  # Bouton de téléchargement du graphique
  render_download_plot(
    id = "download_selectedmodelcroissanceplot",
    plot_reactive = plot_selectedmodelcroissance,
    filename = "courbe_croissance",  # ou un reactive si tu veux le personnaliser
    width = 7, height = 5, dpi = 300,
    label = "Télécharger le graphique"
  )
  
  # Mortalité ----
  
  ## Tableau de sélection de modèles ----
  
  # Âge maximum
  mortalite_get_age_max_res <- reactive({
    req(specimen())
    mortalite_get_age_max(data = specimen())
  })
  
  # Peak Plus (pp)
  pp <- reactive({
    req(specimen())
    mortalite_get_peak_plus(data = specimen())
  })
  
  # Données corrigées pour la mortalité
  df_age_corrigee <- reactive({
    req(specimen(), pp(), mortalite_get_age_max_res())
    mortalite_prepare_corr(
      data = specimen(),
      age_peak_plus = pp(),
      age_max = mortalite_get_age_max_res()
    )
  })
  
  # Données étendues
  df_age_etendue <- reactive({
    req(df_age_corrigee(), mortalite_get_age_max_res())
    mortalite_prepare_extended(
      df_corrigee = df_age_corrigee(),
      age_max     = mortalite_get_age_max_res()
    )
  })
  
  # Test de surdispersion (Poisson)
  res_test_surdisp <- reactive({
    req(df_age_corrigee())
    mortalite_test_surdispersion_poisson(df_age_corrigee())
  })
  
  output$dispersion_msg <- renderText({
    req(res_test_surdisp())
    res_test_surdisp()$message
  })
  
  render_plot_ggplot(
    output_id = "plot_dispersion_poisson",
    plot_reactive = reactive(res_test_surdisp()$plot),
    width = 600, height = 400, res = 96
  )
  
  render_download_plot(
    id = "download_plot_dispersion_poisson",
    plot_reactive = reactive(res_test_surdisp()$plot),
    filename = "dispersion_poisson",
    width = 7, height = 5, dpi = 300,
    label = "Télécharger le graphique"
  )
  
  # Comparaison des modèles
  mortalite_compare_modele_res <- reactive({
    req(df_age_etendue())
    mortalite_compare_modele(data = df_age_etendue())
  })
  
  render_table_flextable("comparaison_mortalite_ui", reactive(mortalite_compare_modele_res()$flextable))
  
  render_download_table("download_comparaison_mortalite_table", reactive(mortalite_compare_modele_res()$data))
  
  # Meilleur modèle (nom)
  meilleur_modele_nom <- reactive({
    req(mortalite_compare_modele_res())
    mortalite_select_best_modele(mortalite_compare_modele_res()$data)
  })
  
  
   meilleur_modele_fit <- reactive({
    req(df_age_etendue(), meilleur_modele_nom())
     mortalite_fit_best_modele(df_age_etendue(), methode = meilleur_modele_nom())
  })
  
  # Phrase descriptive
  output$phrase_mortalite <- renderText({
    req(mortalite_compare_modele_res(), meilleur_modele_nom())
    
    ligne <- mortalite_compare_modele_res()$data |>
      dplyr::filter(Méthode == meilleur_modele_nom())
    
    modele_nom <- toupper(meilleur_modele_nom())
    mortalite_A <- ligne$A
    
    glue::glue("Le modèle {modele_nom} décrit le mieux la mortalité de la population. La mortalité annuelle s’élève à {mortalite_A} %.") |> as.character()
  })
  
  
  ## Graphique du modèle choisi ----
  
  
  # Courbe du modèle retenu
  plot_mortalite <- reactive({
    req(specimen(), meilleur_modele_fit(), mortalite_compare_modele_res())
  
    mortalite_plot_modele(
      specimen = specimen(),
      modele = meilleur_modele_fit(),
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
  
  ## Chapman-Robson ----
  
  res_chaprob <- reactive({
    req(specimen(), pp(), mortalite_get_age_max_res())
    mortalite_chaprob(
      specimen = specimen(),
      pp       = pp(),
      age_max  = mortalite_get_age_max_res()
    )
  })
  
  render_table_flextable("table_chaprob", reactive(res_chaprob()$flextable))
  
  render_download_table("download_chaprob_df", reactive(res_chaprob()$data))
  
  
  # Maturité sexuelle ----
  ## Longueur à maturité ----
  
  ### Tableau de sélection de modèles ----
  
  # Résultat complet : modèles et tables
  table_modeles_maturite_resultats <- reactive({
    req(specimen())
    maturite_compare_modele(
      specimen_data = specimen(),
      prefer_combined = FALSE,
      variable = "ltm"
    )
  })
  
  # Index du meilleur modèle pour sélection par défaut
  default_model_index_maturite <- reactive({
    table <- table_modeles_maturite_resultats()$table$df
    req(nrow(table) > 0)
    idx <- which(table$recommande)
    if (length(idx) == 0) idx <- 1
    idx
  })
  
  # Tableau interactif des modèles
  output$table_modeles_maturite_table <- renderReactable({
    req(table_modeles_maturite_resultats())
    table <- table_modeles_maturite_resultats()$table$df
    idx <- default_model_index_maturite()
    
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
  
  
  # Message explicatif sur les modèles évalués
  output$message_l50 <- renderText({
    req(table_modeles_maturite_resultats())
    table_modeles_maturite_resultats()$message
  })
  
  
  # Résultat du modèle sélectionné
  maturite_generate_modele_res <- reactive({
    req(specimen(), selected_model_info_maturite())
    maturite_generate_modele(
      data = specimen(),
      variable = selected_model_info_maturite()$variable,
      modele = selected_model_info_maturite()$modele,
      lien = selected_model_info_maturite()$lien
    )
  })
  
  
  ### Tableau du modèle choisi ----
  render_table_flextable("table_ogive_maturite_ui", reactive(maturite_generate_modele_res()$table_resultats_flextable))
  render_download_table("download_ogive_maturite_table", reactive(maturite_generate_modele_res()$table_resultats))
 
  ### Graphique du modèle choisi ----
  render_plot_ggplot("plot_ogive_maturite", reactive(maturite_generate_modele_res()$graphique))
  render_download_plot("download_ogive_maturite_plot", reactive(maturite_generate_modele_res()$graphique))
 
   
  ## Âge à maturité ----
  ### Tableau de sélection de modèles ----
  
  ### Tableau du modèle choisi ----
  ### Graphique du modèle choisi ----
  
  
}