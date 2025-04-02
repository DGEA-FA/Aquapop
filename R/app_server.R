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
  
  # === PSD (Proportional Size Distribution) ===================================
  
  # --- 1. Tableau : Indice PSD global -----------------------------------------
  
  # Reactive : retourne un tableau flextable avec l'indice PSD et IC95
  ft_psd_indice <- reactive({
    req(specimen_valid())
    psd_indice(data = specimen_valid(), format = "flextable")
  })
  
  # Rendu du tableau dans l'interface
  render_table_flextable("psd_indice_ui", ft_psd_indice)
  
  
  # --- 2. Tableau : Répartition par classe de taille --------------------------
  
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
  
  
  # --- 3. Graphique : Histogramme par classe de taille ------------------------
  
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
  
  
  # CPUE ------------------------------------------------
   ## tous --------------------------------------------------------------------
  selection_modele_CPUE_tous_data <- reactive({
    req(specimen(), sp_pen(), capture(), data_station())
    selection_modele_CPUE(
        capture = capture(),
        specimen = specimen(),
        espece = sp_pen(),
        station = data_station()
      )
  })
  output$selection_modele_CPUE_toustable <-  function() {
    kable_CPUEtous(data = selection_modele_CPUE_tous_data())
  }
  output$download_selection_modele_CPUE_toustable <-
    download_data_format_xlsx(nom_output = "selection_modele_CPUE_tous_data", data = selection_modele_CPUE_tous_data())
 
   CPUE_tous <- reactive({
    req(selection_modele_CPUE_tous_data())
    paste(selection_modele_CPUE_tous_data()[1 , "CPUE"]) #prendre le premier de la liste, car classe par ordre croissant de Ajustement
  })
  CPUEic_tous <- reactive({
    req(selection_modele_CPUE_tous_data())
    paste(selection_modele_CPUE_tous_data()[1 , 'IC 95%']) #prendre le premier de la liste, car classe par ordre croissant de Ajustement
  })
  ## femelle mature ----------------------------------------------------------
  selection_modele_CPUE_Fmature_data <- reactive({
    req(specimen(), sp_pen(), capture(), data_station())
      selection_modele_CPUE(
        capture = capture(),
        specimen = specimen(),
        espece = sp_pen(),
        station = data_station(),
        filtre_specimen = quo(maturite == "O" & sexe == "F")
      ) 
  })
  output$selection_modele_CPUE_Fmaturetable <-  function() {
    kable_CPUEFmature(data = selection_modele_CPUE_Fmature_data())
  }
  output$download_selection_modele_CPUE_Fmaturetable <-
    download_data_format_xlsx(nom_output = "selection_modele_CPUE_Fmature_data", data = selection_modele_CPUE_Fmature_data())
  CPUE_Fmature <- reactive({
    req(selection_modele_CPUE_Fmature_data())
    paste(selection_modele_CPUE_Fmature_data()[1 , "CPUE"]) #prendre le premier de la liste, car classe par ordre croissant de Ajustement
  })
  CPUEic_Fmature <- reactive({
    req(selection_modele_CPUE_Fmature_data())
    paste(selection_modele_CPUE_Fmature_data()[1 , 'IC 95%']) #prendre le premier de la liste, car classe par ordre croissant de Ajustement
  })
  ## abondance table ---------------------------------------------------------
 
  abondance1 <- reactive({
    req(
      specimen(),
      sp_pen(),
      CPUE_tous(),
      CPUEic_tous(),
      CPUE_Fmature(),
      CPUEic_Fmature()
    )
    
    # Générer la table d'abondance de base
    base_table <- abondance_table(
      specimen_data = specimen(),
      espece = sp_pen()
    )
    
    # Préparer la table pour la mise en page
    final_abondance_table <- prepare_abondance_table(
      abundance_table = base_table,
      CPUE_tous = CPUE_tous(),
      CPUEic_tous = CPUEic_tous(),
      CPUE_Fmature = CPUE_Fmature(),
      CPUEic_Fmature = CPUEic_Fmature()
    )
    
    return(final_abondance_table)
  })
  
  output$abondance1table <- render_gt({
    gt_abondance(data = abondance1())
  })
  
 
  output$download_abondance1 <-
    download_data_format_xlsx(nom_output = "abondance1", data = abondance1())
  # BPUE ------------------------------------------------
  biomasse1 <- reactive({
    req(specimen(), sp_pen(), data_station())
    
    # Appel de la fonction biomasse_table
    biomasse_table(
      specimen = specimen(),
      sp_pen = sp_pen(),
      data_station = data_station()
    )
  })
  output$biomasse1table <- render_gt({
    gt_biomasse(data = biomasse1())
  })
  output$download_biomasse1 <-
    download_data_format_xlsx(nom_output = "biomasse1", data = biomasse1())
 
 
  
  # Croissance ------------------------------------------------
  # 1. Réactif pour la table de modèles
  table_modeles_croissance <- reactive({
    req(specimen()) 
    courbe_croissance_comparaison(data = specimen(), format = "data.frame")
  })
  
  # 2. Réactif pour la ligne par défaut (le meilleur modèle)
  default_model_index <- reactive({
    req(table_modeles_croissance())
    best_model <- select_best_croissance_model(table_modeles_croissance())
    idx <- match(best_model, table_modeles_croissance()$methode)
    validate(need(!is.na(idx), "Le meilleur modèle n'a pas été trouvé dans les résultats"))
    idx
  })
  
  
  # 3. Table interactive avec sélection par défaut dynamique
  output$table_modeles_croissance_table <- renderReactable({
    req(default_model_index())
    reactable(
      labelled_data(table_modeles_croissance()),  
      selection = "single",
      onClick = "select",
      defaultSelected = default_model_index(),
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
    table_modeles_croissance()[selected, 1]
      })
  
  # 8. Bouton de téléchargement de la table des modèles
  render_download_table(
    id = "download_table_modeles_croissance",
    data_reactive = table_modeles_croissance()
  )
  
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
  deathdf <- reactive({
    req(data_specimen(), sp_pen())
    death(data = data_specimen(), espece = sp_pen()) %>% as.data.frame()
  })
  pp <- reactive({
    req(deathdf())
    peakplus(data = deathdf())
  })
  
  output$pp_og <- renderText({
    pp()
  })
  
  output$structureageplot4death <- renderPlot({ #potentiellement ici, est-ce que je devrais mettre data_specimen alors ? 
    req(specimen(), sp_pen(), pp(), nomsp_reactive())
    structure_age(dfspecimen = specimen(),
                       espece = sp_pen(),
                  nomsp = nomsp_reactive(), 
                  groupement = "tous") + 
        theme( legend.position = "none") +
        gghighlight::gghighlight(age == pp(), use_group_by = FALSE, label_key= espece)
    } )
  
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  newPP <- eventReactive(input$goButton, {
    req(input$newPPtext)  # Vérifie que l'entrée n'est pas vide ou NULL
    input$newPPtext
  })
  
  
  output$newPP_veriftext <- renderText({
    req(newPP())
    newPP()
  })
  
 
  agemax_val <- reactive({
    req(deathdf())
    agemax(data = deathdf())
  })
  
  df_corr <- reactive({
    req(deathdf(), newPP(), agemax_val())
    creation_df_CORR(data = deathdf(),
                     peakplus = newPP(),
                     agemax = agemax_val()) %>% as.data.frame()
  })
  df_ext <- reactive({
    req(df_corr(), newPP(), agemax_val())
    creation_df_EXT(data = df_corr(),
                    peakplus = newPP(),
                    agemax = agemax_val()) %>% as.data.frame()
  })
  dispersion_result <- reactive({
    req(df_corr())
    dispersiontest(data = df_corr())
  })
  mortalite1 <- reactive({
    req(df_ext())
    mortalite_selection_modeles(df_EXT = df_ext()) %>% as.data.frame()
  })
  
  output$mortalite1_table <-  function() {
    kable_mortalite1(data = mortalite1())
  }
  
  mortalite2 <- reactive({
    req(newPP(), agemax_val(), deathdf())
    mortalite_chaprob(data = deathdf(),
                            pp = newPP(),
                            agemax_val = agemax_val()) %>% as.data.frame()
  })
  
  output$mortalite2_table <-  function() {
    gt_mortalite2(data = mortalite2())
  }
 
   output$download_mortalite1 <-
    download_data_format_xlsx(nom_output = "mortalite_selection_modeles", data = mortalite1())

  
  zobs <- reactive({
    req(newPP(), deathdf(), agemax_val())
    get_zobs(PP = newPP(),
             death = deathdf(),
             agemax = agemax_val())
  })
 
   output$zobs_text <- renderText(zobs())
  # Maturite sexuelle -------------------------------------------------------

   df_maturiteltm <- reactive({
     req(specimen(), sp_pen())  # Vérifie que specimen() et sp_pen() ne sont pas NULL
     create_df_maturiteltm(specimen(), sp_pen())
   })   


   # df_maturiteage <- reactive({
   #   req(specimen(), sp_pen())
   #   specimen() %>%
   #     filter(sp == sp_pen(), maturite != "IND", sexe != "IND", !is.na(age)) %>%
   #     droplevels() %>%
   #     mutate(maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE))
   # })

   # 1. Ajustement des modèles séparés
   L50_models <- reactive({
     req(df_maturiteltm())
     fit_L50_models(df_maturiteltm())
   })
   
   # 1. Évaluation des modèles sexes séparés
   L50_results <- reactive({
     req(L50_models())  # Assurez-vous que les modèles sont disponibles
     evaluate_L50_models(L50_models()) 
   })
   
   # 2. Sélection des meilleurs modèles pour chaque sexe
   best_L50 <- reactive({
     req(L50_results())
     select_best_L50_models(L50_results())
   })
   
   # 3. Détection de la nécessité d'utiliser l'approche combinée
   use_combined <- reactive({
     best <- best_L50()
     if ("use_combined" %in% names(best)) {
       return(best$use_combined)
     }
     return(FALSE)
   })
   
   # 1. Ajustement des modèles combinés
   L50_combined_models <- reactive({
     req(df_maturiteltm())
     fit_L50_combined_models(df_maturiteltm())
   })
   
   # 4. Évaluation des modèles combinés (si nécessaire)
   L50_combined_evaluation <- reactive({
     req(use_combined(), L50_combined_models())
     evaluate_L50_models(L50_combined_models())
   })
   
   # 5. Sélection du meilleur modèle combiné (si nécessaire)
   best_L50_combined <- reactive({
     req(use_combined(), L50_combined_evaluation())
     select_best_L50_combined_model(L50_combined_evaluation())
   })
   
   # 6. AFFICHAGE DANS L'APPLICATION
   
   # 6.1 Tableau des résultats des modèles sexes séparés (toujours visible)
   output$separate_evaluation_table <- renderTable({
     req(L50_results())
     L50_results() %>% afficher_avec_labels()
     }, striped = TRUE, bordered = TRUE, hover = FALSE)
   
   # 6.2 Message des meilleurs modèles séparés
   output$best_separate_model_text <- renderText({
     best <- best_L50()
     if ("message" %in% names(best)) {
       return(best$message)
     } else {
       return(paste0(
         "Modèle sélectionné pour les mâles : ", best$best_model_M, "\n",
         "Modèle sélectionné pour les femelles : ", best$best_model_F
       ))
     }
   })
   
   output$combined_section <- renderUI({
     req(use_combined())  # Empêche l'exécution si use_combined() est FALSE
     
     best_comb <- best_L50_combined()
     
     message_text <- if ("message" %in% names(best_comb)) {
       best_comb$message
     } else {
       # Si plusieurs modèles sont sélectionnés, les afficher sous forme de liste
       paste0("Modèle(s) combiné(s) sélectionné(s) : ", paste(best_comb$best_model, collapse = ", "))
     }
     
     tagList(
       h4("Approche combinée"),
       p(message_text),
       tableOutput("combined_evaluation_table")
     )
   })
   
   
   
   # 6.4 Tableau des résultats des modèles combinés (affiché uniquement si nécessaire)
   output$combined_evaluation_table <- renderTable({
     req(use_combined(), L50_combined_evaluation())
     L50_combined_evaluation() %>% afficher_avec_labels()
   }, striped = TRUE, bordered = TRUE, hover = FALSE)
  
   
  
}