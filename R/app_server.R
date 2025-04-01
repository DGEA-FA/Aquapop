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
  render_flextable_ui("taillemasseage_ui", ft_taillemasseage)
  
  # 4. Bouton de téléchargement
  render_download_button_ui("dl_taillemasseage", df_taillemasseage())
  
  
  # Structure taille ggplot ------------------------------------------------

  output$structuretailleplot <- renderPlot({
    req(specimen_valid(), sp_pen(), nomsp_reactive(), binwidth_reactive())
    structure_taille(dfspecimen = specimen_valid(),
                     espece = sp_pen(),
                     binwidth = binwidth_reactive(),
                     nomsp = nomsp_reactive(),
                     groupement = input$groupetailleplot)
  }, res = 96)
  
  
  
  output$download_groupetailleplot <- downloadHandler(
    filename = function() {
      paste("groupetailleplot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen_valid(), sp_pen(), binwidth_reactive(), nomsp_reactive())
        if (input$groupetailleplot == "tous") {
          structure_taille(dfspecimen = specimen_valid(),
                           espece = sp_pen(),
                           binwidth = binwidth_reactive(),
                           nomsp = nomsp_reactive(),
                           groupement = "tous")
        } else if (input$groupetailleplot == "marquage") {
          structure_taille(dfspecimen = specimen_valid(),
                           espece = sp_pen(),
                           binwidth = binwidth_reactive(),
                           nomsp = nomsp_reactive(),
                           groupement = "marquage")
        } else if (input$groupetailleplot == "sexe") {
          structure_taille(dfspecimen = specimen_valid(),
                           espece = sp_pen(),
                           binwidth = binwidth_reactive(),
                           nomsp = nomsp_reactive(),
                           groupement = "sexe")
        } else if (input$groupetailleplot == "maturite") {
          structure_taille(dfspecimen = specimen_valid(),
                           espece = sp_pen(),
                           binwidth = binwidth_reactive(),
                           nomsp = nomsp_reactive(),
                           groupement = "maturite")
        }
      }, width = 10, height = 7, dpi = 300)  # Specify width, height, and dpi if needed
    },
    contentType = 'image/png'  # Ensure the content type is set correctly for PNG files
  )
  

  data4plot_taille <- reactive({
    req(specimen_valid(), sp_pen(), binwidth_reactive(), nomsp_reactive())
    
    plot <- structure_taille(dfspecimen = specimen_valid(),
                             espece = sp_pen(),
                             binwidth = binwidth_reactive(),
                             nomsp = nomsp_reactive(),
                             groupement = input$groupetailleplot)
    
    get_df_from_plot(plot, groupement = input$groupetailleplot)
  })
  
  
  
  
  output$download_data4plot_taille <-
    download_data_format_xlsx(nom_output = paste0("data4plot_taille_",input$groupetailleplot), data = data4plot_taille())
  
  
  
  # Structure age ggplot ------------------------------------------------
  output$structureageplot <- renderPlot({
    req(specimen_valid(), sp_pen(), nomsp_reactive())
    
    structure_age(dfspecimen = specimen_valid(),
                  espece = sp_pen(),
                  nomsp = nomsp_reactive(),
                  groupement = input$groupeageplot)
  }, res = 96)
  

  
  output$download_groupeageplot <- downloadHandler(
    filename = function() {
      paste("groupeageplot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen_valid(), sp_pen(), nomsp_reactive())
        
        structure_age(dfspecimen = specimen_valid(),
                      espece = sp_pen(),
                      nomsp = nomsp_reactive(),
                      groupement = input$groupeageplot)
      }, width = 10, height = 7, dpi = 300)  # Specify width, height, and dpi if needed
    },
    contentType = 'image/png'
  )
  
  
  
  data4plot_age <- reactive({
    req(specimen_valid(), sp_pen(), nomsp_reactive())
    
    plot <- structure_age(dfspecimen = specimen_valid(),
                          espece = sp_pen(),
                          nomsp = nomsp_reactive(),
                          groupement = input$groupeageplot)
    
    get_df_from_plot(plot, groupement = input$groupeageplot)
  })
  
  
  
  
  output$download_data4plot_age <- download_data_format_xlsx(
    nom_output = paste0("data4plot_age_", input$groupeageplot),
    data = data4plot_age()
  )
  
  
  # CPUE ------------------------------------------------
  ## verification du n de specimen ------------------------------------------------
  verif_n_data <- reactive({
    req(specimen(), sp_pen(), capture())
    verif_n_recolte_specimen(
      capture = capture(),
      specimen = specimen(),
      espece = sp_pen()
    ) %>% as.data.frame()
  })
  output$verif_ntable <- renderTable(verif_n_data())
 
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
  # PSD -------------------------------------------------------
  psd1 <- reactive({
    req(specimen_valid(), sp_pen())
    psd_indice(data = specimen_valid(), sp = sp_pen()) %>% as.data.frame()
  })
  
  output$psd1_table <-  function() {
    kable_psd1(data = psd1())
  }
 
   psd2 <- reactive({
    req(specimen_valid(), sp_pen())
    psd_byclass(data = specimen_valid(), espece = sp_pen()) %>% as.data.frame()
  })
  output$psd2_table <-  function() {
    kable_psd2(data = psd2())
  }
  output$download_psd2 <-
    download_data_format_xlsx(nom_output = "psd_byclass", data = psd2())

    output$psd1plot <- renderPlot({
    req(psd2())
    psd_plot(data = psd2())
  }, res = 96)
  output$download_psd1plot <- downloadHandler(
    filename = function() {
      paste("psd1plot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(psd2(), sp_pen())
        psd_plot(data = psd2())
      } , device = "png")
    }
  )
  # Relation masse-longueur -------------------------------------------------------
    relation_masse_longueur_data <- reactive({
    req(specimen(), sp_pen())
    relation_masse_longueur(specimen(), sp_pen())
  })
  
  # Affichage du tableau des coefficients
  output$relation_masse_longueur_table <- renderTable({
    relation_masse_longueur_data()$table
  })
  
  # Bouton de téléchargement
  output$download_relation_masse_longueur_table <-
    download_data_format_xlsx(
      nom_output = "relation_masse_longueur_coefficient",
      data = relation_masse_longueur_data()$table)
    
  
  output$masselongueur_plot <- plotly::renderPlotly({
    req(relation_masse_longueur_data())
    relation_masse_longueur_data()$graph %>%
      plotly::ggplotly(tooltip = "text")
  })
 
  output$download_masselongueur_plot <- downloadHandler(
    filename = function() {
      paste("relation_masse_longueur", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(relation_masse_longueur_data())
        relation_masse_longueur_data()$graph
      } , device = "png")
    }
  )
  
  # Indice de condition -------------------------------------------------------
  wri1data <- reactive({
    req(data_specimen(), sp_pen())
    table_wri(data = data_specimen(), espece = sp_pen()) %>% as.data.frame()
  })
  output$wri1_table <-  function() {
    kable_wri(data = wri1data())
  }
  output$download_wri1 <-
    download_data_format_xlsx(nom_output = "table_wri",
                              data = wri1data()%>%
                                tibble::rownames_to_column(var = "Catégorie"))
  ## Wri tous ggplot ------------------------------------------------
  output$wri2plot <- renderPlot({
    req(data_specimen(), sp_pen())
    fig_wri_tous(data = data_specimen(), espece = sp_pen())
  }, res = 96)
  output$download_wri2plot <- downloadHandler(
    filename = function() {
      paste("wri_tous", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(data_specimen(), sp_pen())
        fig_wri_tous(data = data_specimen(), espece = sp_pen())
      } , device = "png")
    }
  )
  ## Wri byclass ggplot ------------------------------------------------
  output$wri3plot <- renderPlot({
    req(data_specimen(), sp_pen())
    fig_wri_byclass(data = data_specimen(), espece = sp_pen())
  }, res = 96)
  output$download_wri3plot <- downloadHandler(
    filename = function() {
      paste("wri_byclass", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(data_specimen(), sp_pen())
        fig_wri_byclass(data = data_specimen(), espece = sp_pen())
      } , device = "png")
    }
  )
  # Croissance ------------------------------------------------
  croissance1 <- reactive({
    req(data_specimen(), sp_pen())  # S'assurer que les données nécessaires sont disponibles
    
    # Appeler directement la fonction `courbe_croissance_comparaison`
    courbe_croissance_comparaison(dfspecimen = data_specimen(), sp_pen = sp_pen()) %>% as.data.frame()
  })

  output$croissance1_table <- renderReactable(
    reactable(
      labelled_data(croissance1()),  
      selection = "single",
      onClick = "select",
      defaultSelected = 1,
      defaultColDef = colDef(
        align = "center",  # Centre les valeurs par défaut
        headerStyle = list(textAlign = "center")  # Centre les titres par défaut
      )
    )
  )

  selectedmodelcroissance <- reactive({ #selection du modele choisi dans le reactable
    selected <- getReactableState("croissance1_table", "selected")
    req(selected, croissance1())
    details <- croissance1()[selected, 1]
  })
  
  
  
  output$download_croissance1 <-
    download_data_format_xlsx(nom_output = "courbe_croissance_comparaison", data = croissance1())
  
  output$selectedmodelcroissanceplot <- renderPlot({
    req(selectedmodelcroissance(), data_specimen(), sp_pen(), croissance1())
    
    # Appeler la fonction générique en fonction du modèle sélectionné
    courbe_croissance_plot(dfspecimen = data_specimen(), sp_pen = sp_pen(), tablemodele = croissance1(), modele = paste(selectedmodelcroissance()))
  }, res = 96)
  
  
  # download_selectedmodelcroissanceplot
  output$download_selectedmodelcroissanceplot <- downloadHandler(
    filename = function() {
      req(selectedmodelcroissance())
      if (selectedmodelcroissance() == "Von Bertalanffy") {
        paste("croissance_vb", '.png', sep = '')
      } else if (selectedmodelcroissance() == "Gompertz") {
        paste("croissance_gomp", '.png', sep = '')
      } else if (selectedmodelcroissance() == "Logistique") {
        paste("croissance_logistique", '.png', sep = '')
      }
    },
    content = function(file) {
      ggsave(file, plot = {
        req(selectedmodelcroissance(), data_specimen(), sp_pen(), croissance1())
        
        # Appeler la fonction générique en fonction du modèle sélectionné
        courbe_croissance_plot(dfspecimen = data_specimen(), sp_pen = sp_pen(), tablemodele = croissance1(), modele = selectedmodelcroissance())
      }, device = "png")
    }
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
  
   
  #  LTMmaturite.model.logit.L <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm, family = binomial(link = "logit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.probit.L <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm, family = binomial(link = "probit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.cloglog.L <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df_maturiteltm()) })
  # 
  #  LTMmaturite.model.logit.ADD <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm + sexe, family = binomial(link = "logit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.probit.ADD <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm + sexe, family = binomial(link = "probit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.cloglog.ADD <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm + sexe, family = binomial(link = "cloglog"), data = df_maturiteltm()) })
  # 
  #  LTMmaturite.model.logit.INT <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm * sexe, family = binomial(link = "logit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.probit.INT <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm * sexe, family = binomial(link = "probit"), data = df_maturiteltm()) })
  #  LTMmaturite.model.cloglog.INT <- reactive({ req(df_maturiteltm()); glm(maturite ~ ltm * sexe, family = binomial(link = "cloglog"), data = df_maturiteltm()) })
  # 
  # L50_selection_modeles_df <- reactive({
  #   req(
  #     LTMmaturite.model.logit.L(),
  #     LTMmaturite.model.probit.L(),
  #     LTMmaturite.model.cloglog.L(),
  #     LTMmaturite.model.logit.ADD(),
  #     LTMmaturite.model.probit.ADD(),
  #     LTMmaturite.model.cloglog.ADD(),
  #     LTMmaturite.model.logit.INT(),
  #     LTMmaturite.model.probit.INT(),
  #     LTMmaturite.model.cloglog.INT(),
  #     df_maturiteltm()
  #   )
  #   L50_selection_modeles(
  #     df = df_maturiteltm(),
  #     modlogit.l = LTMmaturite.model.logit.L(),
  #     modprobit.l = LTMmaturite.model.probit.L(),
  #     modcloglog.l = LTMmaturite.model.cloglog.L(),
  #     modlogit.add = LTMmaturite.model.logit.ADD(),
  #     modprobit.add = LTMmaturite.model.probit.ADD(),
  #     modcloglog.add = LTMmaturite.model.cloglog.ADD(),
  #     modlogit.int = LTMmaturite.model.logit.INT(),
  #     modprobit.int = LTMmaturite.model.probit.INT(),
  #     modcloglog.int = LTMmaturite.model.cloglog.INT()
  #   )
  # })
  # output$L50_selection_modeles_table <-
  #   renderReactable(
  #     reactable(
  #       L50_selection_modeles_df(),
  #       selection = "single",
  #       onClick = "select",
  #       fullWidth = TRUE,
  #       # defaultSelected = 1
  #     )
  #   )
  # output$download_L50_selection_modeles_table <-
  #   download_data_format_xlsx(nom_output = "L50_selection_modeles_table", data = L50_selection_modeles_df())
  # ## minitable l50 -----------------------------------------------------------
  # minitablel50.logit.L <- reactive({
  #   req(LTMmaturite.model.logit.L())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.logit.L(), model_type = "l") %>% as.data.frame()
  # })
  # 
  # minitablel50.probit.L <- reactive({
  #   req(LTMmaturite.model.probit.L())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.probit.L(), model_type = "l") %>% as.data.frame()
  # })
  # 
  # minitablel50.cloglog.L <- reactive({
  #   req(LTMmaturite.model.cloglog.L())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.cloglog.L(), model_type = "l") %>% as.data.frame()
  # })
  # 
  # minitablel50.logit.ADD <- reactive({
  #   req(LTMmaturite.model.logit.ADD())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.logit.ADD(), model_type = "add") %>% as.data.frame()
  # })
  # 
  # minitablel50.probit.ADD <- reactive({
  #   req(LTMmaturite.model.probit.ADD())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.probit.ADD(), model_type = "add") %>% as.data.frame()
  # })
  # 
  # minitablel50.cloglog.ADD <- reactive({
  #   req(LTMmaturite.model.cloglog.ADD())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.cloglog.ADD(), model_type = "add") %>% as.data.frame()
  # })
  # 
  # minitablel50.logit.INT <- reactive({
  #   req(LTMmaturite.model.logit.INT(), df_maturiteltm())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.logit.INT(), model_type = "int", df = df_maturiteltm()) %>% as.data.frame()
  # })
  # 
  # minitablel50.probit.INT <- reactive({
  #   req(LTMmaturite.model.probit.INT(), df_maturiteltm())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.probit.INT(), model_type = "int", df = df_maturiteltm()) %>% as.data.frame()
  # })
  # 
  # minitablel50.cloglog.INT <- reactive({
  #   req(LTMmaturite.model.cloglog.INT(), df_maturiteltm())
  #   generate_minitable_param_modelL50(model = LTMmaturite.model.cloglog.INT(), model_type = "int", df = df_maturiteltm()) %>% as.data.frame()
  # })
  # 
  # ## selection modele L50 ----------------------------------------------------
  # selectedmodelL50 <- reactive({
  #   selected <-
  #     getReactableState("L50_selection_modeles_table", "selected")
  #   req(selected, L50_selection_modeles_df())
  #   details <- L50_selection_modeles_df()[selected, 1]
  #   paste(details)
  # })
  # 
  # output$selectedmodelL50minitable <- renderTable({
  #   req(selectedmodelL50())
  #   if (selectedmodelL50() == "Longueur (lien logit)") {
  #     req(minitablel50.logit.L())
  #     minitablel50.logit.L()
  #   } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
  #     req(minitablel50.logit.ADD())
  #     minitablel50.logit.ADD()
  #   } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
  #     req(minitablel50.logit.INT())
  #     minitablel50.logit.INT()
  #   } else if (selectedmodelL50() == "Longueur (lien probit)") {
  #     req(minitablel50.probit.L())
  #     minitablel50.probit.L()
  #   } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
  #     req(minitablel50.probit.ADD())
  #     minitablel50.probit.ADD()
  #   } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
  #     req(minitablel50.probit.INT())
  #     minitablel50.probit.INT()
  #   } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
  #     req(minitablel50.cloglog.L())
  #     minitablel50.cloglog.L()
  #   } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
  #     req(minitablel50.cloglog.ADD())
  #     minitablel50.cloglog.ADD()
  #   } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
  #     req(minitablel50.cloglog.INT())
  #     minitablel50.cloglog.INT()
  #   }
  # })
  # output$download_minitableselectedmodelL50 <-
  #   download_data_format_xlsx(nom_output = ({
  #     req(selectedmodelL50())
  #     if (selectedmodelL50() == "Longueur (lien logit)") {
  #       paste0("minitablel50.logit.L")
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
  #       paste0("minitablel50.logit.ADD")
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
  #       paste0("minitablel50.logit.INT")
  #     } else if (selectedmodelL50() == "Longueur (lien probit)") {
  #       paste0("minitablel50.probit.L")
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
  #       paste0("minitablel50.probit.ADD")
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
  #       paste0("minitablel50.probit.INT")
  #     } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
  #       paste0("minitablel50.cloglog.L")
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
  #       paste0("minitablel50.cloglog.ADD")
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
  #       paste0("minitablel50.cloglog.INT")
  #     }
  #   }),
  #   data = ({
  #     req(selectedmodelL50())
  #     if (selectedmodelL50() == "Longueur (lien logit)") {
  #       req(minitablel50.logit.L())
  #       minitablel50.logit.L()
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
  #       req(minitablel50.logit.ADD())
  #       minitablel50.logit.ADD()
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
  #       req(minitablel50.logit.INT())
  #       minitablel50.logit.INT()
  #     } else if (selectedmodelL50() == "Longueur (lien probit)") {
  #       req(minitablel50.probit.L())
  #       minitablel50.probit.L()
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
  #       req(minitablel50.probit.ADD())
  #       minitablel50.probit.ADD()
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
  #       req(minitablel50.probit.INT())
  #       minitablel50.probit.INT()
  #     } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
  #       req(minitablel50.cloglog.L())
  #       minitablel50.cloglog.L()
  #     } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
  #       req(minitablel50.cloglog.ADD())
  #       minitablel50.cloglog.ADD()
  #     } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
  #       req(minitablel50.cloglog.INT())
  #       minitablel50.cloglog.INT()
  #     }
  #   }))
  # ## selected L50 plot -------------------------------------------------------
  # output$selectedmodelL50plot <- renderPlot({
  #   req(selectedmodelL50(), df_maturiteltm())
  # 
  #   model_info <- list(
  #     "Longueur (lien logit)" = list(model = LTMmaturite.model.logit.L, minitable = minitablel50.logit.L, type = "L"),
  #     "Longueur + Sexe (lien logit)" = list(model = LTMmaturite.model.logit.ADD, minitable = minitablel50.logit.ADD, type = "ADD"),
  #     "Longueur + Sexe + Longueur:Sexe (lien logit)" = list(model = LTMmaturite.model.logit.INT, minitable = minitablel50.logit.INT, type = "INT"),
  #     "Longueur (lien probit)" = list(model = LTMmaturite.model.probit.L, minitable = minitablel50.probit.L, type = "L"),
  #     "Longueur + Sexe (lien probit)" = list(model = LTMmaturite.model.probit.ADD, minitable = minitablel50.probit.ADD, type = "ADD"),
  #     "Longueur + Sexe + Longueur:Sexe (lien probit)" = list(model = LTMmaturite.model.probit.INT, minitable = minitablel50.probit.INT, type = "INT"),
  #     "Longueur (lien cloglog)" = list(model = LTMmaturite.model.cloglog.L, minitable = minitablel50.cloglog.L, type = "L"),
  #     "Longueur + Sexe (lien cloglog)" = list(model = LTMmaturite.model.cloglog.ADD, minitable = minitablel50.cloglog.ADD, type = "ADD"),
  #     "Longueur + Sexe + Longueur:Sexe (lien cloglog)" = list(model = LTMmaturite.model.cloglog.INT, minitable = minitablel50.cloglog.INT, type = "INT")
  #   )
  # 
  #   selected_info <- model_info[[selectedmodelL50()]]
  # 
  #   req(selected_info$model(), selected_info$minitable())
  # 
  #   generate_ggmodel_L50(df = df_maturiteltm(),
  #                        model = selected_info$model(),
  #                        minitable = selected_info$minitable(),
  #                        model_type = selected_info$type)
  # }, res = 96)
  # ## download_selectedmodelL50plot -------------------------------------------
  # output$download_selectedmodelL50plot <- downloadHandler(
  #   filename = function() {
  #     req(selectedmodelL50())
  # 
  #     file_names <- list(
  #       "Longueur (lien logit)" = "ggLTMmaturite_logit.L.png",
  #       "Longueur + Sexe (lien logit)" = "ggLTMmaturite_logit.ADD.png",
  #       "Longueur + Sexe + Longueur:Sexe (lien logit)" = "ggLTMmaturite_logit.INT.png",
  #       "Longueur (lien probit)" = "ggLTMmaturite_probit.L.png",
  #       "Longueur + Sexe (lien probit)" = "ggLTMmaturite_probit.ADD.png",
  #       "Longueur + Sexe + Longueur:Sexe (lien probit)" = "ggLTMmaturite_probit.INT.png",
  #       "Longueur (lien cloglog)" = "ggLTMmaturite_cloglog.L.png",
  #       "Longueur + Sexe (lien cloglog)" = "ggLTMmaturite_cloglog.ADD.png",
  #       "Longueur + Sexe + Longueur:Sexe (lien cloglog)" = "ggLTMmaturite_cloglog.INT.png"
  #     )
  # 
  #     file_names[[selectedmodelL50()]]
  #   },
  # 
  #   content = function(file) {
  #     req(selectedmodelL50(), df_maturiteltm())
  # 
  #     model_info <- list(
  #       "Longueur (lien logit)" = list(model = LTMmaturite.model.logit.L, minitable = minitablel50.logit.L, type = "L"),
  #       "Longueur + Sexe (lien logit)" = list(model = LTMmaturite.model.logit.ADD, minitable = minitablel50.logit.ADD, type = "ADD"),
  #       "Longueur + Sexe + Longueur:Sexe (lien logit)" = list(model = LTMmaturite.model.logit.INT, minitable = minitablel50.logit.INT, type = "INT"),
  #       "Longueur (lien probit)" = list(model = LTMmaturite.model.probit.L, minitable = minitablel50.probit.L, type = "L"),
  #       "Longueur + Sexe (lien probit)" = list(model = LTMmaturite.model.probit.ADD, minitable = minitablel50.probit.ADD, type = "ADD"),
  #       "Longueur + Sexe + Longueur:Sexe (lien probit)" = list(model = LTMmaturite.model.probit.INT, minitable = minitablel50.probit.INT, type = "INT"),
  #       "Longueur (lien cloglog)" = list(model = LTMmaturite.model.cloglog.L, minitable = minitablel50.cloglog.L, type = "L"),
  #       "Longueur + Sexe (lien cloglog)" = list(model = LTMmaturite.model.cloglog.ADD, minitable = minitablel50.cloglog.ADD, type = "ADD"),
  #       "Longueur + Sexe + Longueur:Sexe (lien cloglog)" = list(model = LTMmaturite.model.cloglog.INT, minitable = minitablel50.cloglog.INT, type = "INT")
  #     )
  # 
  #     selected_info <- model_info[[selectedmodelL50()]]
  # 
  #     req(selected_info$model(), selected_info$minitable())
  # 
  #     ggsave(
  #       file,
  #       plot = generate_ggmodel_L50(
  #         df = df_maturiteltm(),
  #         model = selected_info$model(),
  #         minitable = selected_info$minitable(),
  #         model_type = selected_info$type
  #       ),
  #       device = "png"
  #     )
  #   }
  # )
  # 
  # ## A50 -------------------------------------------------------
  # AGEmaturite.model.logit.L <- reactive({ req(df_maturiteage()); glm(maturite ~ age, family = binomial(link = "logit"), data = df_maturiteage()) })
  # AGEmaturite.model.probit.L <- reactive({ req(df_maturiteage()); glm(maturite ~ age, family = binomial(link = "probit"), data = df_maturiteage()) })
  # AGEmaturite.model.cloglog.L <- reactive({ req(df_maturiteage()); glm(maturite ~ age, family = binomial(link = "cloglog"), data = df_maturiteage()) })
  # 
  # AGEmaturite.model.logit.ADD <- reactive({ req(df_maturiteage()); glm(maturite ~ age + sexe, family = binomial(link = "logit"), data = df_maturiteage()) })
  # AGEmaturite.model.probit.ADD <- reactive({ req(df_maturiteage()); glm(maturite ~ age + sexe, family = binomial(link = "probit"), data = df_maturiteage()) })
  # AGEmaturite.model.cloglog.ADD <- reactive({ req(df_maturiteage()); glm(maturite ~ age + sexe, family = binomial(link = "cloglog"), data = df_maturiteage()) })
  # 
  # AGEmaturite.model.logit.INT <- reactive({ req(df_maturiteage()); glm(maturite ~ age * sexe, family = binomial(link = "logit"), data = df_maturiteage()) })
  # AGEmaturite.model.probit.INT <- reactive({ req(df_maturiteage()); glm(maturite ~ age * sexe, family = binomial(link = "probit"), data = df_maturiteage()) })
  # AGEmaturite.model.cloglog.INT <- reactive({ req(df_maturiteage()); glm(maturite ~ age * sexe, family = binomial(link = "cloglog"), data = df_maturiteage()) })
  # 
  # A50_selection_modeles_df <- reactive({
  #   req(
  #     AGEmaturite.model.logit.L(),
  #     AGEmaturite.model.probit.L(),
  #     AGEmaturite.model.cloglog.L(),
  #     AGEmaturite.model.logit.ADD(),
  #     AGEmaturite.model.probit.ADD(),
  #     AGEmaturite.model.cloglog.ADD(),
  #     AGEmaturite.model.logit.INT(),
  #     AGEmaturite.model.probit.INT(),
  #     AGEmaturite.model.cloglog.INT(),
  #     df_maturiteage()
  #   )
  #   A50_selection_modeles(
  #     df = df_maturiteage(),
  #     modlogit.l = AGEmaturite.model.logit.L(),
  #     modprobit.l = AGEmaturite.model.probit.L(),
  #     modcloglog.l = AGEmaturite.model.cloglog.L(),
  #     modlogit.add = AGEmaturite.model.logit.ADD(),
  #     modprobit.add = AGEmaturite.model.probit.ADD(),
  #     modcloglog.add = AGEmaturite.model.cloglog.ADD(),
  #     modlogit.int = AGEmaturite.model.logit.INT(),
  #     modprobit.int = AGEmaturite.model.probit.INT(),
  #     modcloglog.int = AGEmaturite.model.cloglog.INT()
  #   )
  # })
  # output$A50_selection_modeles_table <-
  #   renderReactable(
  #     reactable(
  #       A50_selection_modeles_df(),
  #       selection = "single",
  #       onClick = "select",
  #       width = "auto",
  #       defaultSelected = 1
  #     )
  #   )
  # output$download_A50_selection_modeles_table <-
  #   download_data_format_xlsx(nom_output = "A50_selection_modeles_table", data = A50_selection_modeles_df())
  # ## minitable a50 -----------------------------------------------------------
  # minitablea50.logit.L <- reactive({
  #   req(AGEmaturite.model.logit.L())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.logit.L(), model_type = "L") %>% as.data.frame()
  # })
  # 
  # minitablea50.probit.L <- reactive({
  #   req(AGEmaturite.model.probit.L())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.probit.L(), model_type = "L") %>% as.data.frame()
  # })
  # 
  # minitablea50.cloglog.L <- reactive({
  #   req(AGEmaturite.model.cloglog.L())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.cloglog.L(), model_type = "L") %>% as.data.frame()
  # })
  # 
  # minitablea50.logit.ADD <- reactive({
  #   req(AGEmaturite.model.logit.ADD())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.logit.ADD(), model_type = "ADD") %>% as.data.frame()
  # })
  # 
  # minitablea50.probit.ADD <- reactive({
  #   req(AGEmaturite.model.probit.ADD())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.probit.ADD(), model_type = "ADD") %>% as.data.frame()
  # })
  # 
  # minitablea50.cloglog.ADD <- reactive({
  #   req(AGEmaturite.model.cloglog.ADD())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.cloglog.ADD(), model_type = "ADD") %>% as.data.frame()
  # })
  # 
  # minitablea50.logit.INT <- reactive({
  #   req(AGEmaturite.model.logit.INT(), df_maturiteage())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.logit.INT(), model_type = "INT", df = df_maturiteage()) %>% as.data.frame()
  # })
  # 
  # minitablea50.probit.INT <- reactive({
  #   req(AGEmaturite.model.probit.INT(), df_maturiteage())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.probit.INT(), model_type = "INT", df = df_maturiteage()) %>% as.data.frame()
  # })
  # 
  # minitablea50.cloglog.INT <- reactive({
  #   req(AGEmaturite.model.cloglog.INT(), df_maturiteage())
  #   generate_minitable_param_model_a50(model = AGEmaturite.model.cloglog.INT(), model_type = "INT", df = df_maturiteage()) %>% as.data.frame()
  # })
  # ## selection modele A50 ----------------------------------------------------
  # selectedmodelA50 <- reactive({
  #   selected <-
  #     getReactableState("A50_selection_modeles_table", "selected")
  #   req(selected, A50_selection_modeles_df())
  #   details <- A50_selection_modeles_df()[selected, 1]
  #   paste(details)
  # })
  # 
  # output$selectedmodelA50minitable <- renderTable({
  #   req(selectedmodelA50())
  #   if (selectedmodelA50() == "Âge (lien logit)") {
  #     req(minitablea50.logit.L())
  #     minitablea50.logit.L()
  #   } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
  #     req(minitablea50.logit.ADD())
  #     minitablea50.logit.ADD()
  #   } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
  #     req(minitablea50.logit.INT())
  #     minitablea50.logit.INT()
  #   } else if (selectedmodelA50() == "Âge (lien probit)") {
  #     req(minitablea50.probit.L())
  #     minitablea50.probit.L()
  #   } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
  #     req(minitablea50.probit.ADD())
  #     minitablea50.probit.ADD()
  #   } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
  #     req(minitablea50.probit.INT())
  #     minitablea50.probit.INT()
  #   } else if (selectedmodelA50() == "Âge (lien cloglog)") {
  #     req(minitablea50.cloglog.L())
  #     minitablea50.cloglog.L()
  #   } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
  #     req(minitablea50.cloglog.ADD())
  #     minitablea50.cloglog.ADD()
  #   } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
  #     req(minitablea50.cloglog.INT())
  #     minitablea50.cloglog.INT()
  #   }
  # })
  # output$download_minitableselectedmodelA50 <-
  #   download_data_format_xlsx(nom_output = ({
  #     req(selectedmodelA50())
  #     if (selectedmodelA50() == "Âge (lien logit)") {
  #       paste0("minitablea50.logit.L")
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
  #       paste0("minitablea50.logit.ADD")
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
  #       paste0("minitablea50.logit.INT")
  #     } else if (selectedmodelA50() == "Âge (lien probit)") {
  #       paste0("minitablea50.probit.L")
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
  #       paste0("minitablea50.probit.ADD")
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
  #       paste0("minitablea50.probit.INT")
  #     } else if (selectedmodelA50() == "Âge (lien cloglog)") {
  #       paste0("minitablea50.cloglog.L")
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
  #       paste0("minitablea50.cloglog.ADD")
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
  #       paste0("minitablea50.cloglog.INT")
  #     }
  #   }),
  #   data = ({
  #     req(selectedmodelA50())
  #     if (selectedmodelA50() == "Âge (lien logit)") {
  #       req(minitablea50.logit.L())
  #       minitablea50.logit.L()
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
  #       req(minitablea50.logit.ADD())
  #       minitablea50.logit.ADD()
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
  #       req(minitablea50.logit.INT())
  #       minitablea50.logit.INT()
  #     } else if (selectedmodelA50() == "Âge (lien probit)") {
  #       req(minitablea50.probit.L())
  #       minitablea50.probit.L()
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
  #       req(minitablea50.probit.ADD())
  #       minitablea50.probit.ADD()
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
  #       req(minitablea50.probit.INT())
  #       minitablea50.probit.INT()
  #     } else if (selectedmodelA50() == "Âge (lien cloglog)") {
  #       req(minitablea50.cloglog.L())
  #       minitablea50.cloglog.L()
  #     } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
  #       req(minitablea50.cloglog.ADD())
  #       minitablea50.cloglog.ADD()
  #     } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
  #       req(minitablea50.cloglog.INT())
  #       minitablea50.cloglog.INT()
  #     }
  #   }))
  # # selected A50 plot -------------------------------------------------------
  # output$selectedmodelA50plot <- renderPlot({
  #   req(selectedmodelA50(), df_maturiteage())
  # 
  #   model_info <- list(
  #     "Âge (lien logit)" = list(model = AGEmaturite.model.logit.L, minitable = minitablea50.logit.L, type = "L"),
  #     "Âge + Sexe (lien logit)" = list(model = AGEmaturite.model.logit.ADD, minitable = minitablea50.logit.ADD, type = "ADD"),
  #     "Âge + Sexe + Âge:Sexe (lien logit)" = list(model = AGEmaturite.model.logit.INT, minitable = minitablea50.logit.INT, type = "INT"),
  #     "Âge (lien probit)" = list(model = AGEmaturite.model.probit.L, minitable = minitablea50.probit.L, type = "L"),
  #     "Âge + Sexe (lien probit)" = list(model = AGEmaturite.model.probit.ADD, minitable = minitablea50.probit.ADD, type = "ADD"),
  #     "Âge + Sexe + Âge:Sexe (lien probit)" = list(model = AGEmaturite.model.probit.INT, minitable = minitablea50.probit.INT, type = "INT"),
  #     "Âge (lien cloglog)" = list(model = AGEmaturite.model.cloglog.L, minitable = minitablea50.cloglog.L, type = "L"),
  #     "Âge + Sexe (lien cloglog)" = list(model = AGEmaturite.model.cloglog.ADD, minitable = minitablea50.cloglog.ADD, type = "ADD"),
  #     "Âge + Sexe + Âge:Sexe (lien cloglog)" = list(model = AGEmaturite.model.cloglog.INT, minitable = minitablea50.cloglog.INT, type = "INT")
  #   )
  # 
  #   selected_info <- model_info[[selectedmodelA50()]]
  # 
  #   req(selected_info$model(), selected_info$minitable())
  # 
  #   generate_ggmodel_A50(df = df_maturiteage(),
  #                        model = selected_info$model(),
  #                        minitable = selected_info$minitable(),
  #                        model_type = selected_info$type)
  # }, res = 96)
  # 
  # output$download_selectedmodelA50plot <- downloadHandler(
  #   filename = function() {
  #     req(selectedmodelA50())
  # 
  #     file_names <- list(
  #       "Âge (lien logit)" = "ggAGEmaturite_logit.L.png",
  #       "Âge + Sexe (lien logit)" = "ggAGEmaturite_logit.ADD.png",
  #       "Âge + Sexe + Âge:Sexe (lien logit)" = "ggAGEmaturite_logit.INT.png",
  #       "Âge (lien probit)" = "ggAGEmaturite_probit.L.png",
  #       "Âge + Sexe (lien probit)" = "ggAGEmaturite_probit.ADD.png",
  #       "Âge + Sexe + Âge:Sexe (lien probit)" = "ggAGEmaturite_probit.INT.png",
  #       "Âge (lien cloglog)" = "ggAGEmaturite_cloglog.L.png",
  #       "Âge + Sexe (lien cloglog)" = "ggAGEmaturite_cloglog.ADD.png",
  #       "Âge + Sexe + Âge:Sexe (lien cloglog)" = "ggAGEmaturite_cloglog.INT.png"
  #     )
  # 
  #     file_names[[selectedmodelA50()]]
  #   },
  # 
  #   content = function(file) {
  #     req(selectedmodelA50(), df_maturiteage())
  # 
  #     model_info <- list(
  #       "Âge (lien logit)" = list(model = AGEmaturite.model.logit.L, minitable = minitablea50.logit.L, type = "L"),
  #       "Âge + Sexe (lien logit)" = list(model = AGEmaturite.model.logit.ADD, minitable = minitablea50.logit.ADD, type = "ADD"),
  #       "Âge + Sexe + Âge:Sexe (lien logit)" = list(model = AGEmaturite.model.logit.INT, minitable = minitablea50.logit.INT, type = "INT"),
  #       "Âge (lien probit)" = list(model = AGEmaturite.model.probit.L, minitable = minitablea50.probit.L, type = "L"),
  #       "Âge + Sexe (lien probit)" = list(model = AGEmaturite.model.probit.ADD, minitable = minitablea50.probit.ADD, type = "ADD"),
  #       "Âge + Sexe + Âge:Sexe (lien probit)" = list(model = AGEmaturite.model.probit.INT, minitable = minitablea50.probit.INT, type = "INT"),
  #       "Âge (lien cloglog)" = list(model = AGEmaturite.model.cloglog.L, minitable = minitablea50.cloglog.L, type = "L"),
  #       "Âge + Sexe (lien cloglog)" = list(model = AGEmaturite.model.cloglog.ADD, minitable = minitablea50.cloglog.ADD, type = "ADD"),
  #       "Âge + Sexe + Âge:Sexe (lien cloglog)" = list(model = AGEmaturite.model.cloglog.INT, minitable = minitablea50.cloglog.INT, type = "INT")
  #     )
  # 
  #     selected_info <- model_info[[selectedmodelA50()]]
  # 
  #     req(selected_info$model(), selected_info$minitable())
  # 
  #     ggsave(
  #       file,
  #       plot = generate_ggmodel_A50(
  #         df = df_maturiteage(),
  #         model = selected_info$model(),
  #         minitable = selected_info$minitable(),
  #         model_type = selected_info$type
  #       ),
  #       device = "png"
  #     )
  #   }
  # )

}