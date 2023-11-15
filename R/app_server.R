app_server <- function(input, output, session) {
  # voir_exemple -----------------------------------------------------------
  uploadexampleServer("uploadexample1") #dans uploadexample.R
  # Filter ID --------------------------------------------------------------
  data_temp <-
    eventReactive(input$upload, {
      load_lac(path = input$upload$datapath,
               namesheet = "Lac")
    })
  output$no_lac <- renderUI({
    req(data_temp())
    typ_pech <- unique(data_temp()$typ_pech)
    radioButtons(
      inputId = "typ_pech",
      label = "Sélectionner le type de pêche normalisée",
      choices = typ_pech,
      selected = character(0)
    )
  })
  df_filtered1 <- reactive({
    req(input$typ_pech)
    filter(data_temp(), typ_pech %in% input$typ_pech) %>% droplevels()
  })
  output$typ_pech <- renderUI({
    req(df_filtered1())
    no_lac <- unique(df_filtered1()$no_lac)
    selectInput(
      inputId = "no_lac",
      label = "Sélectionner le numéro de lac",
      choices = no_lac,
      selected = NULL
    )
  })
  df_filtered2 <- reactive({
    req(input$no_lac)
    filter(df_filtered1(), no_lac %in% input$no_lac) %>% droplevels()
  })
  output$annee <- renderUI({
    req(df_filtered2())
    annee <- unique(df_filtered2()$annee)
    checkboxGroupInput(inputId = "annee",
                       label = "Sélectionner les années à considérer dans l'inventaire",
                       choices = sort(annee))
  })
  df_filtered3 <- reactive({
    req(input$annee)
    filter(df_filtered2(), annee %in% input$annee)
  })
  output$recap_intro_table <- renderTable({
    req(df_filtered3(), capture())
    table_recap(datalac = df_filtered3(), capture = capture())
  })
  output$visualiser <- renderUI({
    req(df_filtered3()) #pas necessaire dans les calculs en soit, mais sinon le menu deroulant apparait direct au debut plutot que juste au moment opportun
    selectInput(
      inputId = "controller",
      label = "Visualiser les données brutes",
      choices = c(
        "Lac" = "lac",
        "Stations" = "station",
        "Récolte" = "recolte",
        "Spécimens" = "specimen",
        "Profil" = "profil",
        "Paramètres" = "parametres"
      ),
      selected = NULL,
      multiple = FALSE
    )
  })
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher",
                      selected = input$controller)
  })
  # Display brut -----------------------------------------------------------
  data_lac <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_lac(path = input$upload$datapath, namesheet = "Lac") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  data_station <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_station(path = input$upload$datapath, namesheet = "Stations") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  data_recolte <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_recolte(path = input$upload$datapath, namesheet = "Recolte") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  data_specimen <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_specimen(path = input$upload$datapath, namesheet = "Specimens") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  data_profil <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_profil(path = input$upload$datapath, namesheet = "Profil") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  data_parametres <- reactive({
    req(input$upload, input$no_lac, input$typ_pech, input$annee)
    load_parametres(path = input$upload$datapath, namesheet = "Parametres") %>%
      filter(no_lac %in% input$no_lac,
             typ_pech %in% input$typ_pech,
             annee %in% input$annee) %>% droplevels()
  })
  output$table_lac <-
    renderDataTable(data_lac(), options = brut_options) #brut_options dans utils.R
  output$table_station <-
    renderDataTable(data_station(), options = brut_options)
  output$table_recolte <-
    renderDataTable(data_recolte(), options = brut_options)
  output$table_specimen <-
    renderDataTable(data_specimen(), options = brut_options)
  output$table_profil <-
    renderDataTable(data_profil(), options = brut_options)
  output$table_parametres <-
    renderDataTable(data_parametres(), options = brut_options)
  # identifier sp d'interet ------------------------------------------------
  sp_pen <- reactive({
    req(input$typ_pech)
    if (input$typ_pech == "PENT") {
      "SANA"
    } else if (input$typ_pech == "PENOF") {
      "SAFO"
    } else if (input$typ_pech == "PENDJ") {
      "SAVI"
    } else {
      NULL
    }
  })
  output$sp_queentexte <- renderText({
    req(sp_pen())
    if (sp_pen() == "SANA") {
      paste0("Touladi")
    } else if (sp_pen() == "SAFO") {
      paste0("Omble de fontaine")
    } else if (sp_pen() == "SAVI") {
      paste0("Doré jaune")
    } else {
      NULL
    }
  })
  
  output$sp_queentextelatin <- renderText({
    req(sp_pen())
    if (sp_pen() == "SANA") {
      paste0("Salvelinus namaycush")
    } else if (sp_pen() == "SAFO") {
      paste0("Salvelinus fontinalis")
    } else if (sp_pen() == "SAVI") {
      paste0("Sander vitreus")
    } else {
      NULL
    }
  })
    
  output$specimen_verif <- renderDataTable({
    req(specimen())
    specimen() %>% as.data.frame()
  })
  
  # Creation du df specimen ------------------------------------------------
  specimen <- reactive({
    req(data_specimen(), data_station())
    inner_join(
      x = data_specimen(),
      y = data_station(),
      by = c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"),
      relationship = "many-to-many"
    ) %>% droplevels()  %>% dplyr::distinct()
  })
  # Creation du df capture -------------------------------------------------
  capture <- reactive({
    req(data_recolte(), data_station())
    inner_join(
      x = data_recolte(),
      y = data_station(),
      by = c("no_station", "annee", "nom_lac", "no_lac", "typ_pech"),
      relationship = "many-to-many"
    ) %>% droplevels()  %>% dplyr::distinct()
  })
  
  output$capture_verif <- renderDataTable({
    req(capture())
    capture() %>% as.data.frame()
  })
  
  # Taille masse age -------------------------------------------------------
  taillemasseagedata <- reactive({
    req(specimen(), sp_pen())
    taille_masse_age(dataspecimen = specimen(), espece = sp_pen()) %>% as.data.frame()
  })
  output$taillemasseagetable <-  function() {
    kable_ltmpoidsage(data = taillemasseagedata())
  }
  output$download_taillemasseagetable <-
    download_data_format_xlsx(givenname = "taille_masse_age_table", datadown = taillemasseagedata())
  # Structure taille ggplot ------------------------------------------------
  output$structuretailleplot <- renderPlot({
    req(specimen(), sp_pen())
    structure_taille(
      data = specimen(),
      espece = sp_pen(),
      regroupement = input$groupetailleplot
    )
  }, res = 96)
  output$titrestructuretailleplot <-
    renderText("Histogramme de fréquence des longueurs")
  output$download_groupetailleplot <- downloadHandler(
    filename = function() {
      paste("groupetailleplot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen(), sp_pen())
        structure_taille(
          data = specimen(),
          espece = sp_pen(),
          regroupement = input$groupetailleplot
        )
      } , device = "png")
    }
  )
  # Structure age ggplot ------------------------------------------------
  output$structureageplot <- renderPlot({
    req(specimen(), sp_pen())
    structure_age(
      data = specimen(),
      espece = sp_pen(),
      regroupement = input$groupeageplot
    )
  }, res = 96)
  output$titrestructureageplot <-
    renderText("Histogramme de fréquence d’âge")
  output$download_groupeageplot <- downloadHandler(
    filename = function() {
      paste("groupeageplot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen(), sp_pen())
        structure_age(
          data = specimen(),
          espece = sp_pen(),
          regroupement = input$groupeageplot
        )
      } , device = "png")
    }
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
    temp <- selection_modele_CPUE_tous(
        capture = capture(),
        specimen = specimen(),
        espece = sp_pen(),
        station = data_station()
      ) %>% as.data.frame()
    temp <- temp %>% rename('IC 95%' = IC95,
                            "Ajustement (résultat du test HNP)" = Ajustement)
    temp
  })
  output$selection_modele_CPUE_toustable <-  function() {
    kable_CPUEtous(data = selection_modele_CPUE_tous_data())
  }
  output$download_selection_modele_CPUE_toustable <-
    download_data_format_xlsx(givenname = "selection_modele_CPUE_tous_data", datadown = selection_modele_CPUE_tous_data())
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
    temp <-
      selection_modele_CPUE_Fmature(
        capture = capture(),
        specimen = specimen(),
        espece = sp_pen(),
        station = data_station()
      ) %>% as.data.frame()
    temp <- temp %>% rename('IC 95%' = IC95,
                            "Ajustement (résultat du test HNP)" = Ajustement)
    temp
  })
  output$selection_modele_CPUE_Fmaturetable <-  function() {
    kable_CPUEFmature(data = selection_modele_CPUE_Fmature_data())
  }
  output$download_selection_modele_CPUE_Fmaturetable <-
    download_data_format_xlsx(givenname = "selection_modele_CPUE_Fmature_data", datadown = selection_modele_CPUE_Fmature_data())
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
      capture(),
      CPUE_tous(),
      CPUEic_tous(),
      CPUE_Fmature(),
      CPUEic_Fmature()
    )
    temp <-
      abondance_table(capture = capture(),
                      specimen = specimen(),
                      espece = sp_pen()) %>% as.data.frame()
    temp$CPUE[temp$Groupe == "Tous"] <- CPUE_tous()
    temp$IC95[temp$Groupe == "Tous"] <- CPUEic_tous()
    temp$CPUE[temp$Groupe == "Repro. actifs ♀"] <- CPUE_Fmature()
    temp$IC95[temp$Groupe == "Repro. actifs ♀"] <- CPUEic_Fmature()
    temp$'Prop. (%)' <-
      format(round(as.numeric(temp$'Prop. (%)'), digits = 0), nsmall = 0)
    temp <- temp %>% mutate(CPUE = ifelse(is.na(CPUE), "-", CPUE))
    temp <- temp %>% mutate(IC95 = ifelse(is.na(IC95), "-", IC95))
    temp <-
      temp %>% mutate(ratioMF = ifelse(is.na(ratioMF), "-", ratioMF))
    temp <- temp %>% rename('IC 95%' = IC95,
                            "Ratio ♂:♀" = ratioMF) %>% as.data.frame()
    temp
  })
  output$abondance1table <-  function() {
    kable_abondance(data = abondance1())
  }
  output$download_abondance1 <-
    download_data_format_xlsx(givenname = "abondance1", datadown = abondance1())
  # BPUE ------------------------------------------------
  biomasse1 <- reactive({
    req(specimen(), sp_pen(), capture())
    temp <-
      biomasse_table(capture = capture(),
                     specimen = specimen(),
                     espece = sp_pen())
    temp <- temp %>% mutate(IC95 = ifelse(is.na(IC95), "-", IC95))
    temp <- temp %>% rename(
      "Biomasse totale (kg) " = Biomasse,
      "Prop. (%)" = Perc,
      "IC 95%" = IC95
    )
    temp
  })
  output$biomasse1table <-  function() {
    kable_biomasse(data = biomasse1())
  }
  output$download_biomasse1 <-
    download_data_format_xlsx(givenname = "biomasse1", datadown = biomasse1())
  # PSD -------------------------------------------------------
  psd1 <- reactive({
    req(specimen(), sp_pen())
    psd_indice(data = specimen(), sp = sp_pen()) %>% as.data.frame()
  })
  output$psd1_table <-  function() {
    kable_psd1(data = psd1())
  }
  output$download_psd1 <-
    download_data_format_xlsx(givenname = "psd_indice", datadown = psd1())
  psd2 <- reactive({
    req(specimen(), sp_pen())
    psd_byclass(data = specimen(), sp = sp_pen()) %>% as.data.frame()
  })
  output$psd2_table <-  function() {
    kable_psd2(data = psd2())
  }
  output$download_psd2 <-
    download_data_format_xlsx(givenname = "psd_byclass", datadown = psd2())
  output$psd1plot <- renderPlot({
    req(psd2())
    psd_plot(data = psd2())
  }, res = 96)
  output$titrepsd1plot <- renderText("Distribution de fréquence de longueurs avec les classes de PSD")
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
  output$masselongueur_plot <- plotly::renderPlotly({
    req(specimen(), sp_pen())
    relation_masse_longueur(data = specimen(), espece = sp_pen()) %>%
      plotly::ggplotly(tooltip = "text")
  })
  output$titregraph_relmasselongueur <-
    renderText("Relation masse-longueur")
  output$download_masselongueur_plot <- downloadHandler(
    filename = function() {
      paste("relation_masse_longueur", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen(), sp_pen())
        relation_masse_longueur(data = specimen(), espece = sp_pen())
      } , device = "png")
    }
  )
  # Indice de condition -------------------------------------------------------
  wri1data <- reactive({
    req(specimen(), sp_pen())
    table_wri(data = specimen(), espece = sp_pen()) %>% as.data.frame()
  })
  output$wri1_table <-  function() {
    kable_wri(data = wri1data())
  }
  output$download_wri1 <-
    download_data_format_xlsx(givenname = "table_wri", datadown = wri1data())
  ## Wri tous ggplot ------------------------------------------------
  output$wri2plot <- renderPlot({
    req(specimen(), sp_pen())
    fig_wri_tous(data = specimen(), espece = sp_pen())
  }, res = 96)
  output$titrewri2plot <- renderText("titre de la figure TBD")
  output$download_wri2plot <- downloadHandler(
    filename = function() {
      paste("wri_tous", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen(), sp_pen())
        fig_wri_tous(data = specimen(), espece = sp_pen())
      } , device = "png")
    }
  )
  ## Wri byclass ggplot ------------------------------------------------
  output$wri3plot <- renderPlot({
    req(specimen(), sp_pen())
    fig_wri_byclass(data = specimen(), espece = sp_pen())
  }, res = 96)
  output$titrewri3plot <- renderText("titre de la figure TBD")
  output$download_wri3plot <- downloadHandler(
    filename = function() {
      paste("wri_byclass", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        req(specimen(), sp_pen())
        fig_wri_byclass(data = specimen(), espece = sp_pen())
      } , device = "png")
    }
  )
  # Croissance ------------------------------------------------
  initcroissance <- reactive({
    #selectionner slmt le data necessaire
    req(specimen(), sp_pen())
    x <-
      specimen() %>% filter(sp == sp_pen()) #prendre slmt l'sp PEN
    x <-
      subset(x, !is.na(ltm)) #removing all records where LTM mesures were missing
    x <-
      subset(x, !is.na(age)) #removing all records where AGE mesures were missing
    x
  })
  croissance1 <- reactive({
    req(initcroissance())
    courbe_croissance_comparaison(data = initcroissance()) %>% as.data.frame()
  })
  output$croissance1_table <-
    renderReactable(
      reactable(
        croissance1(),
        selection = "single",
        onClick = "select",
        defaultSelected = 1
      )
    )
  output$titrecroissance1 <- renderText("titre de la figure TBD")
  selectedmodelcroissance <- reactive({
    selected <- getReactableState("croissance1_table", "selected")
    req(selected, croissance1())
    details <- croissance1()[selected, 1]
    paste(details)
  })
  output$table_stateCROISSANCE <-
    renderText(selectedmodelcroissance())
  output$download_croissance1 <-
    download_data_format_xlsx(givenname = "courbe_croissance_comparaison", datadown = croissance1())
  output$titreselectedmodelcroissanceplot <-
    renderText("titre de la figure TBD")
  output$selectedmodelcroissanceplot <- renderPlot({
    req(selectedmodelcroissance())
    if (selectedmodelcroissance() == "Von Bertalanffy") {
      req(initcroissance(), croissance1())
      courbe_croissance_ggVONBERT(initcroissance = initcroissance(), tablemodele = croissance1())
    } else if (selectedmodelcroissance() == "Gompertz") {
      req(initcroissance(), croissance1())
      courbe_croissance_ggGOMP(initcroissance = initcroissance(), tablemodele = croissance1())
    } else if (selectedmodelcroissance() == "Logistique") {
      req(initcroissance(), croissance1())
      courbe_croissance_ggLOGIST(initcroissance = initcroissance(), tablemodele = croissance1())
    }
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
        req(selectedmodelcroissance())
        if (selectedmodelcroissance() == "Von Bertalanffy") {
          req(initcroissance(), croissance1())
          courbe_croissance_ggVONBERT(initcroissance = initcroissance(),
                                      tablemodele = croissance1())
        } else if (selectedmodelcroissance() == "Gompertz") {
          req(initcroissance(), croissance1())
          courbe_croissance_ggGOMP(initcroissance = initcroissance(),
                                   tablemodele = croissance1())
        } else if (selectedmodelcroissance() == "Logistique") {
          req(initcroissance(), croissance1())
          courbe_croissance_ggLOGIST(initcroissance = initcroissance(),
                                     tablemodele = croissance1())
        }
      } , device = "png")
    }
  )
  # Mortalite -------------------------------------------------------
  deathdf <- reactive({
    req(specimen(), sp_pen())
    death(data = specimen(), espece = sp_pen()) %>% as.data.frame()
  })
  pp <- reactive({
    req(deathdf())
    peakplus(data = deathdf())
  })
  agemax_val <- reactive({
    req(deathdf())
    agemax(data = deathdf())
  })
  df_corr <- reactive({
    req(deathdf(), pp(), agemax_val())
    creation_df_CORR(data = deathdf(),
                     peakplus = pp(),
                     agemax = agemax_val()) %>% as.data.frame()
  })
  df_ext <- reactive({
    req(df_corr(), pp(), agemax_val())
    creation_df_EXT(data = df_corr(),
                    peakplus = pp(),
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
    req(pp(), agemax_val(), deathdf())
    mortalite_ChapmanRobson(data = deathdf(),
                            pp = pp(),
                            agemax_val = agemax_val()) %>% as.data.frame()
  })
  output$mortalite2_table <-  function() {
    kable_mortalite2(data = mortalite2())
  }
  output$download_mortalite1 <-
    download_data_format_xlsx(givenname = "mortalite_selection_modeles", datadown = mortalite1())
  output$download_mortalite2 <-
    download_data_format_xlsx(givenname = "mortalite_ChapmanRobson", datadown = mortalite2())
  zobs <- reactive({
    req(pp(), deathdf(), agemax_val())
    get_zobs(PP = pp(),
             death = deathdf(),
             agemax = agemax_val())
  })
  output$zobs_text <- renderText(zobs())
  # Maturite sexuelle -------------------------------------------------------
  df_maturite <- reactive({
    req(specimen(), sp_pen())
    x <- specimen() %>% filter(sp == sp_pen()) %>% droplevels()
    x <-
      x %>% filter(maturite != "IND") %>% droplevels() #filter les donnees pour retirer les specimens de Maturite IND
    x <-
      x %>% filter(sexe != "IND") %>% droplevels() #filter les donnees pour retirer les specimens de sexe IND
    x$maturite <-
      factor(x$maturite,
             levels = c("N", "O"),
             ordered = TRUE)
    x
  })
  output$df_maturitetable <-
    renderDataTable(df_maturite(),
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      searching = FALSE
                    ))
  
  output$titreL50_selection_modeles_table <-
    renderText(
      "Comparaison de modèles visant à schématiser proportion mature en fonction de la longueur TITRE TBD"
    )
  df_maturiteltm <- reactive({
    req(df_maturite())
    subset(df_maturite(), !is.na(ltm))# this data frame needed to be "cleaned" by removing all records where mesures were missing
  })
  output$df_maturitelongtable <-
    renderDataTable(df_maturiteltm(),
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      searching = FALSE
                    ))
  df_maturiteage <- reactive({
    req(df_maturite())
    subset(df_maturite(), !is.na(age))# this data frame needed to be "cleaned" by removing all records where mesures were missing
  })
  output$df_maturiteagetable <-
    renderDataTable(df_maturiteage(),
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      searching = FALSE
                    ))
  ## L50 -------------------------------------------------------
  LTMmaturite.model.logit.L <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm,
        family = binomial(link = "logit"),
        data = df_maturiteltm())
  })
  output$LTMmaturite.model.logit.L_table <-
    renderPrint(LTMmaturite.model.logit.L())
  LTMmaturite.model.probit.L <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm,
        family = binomial(link = "probit"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.cloglog.L <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm,
        family = binomial(link = "cloglog"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.logit.ADD <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm + sexe,
        family = binomial(link = "logit"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.probit.ADD <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm + sexe,
        family = binomial(link = "probit"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.cloglog.ADD <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm + sexe,
        family = binomial(link = "cloglog"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.logit.INT <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm * sexe,
        family = binomial(link = "logit"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.probit.INT <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm * sexe,
        family = binomial(link = "probit"),
        data = df_maturiteltm())
  })
  LTMmaturite.model.cloglog.INT <- reactive({
    req(df_maturiteltm())
    glm(maturite ~ ltm * sexe,
        family = binomial(link = "cloglog"),
        data = df_maturiteltm())
  })
  L50_selection_modeles_df <- reactive({
    req(
      LTMmaturite.model.logit.L(),
      LTMmaturite.model.probit.L(),
      LTMmaturite.model.cloglog.L(),
      LTMmaturite.model.logit.ADD(),
      LTMmaturite.model.probit.ADD(),
      LTMmaturite.model.cloglog.ADD(),
      LTMmaturite.model.logit.INT(),
      LTMmaturite.model.probit.INT(),
      LTMmaturite.model.cloglog.INT(),
      df_maturiteltm()
    )
    L50_selection_modeles(
      df = df_maturiteltm(),
      modlogit.l = LTMmaturite.model.logit.L(),
      modprobit.l = LTMmaturite.model.probit.L(),
      modcloglog.l = LTMmaturite.model.cloglog.L(),
      modlogit.add = LTMmaturite.model.logit.ADD(),
      modprobit.add = LTMmaturite.model.probit.ADD(),
      modcloglog.add = LTMmaturite.model.cloglog.ADD(),
      modlogit.int = LTMmaturite.model.logit.INT(),
      modprobit.int = LTMmaturite.model.probit.INT(),
      modcloglog.int = LTMmaturite.model.cloglog.INT()
    )
  })
  output$L50_selection_modeles_table <-
    renderReactable(
      reactable(
        L50_selection_modeles_df(),
        selection = "single",
        onClick = "select",
        fullWidth = TRUE,
        defaultSelected = 1
      )
    )
  output$download_L50_selection_modeles_table <-
    download_data_format_xlsx(givenname = "L50_selection_modeles_table", datadown = L50_selection_modeles_df())
  ## minitable l50 -----------------------------------------------------------
  minitablel50.logit.L <- reactive({
    req(LTMmaturite.model.logit.L())
    minitable_param_model_l_l50(model = LTMmaturite.model.logit.L()) %>% as.data.frame()
  })
  minitablel50.probit.L <- reactive({
    req(LTMmaturite.model.probit.L())
    minitable_param_model_l_l50(model = LTMmaturite.model.probit.L()) %>% as.data.frame()
  })
  minitablel50.cloglog.L <- reactive({
    req(LTMmaturite.model.cloglog.L())
    minitable_param_model_l_l50(model = LTMmaturite.model.cloglog.L()) %>% as.data.frame()
  })
  minitablel50.logit.ADD <- reactive({
    req(LTMmaturite.model.logit.ADD())
    minitable_param_model_add_l50(model = LTMmaturite.model.logit.ADD()) %>% as.data.frame()
  })
  minitablel50.probit.ADD <- reactive({
    req(LTMmaturite.model.probit.ADD())
    minitable_param_model_add_l50(model = LTMmaturite.model.probit.ADD()) %>% as.data.frame()
  })
  minitablel50.cloglog.ADD <- reactive({
    req(LTMmaturite.model.cloglog.ADD())
    minitable_param_model_add_l50(model = LTMmaturite.model.cloglog.ADD()) %>% as.data.frame()
  })
  minitablel50.logit.INT <- reactive({
    req(LTMmaturite.model.logit.INT(), df_maturiteltm())
    minitable_param_model_int_l50(model = LTMmaturite.model.logit.INT(),
                                  df = df_maturiteltm()) %>% as.data.frame()
  })
  minitablel50.probit.INT <- reactive({
    req(LTMmaturite.model.probit.INT(), df_maturiteltm())
    minitable_param_model_int_l50(model = LTMmaturite.model.probit.INT(),
                                  df = df_maturiteltm()) %>% as.data.frame()
  })
  minitablel50.cloglog.INT <- reactive({
    req(LTMmaturite.model.cloglog.INT(), df_maturiteltm())
    minitable_param_model_int_l50(model = LTMmaturite.model.cloglog.INT(),
                                  df = df_maturiteltm()) %>% as.data.frame()
  })
  ## selection modele L50 ----------------------------------------------------
  selectedmodelL50 <- reactive({
    selected <-
      getReactableState("L50_selection_modeles_table", "selected")
    req(selected, L50_selection_modeles_df())
    details <- L50_selection_modeles_df()[selected, 1]
    paste(details)
  })
  output$table_stateLTM <- renderText(selectedmodelL50())
  output$selectedmodelL50minitable <- renderTable({
    req(selectedmodelL50())
    if (selectedmodelL50() == "Longueur (lien logit)") {
      req(minitablel50.logit.L())
      minitablel50.logit.L()
    } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
      req(minitablel50.logit.ADD())
      minitablel50.logit.ADD()
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
      req(minitablel50.logit.INT())
      minitablel50.logit.INT()
    } else if (selectedmodelL50() == "Longueur (lien probit)") {
      req(minitablel50.probit.L())
      minitablel50.probit.L()
    } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
      req(minitablel50.probit.ADD())
      minitablel50.probit.ADD()
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
      req(minitablel50.probit.INT())
      minitablel50.probit.INT()
    } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
      req(minitablel50.cloglog.L())
      minitablel50.cloglog.L()
    } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
      req(minitablel50.cloglog.ADD())
      minitablel50.cloglog.ADD()
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
      req(minitablel50.cloglog.INT())
      minitablel50.cloglog.INT()
    }
  })
  output$download_minitableselectedmodelL50 <-
    download_data_format_xlsx(givenname = ({
      req(selectedmodelL50())
      if (selectedmodelL50() == "Longueur (lien logit)") {
        paste0("minitablel50.logit.L")
      } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
        paste0("minitablel50.logit.ADD")
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
        paste0("minitablel50.logit.INT")
      } else if (selectedmodelL50() == "Longueur (lien probit)") {
        paste0("minitablel50.probit.L")
      } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
        paste0("minitablel50.probit.ADD")
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
        paste0("minitablel50.probit.INT")
      } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
        paste0("minitablel50.cloglog.L")
      } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
        paste0("minitablel50.cloglog.ADD")
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
        paste0("minitablel50.cloglog.INT")
      }
    }),
    datadown = ({
      req(selectedmodelL50())
      if (selectedmodelL50() == "Longueur (lien logit)") {
        req(minitablel50.logit.L())
        minitablel50.logit.L()
      } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
        req(minitablel50.logit.ADD())
        minitablel50.logit.ADD()
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
        req(minitablel50.logit.INT())
        minitablel50.logit.INT()
      } else if (selectedmodelL50() == "Longueur (lien probit)") {
        req(minitablel50.probit.L())
        minitablel50.probit.L()
      } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
        req(minitablel50.probit.ADD())
        minitablel50.probit.ADD()
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
        req(minitablel50.probit.INT())
        minitablel50.probit.INT()
      } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
        req(minitablel50.cloglog.L())
        minitablel50.cloglog.L()
      } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
        req(minitablel50.cloglog.ADD())
        minitablel50.cloglog.ADD()
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
        req(minitablel50.cloglog.INT())
        minitablel50.cloglog.INT()
      }
    }))
  ## selected L50 plot -------------------------------------------------------
  output$selectedmodelL50plot <- renderPlot({
    req(selectedmodelL50())
    if (selectedmodelL50() == "Longueur (lien logit)") {
      req(df_maturiteltm(),
          LTMmaturite.model.logit.L(),
          minitablel50.logit.L())
      ggmodel_L_L50(df = df_maturiteltm(),
                    model = LTMmaturite.model.logit.L(),
                    minitable = minitablel50.logit.L())
    } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
      req(df_maturiteltm(),
          LTMmaturite.model.logit.ADD(),
          minitablel50.logit.ADD())
      ggmodel_ADD_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.logit.ADD(),
                      minitable = minitablel50.logit.ADD())
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
      req(df_maturiteltm(),
          LTMmaturite.model.logit.INT(),
          minitablel50.logit.INT())
      ggmodel_INT_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.logit.INT(),
                      minitable = minitablel50.logit.INT())
    } else if (selectedmodelL50() == "Longueur (lien probit)") {
      req(df_maturiteltm(),
          LTMmaturite.model.probit.L(),
          minitablel50.probit.L())
      ggmodel_L_L50(df = df_maturiteltm(),
                    model = LTMmaturite.model.probit.L(),
                    minitable = minitablel50.probit.L())
    } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
      req(
        df_maturiteltm(),
        LTMmaturite.model.probit.ADD(),
        minitablel50.probit.ADD()
      )
      ggmodel_ADD_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.probit.ADD(),
                      minitable = minitablel50.probit.ADD())
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
      req(
        df_maturiteltm(),
        LTMmaturite.model.probit.INT(),
        minitablel50.probit.INT()
      )
      ggmodel_INT_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.probit.INT(),
                      minitable = minitablel50.probit.INT())
    } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
      req(df_maturiteltm(),
          LTMmaturite.model.cloglog.L(),
          minitablel50.cloglog.L())
      ggmodel_L_L50(df = df_maturiteltm(),
                    model = LTMmaturite.model.cloglog.L(),
                    minitable = minitablel50.cloglog.L())
    } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
      req(
        df_maturiteltm(),
        LTMmaturite.model.cloglog.ADD(),
        minitablel50.cloglog.ADD()
      )
      ggmodel_ADD_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.cloglog.ADD(),
                      minitable = minitablel50.cloglog.ADD())
    } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
      req(
        df_maturiteltm(),
        LTMmaturite.model.cloglog.INT(),
        minitablel50.cloglog.INT()
      )
      ggmodel_INT_L50(df = df_maturiteltm(),
                      model = LTMmaturite.model.cloglog.INT(),
                      minitable = minitablel50.cloglog.INT())
    }
  }, res = 96)
  ## download_selectedmodelL50plot -------------------------------------------
  output$download_selectedmodelL50plot <- downloadHandler(
    filename = function() {
      req(selectedmodelL50())
      if (selectedmodelL50() == "Longueur (lien logit)") {
        paste("ggLTMmaturite_logit.L", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
        paste("ggLTMmaturite_logit.ADD", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
        paste("ggLTMmaturite_logit.INT", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur (lien probit)") {
        paste("ggLTMmaturite_probit.L", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
        paste("ggLTMmaturite_probit.ADD", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
        paste("ggLTMmaturite_probit.INT", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
        paste("ggLTMmaturite_cloglog.L", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
        paste("ggLTMmaturite_cloglog.ADD", '.png', sep = '')
      } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
        paste("ggLTMmaturite_cloglog.INT", '.png', sep = '')
      }
    },
    content = function(file) {
      ggsave(file, plot = {
        req(selectedmodelL50())
        if (selectedmodelL50() == "Longueur (lien logit)") {
          req(df_maturiteltm(),
              LTMmaturite.model.logit.L(),
              minitablel50.logit.L())
          ggmodel_L_L50(df = df_maturiteltm(),
                        model = LTMmaturite.model.logit.L(),
                        minitable = minitablel50.logit.L())
        } else if (selectedmodelL50() == "Longueur + Sexe (lien logit)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.logit.ADD(),
            minitablel50.logit.ADD()
          )
          ggmodel_ADD_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.logit.ADD(),
                          minitable = minitablel50.logit.ADD())
        } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien logit)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.logit.INT(),
            minitablel50.logit.INT()
          )
          ggmodel_INT_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.logit.INT(),
                          minitable = minitablel50.logit.INT())
        } else if (selectedmodelL50() == "Longueur (lien probit)") {
          req(df_maturiteltm(),
              LTMmaturite.model.probit.L(),
              minitablel50.probit.L())
          ggmodel_L_L50(df = df_maturiteltm(),
                        model = LTMmaturite.model.probit.L(),
                        minitable = minitablel50.probit.L())
        } else if (selectedmodelL50() == "Longueur + Sexe (lien probit)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.probit.ADD(),
            minitablel50.probit.ADD()
          )
          ggmodel_ADD_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.probit.ADD(),
                          minitable = minitablel50.probit.ADD())
        } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien probit)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.probit.INT(),
            minitablel50.probit.INT()
          )
          ggmodel_INT_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.probit.INT(),
                          minitable = minitablel50.probit.INT())
        } else if (selectedmodelL50() == "Longueur (lien cloglog)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.cloglog.L(),
            minitablel50.cloglog.L()
          )
          ggmodel_L_L50(df = df_maturiteltm(),
                        model = LTMmaturite.model.cloglog.L(),
                        minitable = minitablel50.cloglog.L())
        } else if (selectedmodelL50() == "Longueur + Sexe (lien cloglog)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.cloglog.ADD(),
            minitablel50.cloglog.ADD()
          )
          ggmodel_ADD_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.cloglog.ADD(),
                          minitable = minitablel50.cloglog.ADD())
        } else if (selectedmodelL50() == "Longueur + Sexe + Longueur:Sexe (lien cloglog)") {
          req(
            df_maturiteltm(),
            LTMmaturite.model.cloglog.INT(),
            minitablel50.cloglog.INT()
          )
          ggmodel_INT_L50(df = df_maturiteltm(),
                          model = LTMmaturite.model.cloglog.INT(),
                          minitable = minitablel50.cloglog.INT())
        }
      } , device = "png")
    }
  )
  output$titreselectedmodelL50minitable <-
    renderText("Présentation des parametres de la courbe TITRE TBD")
  output$titreselectedmodelL50plot <-
    renderText("L50 graphique titre TBD")
  ## A50 -------------------------------------------------------
  AGEmaturite.model.logit.L <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age,
        family = binomial(link = "logit"),
        data = df_maturiteage())
  })
  output$AGEmaturite.model.logit.L_table <-
    renderPrint(AGEmaturite.model.logit.L())
  AGEmaturite.model.probit.L <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age,
        family = binomial(link = "probit"),
        data = df_maturiteage())
  })
  AGEmaturite.model.cloglog.L <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age,
        family = binomial(link = "cloglog"),
        data = df_maturiteage())
  })
  AGEmaturite.model.logit.ADD <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age + sexe,
        family = binomial(link = "logit"),
        data = df_maturiteage())
  })
  AGEmaturite.model.probit.ADD <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age + sexe,
        family = binomial(link = "probit"),
        data = df_maturiteage())
  })
  AGEmaturite.model.cloglog.ADD <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age + sexe,
        family = binomial(link = "cloglog"),
        data = df_maturiteage())
  })
  AGEmaturite.model.logit.INT <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age * sexe,
        family = binomial(link = "logit"),
        data = df_maturiteage())
  })
  AGEmaturite.model.probit.INT <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age * sexe,
        family = binomial(link = "probit"),
        data = df_maturiteage())
  })
  AGEmaturite.model.cloglog.INT <- reactive({
    req(df_maturiteage())
    glm(maturite ~ age * sexe,
        family = binomial(link = "cloglog"),
        data = df_maturiteage())
  })
  A50_selection_modeles_df <- reactive({
    req(
      AGEmaturite.model.logit.L(),
      AGEmaturite.model.probit.L(),
      AGEmaturite.model.cloglog.L(),
      AGEmaturite.model.logit.ADD(),
      AGEmaturite.model.probit.ADD(),
      AGEmaturite.model.cloglog.ADD(),
      AGEmaturite.model.logit.INT(),
      AGEmaturite.model.probit.INT(),
      AGEmaturite.model.cloglog.INT(),
      df_maturiteage()
    )
    A50_selection_modeles(
      df = df_maturiteage(),
      modlogit.l = AGEmaturite.model.logit.L(),
      modprobit.l = AGEmaturite.model.probit.L(),
      modcloglog.l = AGEmaturite.model.cloglog.L(),
      modlogit.add = AGEmaturite.model.logit.ADD(),
      modprobit.add = AGEmaturite.model.probit.ADD(),
      modcloglog.add = AGEmaturite.model.cloglog.ADD(),
      modlogit.int = AGEmaturite.model.logit.INT(),
      modprobit.int = AGEmaturite.model.probit.INT(),
      modcloglog.int = AGEmaturite.model.cloglog.INT()
    )
  })
  output$A50_selection_modeles_table <-
    renderReactable(
      reactable(
        A50_selection_modeles_df(),
        selection = "single",
        onClick = "select",
        width = "auto",
        defaultSelected = 1
      )
    )
  output$download_A50_selection_modeles_table <-
    download_data_format_xlsx(givenname = "A50_selection_modeles_table", datadown = A50_selection_modeles_df())
  ## minitable a50 -----------------------------------------------------------
  minitablea50.logit.L <- reactive({
    req(AGEmaturite.model.logit.L())
    minitable_param_model_l_a50(model = AGEmaturite.model.logit.L()) %>% as.data.frame()
  })
  minitablea50.probit.L <- reactive({
    req(AGEmaturite.model.probit.L())
    minitable_param_model_l_a50(model = AGEmaturite.model.probit.L()) %>% as.data.frame()
  })
  minitablea50.cloglog.L <- reactive({
    req(AGEmaturite.model.cloglog.L())
    minitable_param_model_l_a50(model = AGEmaturite.model.cloglog.L()) %>% as.data.frame()
  })
  minitablea50.logit.ADD <- reactive({
    req(AGEmaturite.model.logit.ADD())
    minitable_param_model_add_a50(model = AGEmaturite.model.logit.ADD()) %>% as.data.frame()
  })
  minitablea50.probit.ADD <- reactive({
    req(AGEmaturite.model.probit.ADD())
    minitable_param_model_add_a50(model = AGEmaturite.model.probit.ADD()) %>% as.data.frame()
  })
  minitablea50.cloglog.ADD <- reactive({
    req(AGEmaturite.model.cloglog.ADD())
    minitable_param_model_add_a50(model = AGEmaturite.model.cloglog.ADD()) %>% as.data.frame()
  })
  minitablea50.logit.INT <- reactive({
    req(AGEmaturite.model.logit.INT(), df_maturiteage())
    minitable_param_model_int_a50(model = AGEmaturite.model.logit.INT(),
                                  df = df_maturiteage()) %>% as.data.frame()
  })
  minitablea50.probit.INT <- reactive({
    req(AGEmaturite.model.probit.INT(), df_maturiteage())
    minitable_param_model_int_a50(model = AGEmaturite.model.probit.INT(),
                                  df = df_maturiteage()) %>% as.data.frame()
  })
  minitablea50.cloglog.INT <- reactive({
    req(AGEmaturite.model.cloglog.INT(), df_maturiteage())
    minitable_param_model_int_a50(model = AGEmaturite.model.cloglog.INT(),
                                  df = df_maturiteage()) %>% as.data.frame()
  })
  ## selection modele A50 ----------------------------------------------------
  selectedmodelA50 <- reactive({
    selected <-
      getReactableState("A50_selection_modeles_table", "selected")
    req(selected, A50_selection_modeles_df())
    details <- A50_selection_modeles_df()[selected, 1]
    paste(details)
  })
  output$table_stateAGE <- renderText(selectedmodelA50())
  output$selectedmodelA50minitable <- renderTable({
    req(selectedmodelA50())
    if (selectedmodelA50() == "Âge (lien logit)") {
      req(minitablea50.logit.L())
      minitablea50.logit.L()
    } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
      req(minitablea50.logit.ADD())
      minitablea50.logit.ADD()
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
      req(minitablea50.logit.INT())
      minitablea50.logit.INT()
    } else if (selectedmodelA50() == "Âge (lien probit)") {
      req(minitablea50.probit.L())
      minitablea50.probit.L()
    } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
      req(minitablea50.probit.ADD())
      minitablea50.probit.ADD()
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
      req(minitablea50.probit.INT())
      minitablea50.probit.INT()
    } else if (selectedmodelA50() == "Âge (lien cloglog)") {
      req(minitablea50.cloglog.L())
      minitablea50.cloglog.L()
    } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
      req(minitablea50.cloglog.ADD())
      minitablea50.cloglog.ADD()
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
      req(minitablea50.cloglog.INT())
      minitablea50.cloglog.INT()
    }
  })
  output$download_minitableselectedmodelA50 <-
    download_data_format_xlsx(givenname = ({
      req(selectedmodelA50())
      if (selectedmodelA50() == "Âge (lien logit)") {
        paste0("minitablea50.logit.L")
      } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
        paste0("minitablea50.logit.ADD")
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
        paste0("minitablea50.logit.INT")
      } else if (selectedmodelA50() == "Âge (lien probit)") {
        paste0("minitablea50.probit.L")
      } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
        paste0("minitablea50.probit.ADD")
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
        paste0("minitablea50.probit.INT")
      } else if (selectedmodelA50() == "Âge (lien cloglog)") {
        paste0("minitablea50.cloglog.L")
      } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
        paste0("minitablea50.cloglog.ADD")
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
        paste0("minitablea50.cloglog.INT")
      }
    }),
    datadown = ({
      req(selectedmodelA50())
      if (selectedmodelA50() == "Âge (lien logit)") {
        req(minitablea50.logit.L())
        minitablea50.logit.L()
      } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
        req(minitablea50.logit.ADD())
        minitablea50.logit.ADD()
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
        req(minitablea50.logit.INT())
        minitablea50.logit.INT()
      } else if (selectedmodelA50() == "Âge (lien probit)") {
        req(minitablea50.probit.L())
        minitablea50.probit.L()
      } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
        req(minitablea50.probit.ADD())
        minitablea50.probit.ADD()
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
        req(minitablea50.probit.INT())
        minitablea50.probit.INT()
      } else if (selectedmodelA50() == "Âge (lien cloglog)") {
        req(minitablea50.cloglog.L())
        minitablea50.cloglog.L()
      } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
        req(minitablea50.cloglog.ADD())
        minitablea50.cloglog.ADD()
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
        req(minitablea50.cloglog.INT())
        minitablea50.cloglog.INT()
      }
    }))
  ## selected A50 plot -------------------------------------------------------
  output$selectedmodelA50plot <- renderPlot({
    req(selectedmodelA50())
    if (selectedmodelA50() == "Âge (lien logit)") {
      req(df_maturiteage(),
          AGEmaturite.model.logit.L(),
          minitablea50.logit.L())
      ggmodel_L_A50(df = df_maturiteage(),
                    model = AGEmaturite.model.logit.L(),
                    minitable = minitablea50.logit.L())
    } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
      req(df_maturiteage(),
          AGEmaturite.model.logit.ADD(),
          minitablea50.logit.ADD())
      ggmodel_ADD_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.logit.ADD(),
                      minitable = minitablea50.logit.ADD())
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
      req(df_maturiteage(),
          AGEmaturite.model.logit.INT(),
          minitablea50.logit.INT())
      ggmodel_INT_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.logit.INT(),
                      minitable = minitablea50.logit.INT())
    } else if (selectedmodelA50() == "Âge (lien probit)") {
      req(df_maturiteage(),
          AGEmaturite.model.probit.L(),
          minitablea50.probit.L())
      ggmodel_L_A50(df = df_maturiteage(),
                    model = AGEmaturite.model.probit.L(),
                    minitable = minitablea50.probit.L())
    } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
      req(
        df_maturiteage(),
        AGEmaturite.model.probit.ADD(),
        minitablea50.probit.ADD()
      )
      ggmodel_ADD_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.probit.ADD(),
                      minitable = minitablea50.probit.ADD())
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
      req(
        df_maturiteage(),
        AGEmaturite.model.probit.INT(),
        minitablea50.probit.INT()
      )
      ggmodel_INT_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.probit.INT(),
                      minitable = minitablea50.probit.INT())
    } else if (selectedmodelA50() == "Âge (lien cloglog)") {
      req(df_maturiteage(),
          AGEmaturite.model.cloglog.L(),
          minitablea50.cloglog.L())
      ggmodel_L_A50(df = df_maturiteage(),
                    model = AGEmaturite.model.cloglog.L(),
                    minitable = minitablea50.cloglog.L())
    } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
      req(
        df_maturiteage(),
        AGEmaturite.model.cloglog.ADD(),
        minitablea50.cloglog.ADD()
      )
      ggmodel_ADD_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.cloglog.ADD(),
                      minitable = minitablea50.cloglog.ADD())
    } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
      req(
        df_maturiteage(),
        AGEmaturite.model.cloglog.INT(),
        minitablea50.cloglog.INT()
      )
      ggmodel_INT_A50(df = df_maturiteage(),
                      model = AGEmaturite.model.cloglog.INT(),
                      minitable = minitablea50.cloglog.INT())
    }
  }, res = 96)
  ## download_selectedmodelA50plot -------------------------------------------
  output$download_selectedmodelA50plot <- downloadHandler(
    filename = function() {
      req(selectedmodelA50())
      if (selectedmodelA50() == "Âge (lien logit)") {
        paste("ggAGEmaturite_logit.L", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
        paste("ggAGEmaturite_logit.ADD", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
        paste("ggAGEmaturite_logit.INT", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge (lien probit)") {
        paste("ggAGEmaturite_probit.L", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
        paste("ggAGEmaturite_probit.ADD", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
        paste("ggAGEmaturite_probit.INT", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge (lien cloglog)") {
        paste("ggAGEmaturite_cloglog.L", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
        paste("ggAGEmaturite_cloglog.ADD", '.png', sep = '')
      } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
        paste("ggAGEmaturite_cloglog.INT", '.png', sep = '')
      }
    },
    content = function(file) {
      ggsave(file, plot = {
        req(selectedmodelA50())
        if (selectedmodelA50() == "Âge (lien logit)") {
          req(df_maturiteage(),
              AGEmaturite.model.logit.L(),
              minitablea50.logit.L())
          ggmodel_L_A50(df = df_maturiteage(),
                        model = AGEmaturite.model.logit.L(),
                        minitable = minitablea50.logit.L())
        } else if (selectedmodelA50() == "Âge + Sexe (lien logit)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.logit.ADD(),
            minitablea50.logit.ADD()
          )
          ggmodel_ADD_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.logit.ADD(),
                          minitable = minitablea50.logit.ADD())
        } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien logit)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.logit.INT(),
            minitablea50.logit.INT()
          )
          ggmodel_INT_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.logit.INT(),
                          minitable = minitablea50.logit.INT())
        } else if (selectedmodelA50() == "Âge (lien probit)") {
          req(df_maturiteage(),
              AGEmaturite.model.probit.L(),
              minitablea50.probit.L())
          ggmodel_L_A50(df = df_maturiteage(),
                        model = AGEmaturite.model.probit.L(),
                        minitable = minitablea50.probit.L())
        } else if (selectedmodelA50() == "Âge + Sexe (lien probit)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.probit.ADD(),
            minitablea50.probit.ADD()
          )
          ggmodel_ADD_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.probit.ADD(),
                          minitable = minitablea50.probit.ADD())
        } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien probit)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.probit.INT(),
            minitablea50.probit.INT()
          )
          ggmodel_INT_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.probit.INT(),
                          minitable = minitablea50.probit.INT())
        } else if (selectedmodelA50() == "Âge (lien cloglog)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.cloglog.L(),
            minitablea50.cloglog.L()
          )
          ggmodel_L_A50(df = df_maturiteage(),
                        model = AGEmaturite.model.cloglog.L(),
                        minitable = minitablea50.cloglog.L())
        } else if (selectedmodelA50() == "Âge + Sexe (lien cloglog)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.cloglog.ADD(),
            minitablea50.cloglog.ADD()
          )
          ggmodel_ADD_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.cloglog.ADD(),
                          minitable = minitablea50.cloglog.ADD())
        } else if (selectedmodelA50() == "Âge + Sexe + Âge:Sexe (lien cloglog)") {
          req(
            df_maturiteage(),
            AGEmaturite.model.cloglog.INT(),
            minitablea50.cloglog.INT()
          )
          ggmodel_INT_A50(df = df_maturiteage(),
                          model = AGEmaturite.model.cloglog.INT(),
                          minitable = minitablea50.cloglog.INT())
        }
      } , device = "png")
    }
  )
  output$titreA50_selection_modeles_table <-
    renderText(
      "Comparaison de modèles visant à schématiser proportion mature en fonction de l'âge TITRE TBD"
    )
  output$titreselectedmodelA50minitable <-
    renderText("Présentation des parametres de la courbe TITRE TBD")
  output$titreselectedmodelA50plot <-
    renderText("A50 graphique titre TBD")
  # Download report ---------------------------------------------------------
  output$report <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      params <- list(n = input$n,
                     typ_pech = input$typ_pech)
      callr::r(render_report,
               list(
                 input = report_path,
                 output = file,
                 params = params
               ))
    }
  )
}