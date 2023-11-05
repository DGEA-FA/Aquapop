app_ui <- function() {
  fluidPage(
    # app_title --------------------------------------------------------------
    titlePanel(title = "AquaPop : Outil d'aide à l'analyse de données d'inventaire ichtyologique"), 
    navbarPage(
      "",
      # Page d'accueil -------------------------------------------------------------
      tabPanel(
        icon("home"),
        htmltools::includeMarkdown(path = './texte/user_guide.rmd')
      ),
      # Téléchargement ---------------------------------------------------
      tabPanel(
        title = "Téléchargement",
        icon = icon("upload"),
        sidebarLayout(
          sidebarPanel(
            ## upload ----------------------------------------------------------------
            fileInput(
              inputId = "upload",
              label = "Téléchargez vos données (*.xlsx)",
              buttonLabel = "Téléchargement...",
              multiple = FALSE,
              accept = c(".xlsx")
            ),
            
            ## exemple fichier ----------------------------------------------------------
            uploadexampleUI("uploadexample1"),
            ## filter_ID -------------------------------------------------------------
            uiOutput(outputId = "no_lac"),
            uiOutput(outputId = "typ_pech"),
            uiOutput(outputId = "annee"),
            uiOutput(outputId = "annee_notif"),
            uiOutput(outputId = "visualiser")
          ),
          ## display_brut ----------------------------------------------------------
          mainPanel(
            tableOutput(outputId = "recap_intro_table"),
            tabsetPanel(
              id = "switcher",
              type = "hidden",
              selected = NULL,
              tabPanelBody("lac", dataTableOutput(outputId = "table_lac")),
              tabPanelBody("station", dataTableOutput(outputId = "table_station")),
              tabPanelBody("recolte", dataTableOutput(outputId = "table_recolte")),
              tabPanelBody("specimen", dataTableOutput(outputId = "table_specimen")),
              tabPanelBody("profil", dataTableOutput(outputId = "table_profil")),
              tabPanelBody("parametres", dataTableOutput(outputId = "table_parametres"))
            )
          )
        )
      ), 
      # abondance_biomasse_panel ------------------------------------------------
      tabPanel(title = "Abondance et biomasse",
               tabsetPanel(
                 tabPanel(
                   title = "CPUE",
                   htmltools::includeMarkdown(path = './texte/CPUE_texte.rmd'),
                   withSpinner(tableOutput(outputId = "verif_ntable"),
                               type = myspinner),
                   textOutput(outputId = "verif_ntexte"),
                   withSpinner(
                     tableOutput(outputId = "selection_modele_CPUE_toustable"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_selection_modele_CPUE_toustable",
                                  label = "Téléchargement"),
                   withSpinner(
                     tableOutput(outputId = "selection_modele_CPUE_Fmaturetable"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_selection_modele_CPUE_Fmaturetable",
                                  label = "Téléchargement"),
                   withSpinner(tableOutput(outputId = "abondance1table"),
                               type = myspinner),
                   downloadButton(outputId = "download_abondance1",
                                  label = "Téléchargement")
                 ),
                 tabPanel(
                   title = "BPUE",
                   withSpinner(tableOutput(outputId = "biomasse1table"),
                               type = myspinner),
                   downloadButton(outputId = "download_biomasse1",
                                  label = "Téléchargement")
                 )
               )),
      
      # structure_population_panel ----------------------------------------------
      tabPanel(
        title = "Structure de population",
        tabsetPanel(
          ## taille_masse_age_subpanel ----------------------------------------------
          tabPanel(
            title = "Taille, masse et âge moyens",
            htmltools::includeMarkdown(path = './texte/taillemasseage_texte.rmd'),
            withSpinner(tableOutput(outputId = "taillemasseagetable"), type = myspinner),
            downloadButton(outputId = "download_taillemasseagetable", label = "Téléchargement")
          ),
          
          ## structure de taille -------------------------------------------------
          tabPanel(title = "Structure de taille",
            sidebarPanel(
              radioButtons(
                inputId = "groupetailleplot",
                label = "Filtrer des poissons",
                choices  = c(
                  "Tous" = "tous",
                  "Origine" = "marquage",
                  "Sexe" = "sexe",
                  "Statut reproducteur" = "maturite"
                )
              ),
              downloadButton(outputId = "download_groupetailleplot", label = "Téléchargement")
            ),
            
            mainPanel(
              htmltools::includeMarkdown(path = './texte/structuretaille_texte.rmd'),
              withSpinner(
                plotOutput("structuretailleplot", width = 600, height = 400),
                type = myspinner
              )
              
            )
          ),
          
          ## PSD_subpanel -----------------------------------------------------------
          tabPanel(title = "PSD",
                   htmltools::includeMarkdown(path = './texte/psd_texte.rmd'),
                   
            withSpinner(tableOutput(outputId = "psd1_table"), type = myspinner),
            downloadButton(outputId = "download_psd1", label = "Téléchargement"),
            withSpinner(tableOutput(outputId = "psd2_table"), type = myspinner),
            downloadButton(outputId = "download_psd2", label = "Téléchargement"),
            plotOutput("psd1plot", width = 600, height = 400),
            downloadButton(outputId = "download_psd1plot", label = "Téléchargement")
          ),
          
          
          ## ggplot_age_subpanel ----------------------------------------------------
          
          tabPanel(title = "Structure d’âge",
            
            sidebarPanel(
              radioButtons(
                "groupeageplot",
                "Filtrer des poissons",
                c(
                  "Tous" = "tous",
                  "Origine" = "marquage",
                  "Sexe" = "sexe",
                  "Statut reproducteur" = "maturite"
                )
              ),
              
              downloadButton(outputId = "download_groupeageplot", label = "Téléchargement") 
            ),
            
            mainPanel(
              htmltools::includeMarkdown(path = './texte/structureage_texte.rmd'),
              withSpinner(
                plotOutput("structureageplot", width = 600, height = 400),
                type = myspinner
              )
            )
          ),
          
          ## relation_masse_longueur_subpanel ---------------------------------------
          tabPanel(
            "Relation masse-longueur",
            titlePanel("Relation masse-longueur TITRE TBD"),
            htmltools::includeMarkdown(path = './texte/masselongueur_texte.rmd'),
            
            withSpinner(
              plotly::plotlyOutput(outputId = 'masselongueur_plot', width = 600, height = 400),
              type = myspinner
            ),
            # withSpinner(plotOutput("masselongueur_plot", width = 600, height = 400), type = myspinner),
            downloadButton(outputId = "download_masselongueur_plot", label = "Téléchargement") 
          )
        )
      ),
      # indice_condition_panel --------------------------------------------------
      tabPanel(title = "Indice de condition",
        withSpinner(tableOutput(outputId = "wri1_table"), type = myspinner),
        downloadButton(outputId = "download_wri1", label = "Téléchargement"),
        htmltools::includeMarkdown(path = './texte/wri2_texte.rmd'),
        
        withSpinner(plotOutput(
          "wri2plot", width = 600, height = 400
        ), type = myspinner),
        downloadButton(outputId = "download_wri2plot", label = "Téléchargement"),
        
        withSpinner(plotOutput(
          "wri3plot", width = 600, height = 400
        ), type = myspinner),
        downloadButton(outputId = "download_wri3plot", label = "Téléchargement")
      ),
      # croissance_panel --------------------------------------------------------
      tabPanel(
        "Croissance",
        #  withSpinner(tableOutput("croissance1_table"), type = myspinner),
        htmltools::includeMarkdown(path = './texte/croissance_texte.rmd'),
        withSpinner(reactableOutput(outputId = "croissance1_table"), type = myspinner),
        downloadButton(outputId = "download_croissance1", label = "Téléchargement"),
        
        sidebarLayout(
          sidebarPanel(textOutput(outputId = "table_stateCROISSANCE"), ),
          mainPanel(
            plotOutput(outputId = "selectedmodelcroissanceplot",
              width = 600,
              height = 400
            ),
            downloadButton(outputId = "download_selectedmodelcroissanceplot", label = "Téléchargement"),
            
          )
        )#,  textOutput("croissanceJL2003_text"), #ca fonctionne, mais on voulait pas le voir
        
        
      ),
      
      
      # mortalite_panel ---------------------------------------------------------
      
      tabPanel(title = "Mortalité",
               tabsetPanel(
                 #   tabPanel("Mortalité au RMS"),
                 tabPanel(
                   title = "Mortalité observée",
                   htmltools::includeMarkdown(path = './texte/mortalite_texte.rmd'),
                   
                   withSpinner(tableOutput(outputId = "mortalite1_table"), type = myspinner),
                   downloadButton(outputId = "download_mortalite1", label = "Téléchargement"),
                   # textOutput("zobs_text"),
                   
                   withSpinner(tableOutput(outputId = "mortalite2_table"), type = myspinner),
                   downloadButton(outputId = "download_mortalite2", label = "Téléchargement"),
                 )#,
                 
                 
                 #tabPanel(title = "Outil diagnostique"),
                 #tabPanel(title = "Graphique CPUE au RMS")
               )),
      
      
      
      # maturite_sexuelle_panel -------------------------------------------------
      tabPanel(title = "Maturité sexuelle",
               tabsetPanel(
                 tabPanel(title = "Longueur à maturité",
                   
                   htmltools::includeMarkdown(path = './texte/L50_texte.rmd'),
                   br(),
                   titlePanel(
                     "Comparaison de modèles visant à schématiser proportion mature en fonction de la longueur TITRE TBD"
                   ),
                   
                   withSpinner(reactableOutput(outputId = "L50_selection_modeles_table"), type = myspinner),
                   downloadButton(outputId = "download_L50_selection_modeles_table", label = "Téléchargement"),
                   # Button Téléchargement
                   
                   
                   sidebarLayout(
                     sidebarPanel(
                       # textOutput("table_stateLTM"),#c'etait pour verifier que le lien se fait bien entre le table de selection de modeles et le plot
                       titlePanel("Présentation des parametres de la courbe TITRE TBD"),
                       tableOutput(outputId = "selectedmodelL50minitable"),
                       downloadButton(outputId = "download_minitableselectedmodelL50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(
                         "selectedmodelL50plot",
                         width = 600,
                         height = 400
                       ),
                       titlePanel("L50 graphique titre TBD"),
                       
                       downloadButton(outputId = "download_selectedmodelL50plot", label = "Téléchargement"),
                       # Button Téléchargement
                       
                     )
                   )
                 ),
                 tabPanel(title = "Âge à maturité",
                   htmltools::includeMarkdown(path = './texte/A50_texte.rmd'),
                   br(),
                   titlePanel(
                     "Comparaison de modèles visant à schématiser proportion mature en fonction de l'âge TITRE TBD"
                   ),
                   
                   withSpinner(reactableOutput(outputId = "A50_selection_modeles_table"), type = myspinner),
                   downloadButton(outputId = "download_A50_selection_modeles_table", label = "Téléchargement"),
                   # Button Téléchargement
                   
                   
                   sidebarLayout(
                     sidebarPanel(
                       #textOutput("table_stateAGE"), #c'etait pour verifier que le lien se fait bien entre le table de selection de modeles et le plot
                       tableOutput(outputId = "selectedmodelA50minitable"),
                       titlePanel("Présentation des parametres de la courbe TITRE TBD"),
                       
                       downloadButton(outputId = "download_minitableselectedmodelA50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(outputId = "selectedmodelA50plot",
                         width = 600,
                         height = 400
                       ),
                       titlePanel("A50 graphique titre TBD"),
                       
                       downloadButton(outputId = "download_selectedmodelA50plot", label = "Téléchargement"),
                       # Button Téléchargement
                       
                     )
                   )
                   
                 )
               )),
      
      # alimentation_panel ------------------------------------------------------
      
      #tabPanel("Alimentation"),
      
      # Téléchargement panel ------------------------------------------------------
      
      tabPanel(title = "Rapport final",
        sliderInput("n", "Number of points", 1, 100, 50),
        downloadButton(outputId = "report", label = "Generate report")
      )#,
      
      # verification temporaire -------------------------------------------------
      
      # tabPanel("verif_temp",
      #          textOutput("sp_queentexte"),
      #          textOutput("sp_queentextelatin"),
      #          # renderDataTable("specimentable"),
      #          # renderDataTable("df_maturitetable"),
      #          #renderDataTable("df_maturitelongtable"),
      #          #renderDataTable("df_maturiteagetable"),
      #          verbatimTextOutput("LTMmaturite.model.logit.L_table"),
      #          #renderDataTable("capturetable")
      # )
      
    )
  )
  
  
  
}
