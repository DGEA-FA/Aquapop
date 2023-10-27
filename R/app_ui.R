# UI ----------------------------------------------------------------------

app_ui <- function() {
  
  fluidPage(
    
    ## app_title --------------------------------------------------------------
    
    titlePanel("AquaPop : Outil d'aide à l'analyse de données d'inventaire ichtyologique"),
    
    navbarPage("",
               
               ## home_panel -------------------------------------------------------------
               
               tabPanel(icon("home"),
                        htmltools::includeMarkdown('./texte/user_guide.rmd')
                        
               ),
               
               ## telechargement_panel ---------------------------------------------------
               tabPanel("Téléchargement", icon = icon("upload"),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            ### upload ----------------------------------------------------------------
                            fileInput("upload", "Téléchargez vos données (*.xlsx)", buttonLabel = "Téléchargement...", multiple = FALSE, accept = c(".xlsx")),
                           
                             ### voir_exemple ----------------------------------------------------------
                            uploadexampleUI("uploadexample1"),
                            ### filter_ID -------------------------------------------------------------
                            uiOutput("no_lac"), 
                            uiOutput("typ_pech"),
                            uiOutput("annee"),
                            uiOutput("annee_notif"),
                            
                      
                            
                            uiOutput("visualiser")    ),
                          
                          ### display_brut ----------------------------------------------------------
                          
                          mainPanel( tableOutput("recap_intro_table"),
                                     
                            tabsetPanel(id = "switcher",
                                        type = "hidden",
                                        selected = NULL, 
                                        tabPanelBody("lac", dataTableOutput("table_lac")),
                                        tabPanelBody("station", dataTableOutput("table_station")),
                                        tabPanelBody("recolte", dataTableOutput("table_recolte")),
                                        tabPanelBody("specimen", dataTableOutput("table_specimen")),
                                        tabPanelBody("profil", dataTableOutput("table_profil")),
                                        tabPanelBody("parametres", dataTableOutput("table_parametres"))
                            )
                          )
                          
                        )),
               
               ## abondance_biomasse_panel ------------------------------------------------
               tabPanel("Abondance et biomasse",
                        tabsetPanel(tabPanel("CPUE",
                                             htmltools::includeMarkdown('./texte/CPUE_texte.rmd'),
                                             #withSpinner(tableOutput("verif_ntable"), type = myspinner),
                                             textOutput("verif_ntexte"),
                                             withSpinner(tableOutput("selection_modele_CPUE_toustable"), type = myspinner),
                                             downloadButton("download_selection_modele_CPUE_toustable", "Téléchargement"), # Button Téléchargement
                                             
                                             withSpinner(tableOutput("selection_modele_CPUE_Fmaturetable"), type = myspinner),
                                             downloadButton("download_selection_modele_CPUE_Fmaturetable", "Téléchargement"), # Button Téléchargement
                                             
                                            withSpinner(tableOutput("abondance1table"), type = myspinner),
                                             downloadButton("download_abondance1", "Téléchargement") # Button Téléchargement
                                            
                          ),
                        tabPanel("BPUE",
                                 withSpinner(tableOutput("biomasse1table"), type = myspinner),
                                 downloadButton("download_biomasse1", "Téléchargement") # Button Téléchargement
                              
                        )
                        )
                        
               ),
               
               ## structure_population_panel ----------------------------------------------
               tabPanel("Structure de population",
                        tabsetPanel(
                          ### taille_masse_age_subpanel ----------------------------------------------
                          
                          tabPanel("Taille, masse et âge moyens",
                                   htmltools::includeMarkdown('./texte/taillemasseage_texte.rmd'),
                                   withSpinner(tableOutput("taillemasseagetable"), type = myspinner),
                                   downloadButton("download_taillemasseagetable", "Téléchargement")), # Button Téléchargement
                          
                          ### ggplot_taille_subpanel -------------------------------------------------
                          
                          tabPanel("Structure de taille",
                                   
                                   sidebarPanel(
                                     radioButtons("groupetailleplot", "Filtrer des poissons",
                                                  c("Tous" = "tous",
                                                    "Origine" = "marquage",
                                                    "Sexe" = "sexe",
                                                    "Statut reproducteur" = "maturite")),
                                     
                                     downloadButton("download_groupetailleplot", "Téléchargement") # Button
                                   ),
                                   
                                   mainPanel(
                                     htmltools::includeMarkdown('./texte/structuretaille_texte.rmd'),
                                     withSpinner(plotOutput("structuretailleplot", width = 600, height = 400), type = myspinner)
                                     
                                      )
                          ),
                          
                          ### PSD_subpanel -----------------------------------------------------------
                          
                          tabPanel("PSD",
                                   withSpinner(tableOutput("psd1_table"), type = myspinner),
                                   downloadButton("download_psd1", "Téléchargement"), # Button Téléchargement
                                   withSpinner(tableOutput("psd2_table"), type = myspinner),
                                   downloadButton("download_psd2", "Téléchargement"),
                                   plotOutput("psd1plot", width = 600, height = 400),
                                   downloadButton("download_psd1plot", "Téléchargement")# Button Téléchargement
                          ), # Button Téléchargement
                          
                          ### ggplot_age_subpanel ----------------------------------------------------
                          
                          tabPanel("Structure d’âge",
                                   
                                   sidebarPanel(
                                     radioButtons("groupeageplot", "Filtrer des poissons",
                                                  c("Tous" = "tous",
                                                    "Origine" = "marquage",
                                                    "Sexe" = "sexe",
                                                    "Statut reproducteur" = "maturite")),
                                     
                                     downloadButton("download_groupeageplot", "Téléchargement") # Button
                                   ),
                                   
                                   mainPanel(
                                     htmltools::includeMarkdown('./texte/structureage_texte.rmd'),
                                     withSpinner(plotOutput("structureageplot", width = 600, height = 400), type = myspinner)
                                     )
                          ),
                          
                          ### relation_masse_longueur_subpanel ---------------------------------------
                          
                          tabPanel("Relation masse-longueur",
                                   withSpinner(plotly::plotlyOutput('masselongueur_plot', width = 600, height = 400), type = myspinner),
                                   
                                  # withSpinner(plotOutput("masselongueur_plot", width = 600, height = 400), type = myspinner),
                                   downloadButton("download_masselongueur_plot", "Téléchargement") # Button
                          )
                        )),
               
               ## indice_condition_panel --------------------------------------------------
               
               tabPanel("Indice de condition",
                      #  titlePanel("Indice de masse relative (Wr)"),###########################
                        
                        withSpinner(tableOutput("wri1_table"), type = myspinner),
                        downloadButton("download_wri1", "Téléchargement"),
                        htmltools::includeMarkdown('./texte/wri2_texte.rmd'),
                      
                        withSpinner(plotOutput("wri2plot", width = 600, height = 400), type = myspinner),
                        downloadButton("download_wri2plot", "Téléchargement"), 
                        
                        withSpinner(plotOutput("wri3plot", width = 600, height = 400), type = myspinner),
                        downloadButton("download_wri3plot", "Téléchargement") 
               ),
               
               
               
               ## croissance_panel --------------------------------------------------------
               
               tabPanel("Croissance",
                      #  withSpinner(tableOutput("croissance1_table"), type = myspinner),
                      htmltools::includeMarkdown('./texte/croissance_texte.rmd'),
                       withSpinner(reactableOutput("croissance1_table"), type = myspinner),
                      downloadButton("download_croissance1", "Téléchargement"),

                      sidebarLayout(
                        sidebarPanel(
                          textOutput("table_stateCROISSANCE"),
                         
                        ),
                        mainPanel(
                          plotOutput("selectedmodelcroissanceplot", width = 600, height = 400),
                          downloadButton("download_selectedmodelcroissanceplot", "Téléchargement"), # Button Téléchargement
                          
                        ))#,  textOutput("croissanceJL2003_text"), #ca fonctionne, mais on voulait pas le voir
                       
                      
               ),
               
               
               ## mortalite_panel ---------------------------------------------------------
               
               tabPanel("Mortalité",
                        tabsetPanel(
                       #   tabPanel("Mortalité au RMS"),
                          tabPanel("Mortalité observée",
                                   htmltools::includeMarkdown('./texte/mortalite_texte.rmd'),
                    
                                   withSpinner(tableOutput("mortalite1_table"), type = myspinner),
                                   downloadButton("download_mortalite1", "Téléchargement"), # Button Téléchargement
                                  # textOutput("zobs_text"),
                                  
                                  withSpinner(tableOutput("mortalite2_table"), type = myspinner),
                                  downloadButton("download_mortalite2", "Téléchargement"), # Button Téléchargement
                                  
                                    )#,
                          
                          
                          #tabPanel("Outil diagnostique"),
                          #tabPanel("Graphique CPUE au RMS")
                        )),
               
               
               
               ## maturite_sexuelle_panel -------------------------------------------------
               tabPanel("Maturité sexuelle",
                        tabsetPanel(
                          tabPanel("Longueur à maturité",
                                   htmltools::includeMarkdown('./texte/L50_texte.rmd'),
                                   br(),
                                   withSpinner( reactableOutput("L50_selection_modeles_table"), type = myspinner),
                                   downloadButton("download_L50_selection_modeles_table", "Téléchargement"), # Button Téléchargement
                                   
                                   
                                   sidebarLayout(
                                   sidebarPanel(
                                     textOutput("table_stateLTM"),
                                     tableOutput("selectedmodelL50minitable"),
                                     downloadButton("download_minitableselectedmodelL50", "Téléchargement") # Button Téléchargement
                                   ),
                                     mainPanel(
                                       plotOutput("selectedmodelL50plot", width = 600, height = 400),
                                       downloadButton("download_selectedmodelL50plot", "Téléchargement"), # Button Téléchargement
                                       
                                   ))
                                   ),
                          tabPanel("Âge à maturité",
                                   htmltools::includeMarkdown('./texte/A50_texte.rmd'),
                                   br(),
                                   withSpinner( reactableOutput("A50_selection_modeles_table"), type = myspinner),
                                   downloadButton("download_A50_selection_modeles_table", "Téléchargement"), # Button Téléchargement
                                   
                                   
                                   sidebarLayout(
                                     sidebarPanel(
                                       textOutput("table_stateAGE"),
                                       tableOutput("selectedmodelA50minitable"),
                                       downloadButton("download_minitableselectedmodelA50", "Téléchargement") # Button Téléchargement
                                     ),
                                     mainPanel(
                                       plotOutput("selectedmodelA50plot", width = 600, height = 400),
                                       downloadButton("download_selectedmodelA50plot", "Téléchargement"), # Button Téléchargement
                                       
                                     ))
                                   
                                   )
                        )),
               
               ## alimentation_panel ------------------------------------------------------
               
               tabPanel("Alimentation"),
               
               ## Téléchargement panel ------------------------------------------------------
               
               tabPanel("Rapport final",
                       sliderInput("n", "Number of points", 1, 100, 50),
                       downloadButton("report", "Generate report")
      
                        )#,
              
               ## verification temporaire -------------------------------------------------
               
               # tabPanel("verif_temp",
               #          textOutput("sp_queentexte"),
               #          textOutput("sp_queentextelatin"),
               #          # dataTableOutput("specimentable"),
               #          # dataTableOutput("df_maturitetable"),
               #          #dataTableOutput("df_maturitelongtable"),
               #          #dataTableOutput("df_maturiteagetable"),
               #          verbatimTextOutput("LTMmaturite.model.logit.L_table"),
               #          #dataTableOutput("capturetable")
               # )
               
    ))
  
  

}


