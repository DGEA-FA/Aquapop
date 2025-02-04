app_ui <- function() {
  # Source the text elements
  source("texte/text_elements.R", local = TRUE)
  fluidPage(
    
    
    # app_title --------------------------------------------------------------
    titlePanel(title = "AquaPop : Outil d'aide à l'analyse de données d'inventaire ichtyologique"),
    navbarPage(
      "",
      # Page d'accueil -------------------------------------------------------------
      tabPanel(
        icon("home"),
        tags$iframe(
          src = "user_guide.html",
          width = "100%",
          height = "800px",
          style = "border:none;"
        )
      ),
      # Téléchargement ---------------------------------------------------
      tabPanel(
        title = "Téléchargement",
        icon = icon("upload"),
        sidebarLayout(
          sidebarPanel(
            # Inclure le texte d'instructions avec une taille réduite
            tags$div(
              htmltools::includeMarkdown(path = './texte/instruction_texte.rmd'),
              style = "font-size: 85%; color: #555;"
            ),
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
            #dans uploadexample.R
            ## filter_ID -------------------------------------------------------------
            uiOutput(outputId = "no_lac"),
            uiOutput(outputId = "typ_pech"),
            uiOutput(outputId = "annee"),
            uiOutput(outputId = "annee_notif"),
            uiOutput(outputId = "visualiser")
          ),
          ## display_brut ----------------------------------------------------------
          mainPanel(
            
            # Affichage des messages de statut pour les dataframes
            uiOutput("status_text_data_station"),
            uiOutput("status_text_data_recolte"),
            uiOutput("status_text_data_specimen"),
            uiOutput("status_text_data_lac"),
            
            
            tableOutput(outputId = "recap_intro_table"),
            tabsetPanel(
              id = "switcher",
              type = "hidden",
              selected = NULL,
              tabPanelBody("lac", dataTableOutput(outputId = "table_lac")),
              tabPanelBody("station", dataTableOutput(outputId = "table_station")),
              tabPanelBody("recolte", dataTableOutput(outputId = "table_recolte")),
              tabPanelBody("specimen", dataTableOutput(outputId = "table_specimen"))

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
                   htmltools::includeMarkdown(path = './texte/BPUE_texte.rmd'),
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
          tabPanel(
            title = "Structure de taille",
            sidebarPanel(
              radioButtons(
                inputId = "groupetailleplot",
                label = "Filtrer des poissons",
                choices  = c(
                  "Tous" = "tous",
                  "Origine (marqué ou non-marqué)" = "marquage",
                  "Sexe" = "sexe",
                  "Statut reproducteur" = "maturite"
                )
              ),
              downloadButton(outputId = "download_groupetailleplot", label = "Téléchargement du graphique")
            ),
            mainPanel(
              htmltools::includeMarkdown(path = './texte/structuretaille_texte.rmd'),
              withSpinner(
                plotOutput("structuretailleplot", width = 600, height = 400),
                type = myspinner
              ),
              h3(text_elements$titrestructuretailleplot),
              
              # textOutput(outputId = 'titrestructuretailleplot'), #titre plot
              downloadButton(outputId = "download_data4plot_taille", label = "Téléchargement des données du graphique")
              
            )
          ),
          ## PSD_subpanel -----------------------------------------------------------
          tabPanel(
            title = "PSD",
            htmltools::includeMarkdown(path = './texte/psd_texte.rmd'),
            withSpinner(tableOutput(outputId = "psd1_table"), type = myspinner),
            withSpinner(tableOutput(outputId = "psd2_table"), type = myspinner),
            downloadButton(outputId = "download_psd2", label = "Téléchargement"),
            plotOutput("psd1plot", width = 600, height = 400),
            h3(text_elements$titrepsd1plot),
            
            downloadButton(outputId = "download_psd1plot", label = "Téléchargement du graphique")
          ),
          ## ggplot_age_subpanel ----------------------------------------------------
          tabPanel(
            title = "Structure d’âge",
            sidebarPanel(
              radioButtons(
                inputId = "groupeageplot",
                label = "Filtrer des poissons",
                choices  = c(
                  "Tous" = "tous",
                  "Origine (marqué ou non-marqué)" = "marquage",
                  "Sexe" = "sexe",
                  "Statut reproducteur" = "maturite"
                )
              ),
              downloadButton(outputId = "download_groupeageplot", label = "Téléchargement du graphique")
            ),
            mainPanel(
              htmltools::includeMarkdown(path = './texte/structureage_texte.rmd'),
              withSpinner(
                plotOutput("structureageplot", width = 600, height = 400),
                type = myspinner
              ),
              
              h3(text_elements$titrestructureageplot),
              downloadButton(outputId = "download_data4plot_age", label = "Téléchargement des données du graphique")
            )
          ),
          ## relation_masse_longueur_subpanel ---------------------------------------
          tabPanel(
            title = "Relation masse-longueur",
            htmltools::includeMarkdown(path = './texte/masselongueur_texte.rmd'),
            withSpinner(
              plotly::plotlyOutput(
                outputId = 'masselongueur_plot',
                width = 600,
                height = 400
              ),
              type = myspinner
            ),
            h3(text_elements$titregraph_relmasselongueur),
            downloadButton(outputId = "download_masselongueur_plot", label = "Téléchargement"),
            br(), br(),
            
            # Ajout du tableau des coefficients
            h3("Tableau des coefficients de la relation Masse-Longueur"),
            tableOutput("relation_masse_longueur_table"),
            
            # Bouton de téléchargement du tableau
            downloadButton(outputId = "download_relation_masse_longueur_table", label = "Télécharger le tableau")
          )
        )
      ),
      # indice_condition_panel --------------------------------------------------
      tabPanel(
        title = "Indice de condition",
        htmltools::includeMarkdown(path = './texte/wri1_texte.rmd'),
        withSpinner(tableOutput(outputId = "wri1_table"), type = myspinner),
        downloadButton(outputId = "download_wri1", label = "Téléchargement"),
        htmltools::includeMarkdown(path = './texte/wri2_texte.rmd'),
        withSpinner(plotOutput(
          "wri2plot", width = 600, height = 400
        ), type = myspinner),
        h3(text_elements$titrewri2plot),
        
        downloadButton(outputId = "download_wri2plot", label = "Téléchargement"),
        htmltools::includeMarkdown(path = './texte/wri3_texte.rmd'),
        withSpinner(plotOutput(
          "wri3plot", width = 600, height = 400
        ), type = myspinner),
        h3(text_elements$titrewri3plot),
        
        downloadButton(outputId = "download_wri3plot", label = "Téléchargement")
      ),
      # croissance_panel --------------------------------------------------------
      tabPanel(
        title = "Croissance",
        htmltools::includeMarkdown(path = './texte/croissance_texte.rmd'),
        h3(text_elements$titrecroissance1),
        
        withSpinner(reactableOutput(outputId = "croissance1_table"), type = myspinner),
        downloadButton(outputId = "download_croissance1", label = "Téléchargement"),
        
        # Supprimer le sidebarLayout
        plotOutput(
          outputId = "selectedmodelcroissanceplot",
          width = 600,
          height = 400
        ),
        h3(text_elements$titreselectedmodelcroissanceplot),
        
        downloadButton(outputId = "download_selectedmodelcroissanceplot", label = "Téléchargement")
      ),
      # mortalite_panel ---------------------------------------------------------
      tabPanel(
        title = "Mortalité",
        p("Voici un rappel du graphique de la structure d'âge, avec le Peak Plus mis en évidence. Le Peak Plus représente l'âge à partir duquel les indicateurs de mortalité devraient être estimés.'Comme on souhaite avoir la meilleure représentation possible des
        classes d’âges pour estimer Z, il est préférable d’utiliser la classe d’âge suivant le Peak observé' (Mainguy et Moral, 2021), 
        soit le Peak Plus."),
        
        plotOutput("structureageplot4death", width = 300, height = 200),
        

        verbatimTextOutput("pp_og"), #modifier ca pour que ce soit plus cute dans la mise en page

        p("Pour lancer les estimations de mortalité, inscrivez ce nombre (ou celui que vous préférez) dans la boîte de texte ci-dessous."),
        
        numericInput("newPPtext", "Âge à partir duquel sera calculée la mortalité:", min = 0, max = 100, value = NA),
        br(),
        
        actionButton("goButton", "C'est parti!"),
        p("Cliquez sur le bouton pour mettre à jour l'âge à partir duquel sera calculée la mortalité."),
        htmltools::includeMarkdown(path = './texte/mortalite_texte.rmd'),
        
        withSpinner(tableOutput(outputId = "mortalite1_table"), type = myspinner),
        downloadButton(outputId = "download_mortalite1", label = "Téléchargement"),
        p("Le modèle XYZ décrit le mieux la mortalité de la population de touladi (plus faible AICc).  La mortalité annuelle s’élève à XX% (libellé TBD)."),
        br(),
        p("Le modèle Chapman-Robson est également présenté à des fins comparatives."),
        withSpinner(tableOutput(outputId = "mortalite2_table"), type = myspinner)
      ),
      # maturite_sexuelle_panel -------------------------------------------------
      tabPanel(title = "Maturité sexuelle",
               tabsetPanel(
                 tabPanel(
                   title = "Longueur à maturité",
                   htmltools::includeMarkdown(path = './texte/L50_texte.rmd'),
                   br(),
                   h3(text_elements$titreL50_selection_modeles_table),
                   
                   withSpinner(
                     reactableOutput(outputId = "L50_selection_modeles_table"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_L50_selection_modeles_table", label = "Téléchargement"),
                   sidebarLayout(
                     sidebarPanel(
                       h3(text_elements$titreselectedmodelL50minitable),
                       tableOutput(outputId = "selectedmodelL50minitable"),
                       downloadButton(outputId = "download_minitableselectedmodelL50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(
                         "selectedmodelL50plot",
                         width = 600,
                         height = 400
                       ),
                       h3(text_elements$titreselectedmodelL50plot),
                       
                       downloadButton(outputId = "download_selectedmodelL50plot", label = "Téléchargement"),
                     )
                   )
                 ),
                 tabPanel(
                   title = "Âge à maturité",
                   htmltools::includeMarkdown(path = './texte/A50_texte.rmd'),
                   br(),
                   h3(text_elements$titreA50_selection_modeles_table),
                   
                   withSpinner(
                     reactableOutput(outputId = "A50_selection_modeles_table"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_A50_selection_modeles_table", label = "Téléchargement"),
                   sidebarLayout(
                     sidebarPanel(
                       h3(text_elements$titreselectedmodelA50minitable),
                       
                       tableOutput(outputId = "selectedmodelA50minitable"),
                       downloadButton(outputId = "download_minitableselectedmodelA50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(
                         outputId = "selectedmodelA50plot",
                         width = 600,
                         height = 400
                       ),
                       h3(text_elements$titreselectedmodelA50plot),
                       downloadButton(outputId = "download_selectedmodelA50plot", label = "Téléchargement"),
                     )
                   )
                 )
               ))
      
    )
  )
}
