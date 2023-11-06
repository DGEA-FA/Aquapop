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
              tabPanelBody(
                "parametres",
                dataTableOutput(outputId = "table_parametres")
              )
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
          tabPanel(
            title = "Structure de taille",
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
              ),
              textOutput(outputId = 'titrestructuretailleplot') #titre plot
            )
          ),
          ## PSD_subpanel -----------------------------------------------------------
          tabPanel(
            title = "PSD",
            htmltools::includeMarkdown(path = './texte/psd_texte.rmd'),
            withSpinner(tableOutput(outputId = "psd1_table"), type = myspinner),
            downloadButton(outputId = "download_psd1", label = "Téléchargement"),
            withSpinner(tableOutput(outputId = "psd2_table"), type = myspinner),
            downloadButton(outputId = "download_psd2", label = "Téléchargement"),
            plotOutput("psd1plot", width = 600, height = 400),
            textOutput(outputId = 'titrepsd1plot'),
            #titre plot
            downloadButton(outputId = "download_psd1plot", label = "Téléchargement")
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
              ),
              textOutput(outputId = 'titrestructureageplot') #titre plot
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
            textOutput(outputId = 'titregraph_relmasselongueur'),
            #titre plot
            downloadButton(outputId = "download_masselongueur_plot", label = "Téléchargement")
          )
        )
      ),
      # indice_condition_panel --------------------------------------------------
      tabPanel(
        title = "Indice de condition",
        withSpinner(tableOutput(outputId = "wri1_table"), type = myspinner),
        downloadButton(outputId = "download_wri1", label = "Téléchargement"),
        htmltools::includeMarkdown(path = './texte/wri2_texte.rmd'),
        withSpinner(plotOutput(
          "wri2plot", width = 600, height = 400
        ), type = myspinner),
        textOutput(outputId = 'titrewri2plot'),
        #titre plot
        downloadButton(outputId = "download_wri2plot", label = "Téléchargement"),
        withSpinner(plotOutput(
          "wri3plot", width = 600, height = 400
        ), type = myspinner),
        textOutput(outputId = 'titrewri3plot'),
        #titre plot
        downloadButton(outputId = "download_wri3plot", label = "Téléchargement")
      ),
      # croissance_panel --------------------------------------------------------
      tabPanel(
        title = "Croissance",
        htmltools::includeMarkdown(path = './texte/croissance_texte.rmd'),
        textOutput(outputId = 'titrecroissance1'),
        #titre table
        withSpinner(reactableOutput(outputId = "croissance1_table"), type = myspinner),
        downloadButton(outputId = "download_croissance1", label = "Téléchargement"),
        sidebarLayout(
          sidebarPanel(textOutput(outputId = "table_stateCROISSANCE"),),
          mainPanel(
            plotOutput(
              outputId = "selectedmodelcroissanceplot",
              width = 600,
              height = 400
            ),
            textOutput(outputId = 'titreselectedmodelcroissanceplot'),
            #titre plot
            downloadButton(outputId = "download_selectedmodelcroissanceplot", label = "Téléchargement"),
          )
        )
      ),
      # mortalite_panel ---------------------------------------------------------
      tabPanel(
        title = "Mortalité",
        htmltools::includeMarkdown(path = './texte/mortalite_texte.rmd'),
        withSpinner(tableOutput(outputId = "mortalite1_table"), type = myspinner),
        downloadButton(outputId = "download_mortalite1", label = "Téléchargement"),
        withSpinner(tableOutput(outputId = "mortalite2_table"), type = myspinner),
        downloadButton(outputId = "download_mortalite2", label = "Téléchargement"),
      ),
      # maturite_sexuelle_panel -------------------------------------------------
      tabPanel(title = "Maturité sexuelle",
               tabsetPanel(
                 tabPanel(
                   title = "Longueur à maturité",
                   htmltools::includeMarkdown(path = './texte/L50_texte.rmd'),
                   br(),
                   textOutput(outputId = 'titreL50_selection_modeles_table'),
                   #titre plot
                   withSpinner(
                     reactableOutput(outputId = "L50_selection_modeles_table"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_L50_selection_modeles_table", label = "Téléchargement"),
                   sidebarLayout(
                     sidebarPanel(
                       textOutput(outputId = 'titreselectedmodelL50minitable'),
                       #titre plot
                       tableOutput(outputId = "selectedmodelL50minitable"),
                       downloadButton(outputId = "download_minitableselectedmodelL50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(
                         "selectedmodelL50plot",
                         width = 600,
                         height = 400
                       ),
                       textOutput(outputId = 'titreselectedmodelL50plot'),
                       #titre plot
                       downloadButton(outputId = "download_selectedmodelL50plot", label = "Téléchargement"),
                     )
                   )
                 ),
                 tabPanel(
                   title = "Âge à maturité",
                   htmltools::includeMarkdown(path = './texte/A50_texte.rmd'),
                   br(),
                   textOutput(outputId = 'titreA50_selection_modeles_table'),
                   #titre plot
                   withSpinner(
                     reactableOutput(outputId = "A50_selection_modeles_table"),
                     type = myspinner
                   ),
                   downloadButton(outputId = "download_A50_selection_modeles_table", label = "Téléchargement"),
                   sidebarLayout(
                     sidebarPanel(
                       textOutput(outputId = 'titreselectedmodelA50minitable'),
                       #titre plot
                       tableOutput(outputId = "selectedmodelA50minitable"),
                       downloadButton(outputId = "download_minitableselectedmodelA50", label = "Téléchargement") # Button Téléchargement
                     ),
                     mainPanel(
                       plotOutput(
                         outputId = "selectedmodelA50plot",
                         width = 600,
                         height = 400
                       ),
                       textOutput(outputId = 'titreselectedmodelA50plot'),
                       #titre plot
                       downloadButton(outputId = "download_selectedmodelA50plot", label = "Téléchargement"),
                     )
                   )
                 )
               )),
      # Téléchargement panel ------------------------------------------------------
      tabPanel(
        title = "Rapport final",
        sliderInput("n", "Number of points", 1, 100, 50),
        downloadButton(outputId = "report", label = "Generate report")
      )
    )
  )
}
