# ════════════════════════════════════════════════════════════════════════
# INTERFACE UTILISATEUR – FONCTION PRINCIPALE app_ui()
# ════════════════════════════════════════════════════════════════════════

app_ui <- function() {
  
  # Chargement des éléments textuels (ex. : titres, instructions, etc.)
  source("texte/text_elements.R", local = TRUE)
  
  fluidPage(
    
    
    # TITRE DE L’APPLICATION -------------------------------------------------
    titlePanel(
      title = "AquaPop : Outil d'aide à l'analyse de données d'inventaire ichtyologique"
      ),
    
    # NAVIGATION PAR ONGLET --------------------------------------------------
    navbarPage(
      "",
      
      # ONGLET 1 – Page d’accueil --------------------------------------------
      tabPanel(
        icon("home"),
        tags$iframe(
          src = "user_guide.html",
          width = "100%",
          height = "800px",
          style = "border:none;"
        )
      ),
      
      # ONGLET 2 – Téléchargement des données --------------------------------
      tabPanel(
        title = "Téléchargement",
        icon = icon("upload"),
        
        sidebarLayout(
          
          # PANNEAU LATÉRAL – Téléversement et filtres -----------------------
          sidebarPanel(
            
            # Texte d’instructions utilisateur
            tags$div(
              htmltools::includeMarkdown(path = './texte/instruction_texte.rmd'),
              style = "font-size: 85%; color: #555;"
            ),
            
            
            # Bouton de téléchargement de fichier .xlsx ----------------------
            tags$head(
              tags$style(HTML("
                .btn-file {
                  background-color: #007bff !important;
                  color: white !important;
                  font-weight: bold !important;
                }
              "))
            ),
            fileInput(
              inputId = "upload",
              label = "Téléchargez vos données (*.xlsx)",
              buttonLabel = "Téléchargement...",
              multiple = FALSE,
              accept = c(".xlsx")
            ),
            
            # Filtres dynamiques (pêche, lac, année) --------------------------
            uiOutput(outputId = "ui_typ_pech"),
            uiOutput(outputId = "ui_no_lac"),
            uiOutput(outputId = "ui_annee"),
            
            # Sélecteur pour visualisation des jeux de données ---------------
            uiOutput(outputId = "visualiser")
          ),
          
          # PANNEAU PRINCIPAL – Affichage brut des données ------------------
          mainPanel(
            
            # Tableau de synthèse introductif
            tableOutput(outputId = "recap_intro_table"),
            
            # Affichage tabulaire conditionnel selon sélection utilisateur
            tabsetPanel(
              id = "switcher",
              type = "hidden",
              selected = NULL,
              tabPanelBody("data_lac", dataTableOutput(outputId = "table_lac")),
              tabPanelBody("data_station", dataTableOutput(outputId = "table_station")),
              tabPanelBody("specimen", dataTableOutput(outputId = "table_specimen")),
              tabPanelBody("specimen_valid", dataTableOutput(outputId = "table_specimen_valid")),
              tabPanelBody("capture", dataTableOutput(outputId = "table_capture"))
            )
          )
        )
      ),
      
      # abondance_biomasse_panel ------------------------------------------------
      tabPanel(title = "Abondance et biomasse",
               tabsetPanel(
                 tabPanel(
                   title = "CPUE",
                   p("Le tableau ci-dessous présente le nombre de captures de 
                   l’espèce visée selon la table *Récolte* et le nombres d’individus
                   dans la table *Spécimens*.  
                   Si la récolte est plus élevée que le nombre de spécimens,
                   il peut s’agir d’un poisson échappé ou trop magané pour
                   prendre des mesures, etc. Si le nombre de spécimens est plus 
                   élevé que la récolte, il y a erreur à corriger dans la base
                   de données. Les modèles d’abondance globale (CPUE_tous)
                   sont calculés à partir du nombre de captures indiqués 
                   dans la *Récolte* alors que le tableau récapitulatif 
                   est calculé à partir des données de la table *Spécimens*."),
                   
                   htmltools::includeMarkdown(path = './texte/CPUE_texte.rmd'),
                   
                   # Tableau CPUE - Tous
                   withSpinner(uiOutput("cpue_tous_table"), type = myspinner),
                   uiOutput("cpue_tous_dl_ui"),
                   
                   # Tableau CPUE - Femelles matures
                   withSpinner(uiOutput("cpue_femelles_table"), type = myspinner),
                   uiOutput("cpue_femelles_dl_ui"),
                   
                   # Tableau abondance
                   withSpinner(uiOutput("abondance1_table"), type = myspinner),
                   uiOutput("abondance1_dl_ui")
                 ),
                 tabPanel(
                   title = "BPUE",
                   p("Le tableau ci-dessous présente la répartition de la biomasse capturée selon la table *Spécimens*."),
                   withSpinner(uiOutput("biomasse1table"), type = myspinner),
                   uiOutput("download_biomasse1_ui")
                 )
                 
               )),
      # structure_population_panel ----------------------------------------------
      tabPanel(
        title = "Structure de population",
        tabsetPanel(
          ## taille_masse_age_subpanel ----------------------------------------------
          tabPanel(
            title = "Taille, masse et âge moyens",
            p("Le tableau suivant reprend les statistiques descriptives, soit le nombre de spécimens mesurés/pesés/âgés (N) 
              ainsi que la moyenne (Moy.), l’écart-type (ET), les valeurs minimale (Min) et maximale (Max) de la longueur
              totale maximale (LTMax), de la masse et de l’âge des poissons pour différents groupes."),
            h3("Aperçu des données morphologiques"),
            withSpinner(uiOutput("taillemasseage_ui"), type = myspinner),
            uiOutput("dl_taillemasseage_ui")
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
              )
            ),
            
            mainPanel(
              p("L’histogramme de fréquence des longueurs permettant de caractériser la structure
                de taille de la population est réalisée avec la fonction geom_histogram de 
                la librairie ggplot2 (Chang et al. 2021). La sélection des intervalles pour
                les classes de taille est basée sur les recommandations de Anderson et Neumann (1996) 
                et Neumann et al. (2012). Ainsi, des intervalles de 20 mm sont utilisés pour l’omble 
                de fontaine, alors qu’ils sont de 50 mm pour le doré jaune et le touladi."),
              h3("Histogramme de fréquence des longueurs"),
              p("La figure ci-dessous représente l’histogramme de fréquence des
                longueurs selon le filtre sélectionné à gauche."),
              withSpinner(
                plotOutput("structuretailleplot", width = "100%", height = "400px"),
                type = myspinner
              ),
              downloadButton(outputId = "download_groupetailleplot", label = "Téléchargement du graphique"),

              downloadButton(outputId = "download_data4plot_taille",
                             label = "Téléchargement des données du graphique"
              )
            )
          ),
          
          
          ## PSD_subpanel -----------------------------------------------------------
          tabPanel(
            title = "PSD",
            p("Autrefois appelé *Proportional stock density*, l’indice *Proportional size distribution* est un descripteur 
              numérique de la distribution de fréquence des longueurs. Il permet de comparer de manière objective la 
              structure de taille de deux populations d’une même espèce (ou d’une même population lors de deux inventaires 
              distincts). Les classes de taille sont établies en fonction de la taille record enregistrée pour une espèce 
              et les autres classes sont dérivées à partir de celle-ci (Gabelhouse 1984)."),
            withSpinner(uiOutput("psd_indice_ui"), type = myspinner),
            uiOutput("psd_byclass_ui"),
            uiOutput("dl_psd_byclass_ui"),
            h3("Distribution de fréquence de longueurs avec les classes de PSD"),
            plotOutput("psd_byclass_plot", width = 600, height = 400),
            downloadButton("download_psd_byclass_plot", label = "Téléchargement du graphique")
          ),
          ## structure d'âge -------------------------------------------------
          tabPanel(
            title = "Structure d'âge",
            
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
              )
            ),
            
            mainPanel(
              p("L’histogramme de fréquence d'âge permettant de caractériser la structure d'âge 
                de la population est réalisée avec la fonction geom_histogram de la librairie ggplot2 (Chang et al. 2021)."),
              h3("Histogramme de fréquence des âges"),
              p("La figure ci-dessous représente l’histogramme de fréquences des âges 
                selon le filtre sélectionné à gauche."),
              withSpinner(
                plotOutput("structureageplot", width = "100%", height = "400px"),
                type = myspinner
              ),
              downloadButton(outputId = "download_groupeageplot",
                             label = "Téléchargement du graphique"
              ),
              downloadButton(
                outputId = "download_data4plot_age",
                label = "Téléchargement des données du graphique"
              )
            )
          ),
          
          ## relation_masse_longueur_subpanel ---------------------------------------
          tabPanel(
            title = "Relation masse-longueur",
            
            # Texte explicatif
            p("La figure suivante représente la relation allométrique entre la longueur totale
              maximale (mm) et la masse (g). L’équation et la valeur des paramètres sont indiqués sur 
              le graphique."),
            
            # Graphique
            h3("Relation masse-longueur"),

            withSpinner(plotOutput("plot_masselongueur"), type = myspinner),
            downloadButton("download_masselongueur_plot", label = "Téléchargement du graphique"),
            # br(), br(),

            # Tableau des coefficients
            h3("Tableau des coefficients"),
            uiOutput("table_masselongueur_ui"),
            uiOutput("download_masselongueur_table_ui")
          )
        )
      ),
      # indice_condition_panel --------------------------------------------------
      tabPanel(
        title = "Indice de condition",
        p("Le tableau ci-dessous présente l’indice de masse relative (Wr) et son intervalle de confiance 
          à 95 % pour l’ensemble de la population, par sexe et par classe de PSD (classe selon
          Gabelhouse 1984)."),
        uiOutput("wri_table_ui"),
        uiOutput("download_wri_table_ui"),
        
        p("Le graphique suivant illustre, pour chaque spécimen capturé, l’indice de condition en 
          fonction de la longueur totale maximale et du sexe. La valeur moyenne est indiquée par une 
          ligne pointillée en rouge (tous), en bleu foncé (femelles) et en bleu pâle. La ligne en gris 
          représente la référence standard pour l’espèce selon Hyatt & Hubert 2011 (SAFO), 
          Murphy et al. 1990 (SAVI) et Piccolo et al. 1993 (SANA)."),
        h3("Indice de condition (Wr) selon la longueur et le sexe"),
        
        withSpinner(plotOutput("wri_plot_tous", height = "400px"), type = myspinner),
        downloadButton("download_wri_plot_tous", "Téléchargement du graphique"),
        p("Ce graphique présente la variation de l’indice de condition selon les classes de PSD. Les 
          valeurs moyenne et les intervalles de confiance sont illustrés."),
        h3("Indice de condition (Wr) moyen par classe de taille"),
        
        plotOutput("wri_plot_byclass", height = "400px"),
        downloadButton("download_wri_plot_byclass", "Téléchargement du graphique")
      ),
      
      # croissance_panel --------------------------------------------------------
      # --- UI pour le panneau Croissance ---
      tabPanel(
        "Croissance",
        p("Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc.
          Prenez note également que le modèle de von Bertalanffy utilise la méthode
          pondérée avec t0 variable. Attention : les IC95% des prédictions ne peuvent
          pas être calculées à partir des IC95% des estimations des paramètres L, K et t0."),
        
        h3("Table de sélection du modèle de croissance"),
        withSpinner(reactableOutput("table_modeles_croissance_table"), type = myspinner),
        uiOutput("download_table_modeles_croissance_ui"),
        # br(), br(),
        
        h3("Longueur à l’âge des spécimens capturés et modèle de croissance"),
        
        plotOutput("selectedmodelcroissanceplot"),
        downloadButton("download_selectedmodelcroissanceplot", "Télécharger le graphique")
      ),
     
      # mortalite_panel ---------------------------------------------------------
      tabPanel(
        title = "Mortalité",
        # p("Voici un rappel du graphique de la structure d'âge, avec le Peak Plus mis en évidence. Le Peak Plus représente l'âge à partir duquel les indicateurs de mortalité devraient être estimés.'Comme on souhaite avoir la meilleure représentation possible des
        # classes d’âges pour estimer Z, il est préférable d’utiliser la classe d’âge suivant le Peak observé' (Mainguy et Moral, 2021), 
        # soit le Peak Plus."),
        h3("Test de sur-dispersion du modèle Poisson"),
        
        p("Ce test évalue si les données de mortalité par âge violent l’hypothèse d’équidispersion du modèle de Poisson. 
    En cas de sur-dispersion, l’utilisation de modèles alternatifs est recommandée."),
        
        strong("Interprétation :"),
        verbatimTextOutput("dispersion_msg"),
        br(),
        withSpinner(plotOutput("plot_dispersion_poisson", width = "100%", height = "400px"), type = myspinner),
        downloadButton("download_plot_dispersion_poisson", label = "Télécharger le graphique"),
        
        p("Le tableau suivant présente les résultats pour l’ensemble des
        modèles testés. Le modèle le mieux adapté aux données est celui 
        avec le plus faible AICc."),
        uiOutput("comparaison_mortalite_ui"),
        uiOutput("download_comparaison_mortalite_table_ui"),
        textOutput("phrase_mortalite"),
        
        h3("Distribution d'âge et modèle de mortalité retenu"),
        withSpinner(plotOutput("plot_mortalite"), type = myspinner),
        downloadButton("download_plot_mortalite", "Télécharger le graphique"),
        br(), br(),
        p("La mortalité estimée selon le modèle de Chapman-Robson est 
          présentée à titre comparatif seulement, car son utilisation
          n’est pas recommandée."),
        h3("Chapman-Robson"),
        withSpinner(uiOutput("table_chaprob"), type = myspinner),
        uiOutput("download_chaprob_df_ui")
      ),
      # maturite_sexuelle_panel -------------------------------------------------
      tabPanel(title = "Maturité sexuelle",
               tabsetPanel(
                 tabPanel(
                   title = "Longueur à maturité",
                   h4("Approche des sexes séparés"),
                   tableOutput("separate_evaluation_table"),
                   textOutput("best_separate_model_text"),
                   
                   br(),
                   uiOutput("combined_section") # Afficher la section combinée seulement si nécessaire
                   
                  
                   
                   
                 ),
                 tabPanel(
                   title = "Âge à maturité",
                   htmltools::includeMarkdown(path = './texte/A50_texte.rmd'),
                   
                 )
               ))
      
    )
  )
}
