#' @import htmltools
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#' @importFrom reactable reactableOutput renderReactable
app_ui <- function() {
  
  render_user_guide_if_needed()

  
  fluidPage(
    
    # Titre de l'application
    titlePanel(
      title = "AquaPop : Outil d'aide à l'analyse de données d'inventaire ichtyologique"
      ),
    
    # Navigation par onglet
    navbarPage(
      "",
      
      # Page d'accueil ----
      tabPanel(
        icon("home"),
        tags$iframe(
          src = "user_guide.html",
          width = "100%",
          height = "800px",
          style = "border:none;"
        )
      ),
      
      # Telechargement des donnees ----
      tabPanel(
        title = "Téléchargement",
        icon = icon("upload"),
        
        sidebarLayout(
          
          # Panneau lateral - Televersement et filtres
          sidebarPanel(
            
            # Texte d'instructions utilisateur
            
            tags$div(
              includeMarkdown(path = './texte/instruction_texte.rmd'),
              style = "font-size: 85%; color: #555;"
            ),
            
            # Bouton de telechargement de fichier .xlsx
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
            
            # Filtres dynamiques (peche, lac, annee)
            uiOutput(outputId = "ui_typ_pech"),
            uiOutput(outputId = "ui_no_lac"),
            uiOutput(outputId = "ui_annee"),
            
            # Selecteur pour visualisation des jeux de donnees
            uiOutput(outputId = "visualiser")
          ),
          
          # Panneau principal - Affichage brut des donnees
          mainPanel(
            
            # Tableau de synthese introductif
            tableOutput(outputId = "recap_intro_table"),
            
            # Affichage tabulaire conditionnel selon selection utilisateur

            tabsetPanel(
              id = "switcher",
              type = "hidden",
              selected = NULL,
              tabPanelBody("data_lac", DTOutput(outputId = "table_lac")),
              tabPanelBody("data_station", DTOutput(outputId = "table_station")),
              tabPanelBody("specimen", DTOutput(outputId = "table_specimen")),
              tabPanelBody("specimen_valid", DTOutput(outputId = "table_specimen_valid")),
              tabPanelBody("capture", DTOutput(outputId = "table_capture"))
            )
          )
        )
      ),
      
      # Panel 1 Abondance et biomasse
      
      tabPanel(title = "Abondance et biomasse",
               tabsetPanel(
                 
                 # CPUE - Abondance ----
                 
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
                   
                   ## Tableau CPUE - Tous ----
                   
                   withSpinner(uiOutput("cpue_tous_table"), type = myspinner),
                   download_button_ui("cpue_tous_table_dl"),
                   
                   br(),
                   
                   ## Tableau CPUE - Femelles matures ----
                   uiOutput("cpue_femelles_table"),
                   download_button_ui("cpue_femelles_table_dl"),
                   br(),
                   
                   ## Tableau d'abondance ----
                   uiOutput("abondance_table"),
                   download_button_ui("abondance_table_dl"),
                   
                 ),
                 
                 # BPUE - Biomasse ----
                 
                 tabPanel(
                   title = "BPUE",
                   p("Le tableau ci-dessous présente la répartition de la biomasse capturée."),
                   withSpinner(uiOutput("biomasse_table"), type = myspinner),
                   download_button_ui("biomasse_table_dl")
                 )
                 
               )),
      
      # Panel 2 Structure de population
      
      tabPanel(
        title = "Structure de population",
        tabsetPanel(
          
          # Taille, masse, age ----
          
          mod_taille_masse_age_ui("taille_masse_age_1"),
          
          
          # Structure de taille ----
          mod_structure_taille_ui("structure_taille_1"),
          
          # Structure d'age ----
          mod_structure_age_ui("structure_age_1"),
          
          # PSD ----
          mod_psd_ui("psd_1"),
         
          
          # Relation masse-longueur ----
          mod_masse_longueur_ui("masselongueur_1")
          
        )
      ),
      # Panel 3 
      
      # Indice de condition ----
    
     mod_wri_ui("wri_1"),
      # Panel 4
      
      # Croissance ----
     
      tabPanel(
        "Croissance",
        
        ## Tableau de sélection de modèles ----
        
        p("Si les trois modèles convergent, sélectionnez celui ayant le plus petit AICc. 
     Prenez note également que le modèle de von Bertalanffy utilise la méthode pondérée 
     avec t₀ variable. Attention : les IC95 % des prédictions ne peuvent pas être calculés 
     à partir des IC95 % des estimations des paramètres L, K et t₀."),
        
        h3("Table de sélection du modèle de croissance"),
        withSpinner(reactableOutput("table_modeles_croissance_table"), type = myspinner),
        download_button_ui("download_table_modeles_croissance"),
        
        br(),
        
        ## Graphique du modele choisi ----
        p("Le graphique suivant illustre la longueur observée des spécimens en fonction de leur âge, 
     ainsi que la courbe de croissance modélisée selon le modèle sélectionné. Les points représentent 
     les données observées, tandis que la ligne montre la prédiction du modèle."),
        
        h3("Longueur à l’âge des spécimens capturés et modèle de croissance"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(plotOutput("selectedmodelcroissanceplot", height = "500px"), type = myspinner),
          br(),
          downloadButton("download_selectedmodelcroissanceplot", "Téléchargement du graphique")
        )
        
        
      ),
      # Panel 5 
      
      # Mortalite ----
      tabPanel(
        title = "Mortalité",
        
        ## Test de sur-dispersion ----
        h3("Test de sur-dispersion du modèle Poisson"),
        
        p("Ce test évalue si les données de mortalité par âge violent l’hypothèse d’équidispersion du modèle de Poisson. 
    En cas de sur-dispersion, l’utilisation de modèles alternatifs est recommandée."),
        
        strong("Interprétation :"),
        verbatimTextOutput("dispersion_msg"),
        br(),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(plotOutput("plot_dispersion_poisson", height = "500px"), type = myspinner),
          br(),
          downloadButton("download_plot_dispersion_poisson", "Téléchargement du graphique")
        ),
        
        br(),
        
        h4("Paramètre avancé : recalcul avec un autre Peak Plus"),
        p("Par défaut, la valeur du Peak Plus est déterminée automatiquement selon la structure d’âge observée. 
Vous pouvez toutefois forcer un recalcul avec une autre valeur."),
        uiOutput("ui_custom_peak_plus"),
        actionButton("recalculer_mortalite", "Recalculer avec ce Peak Plus"),
        em(textOutput("texte_pp_utilise")),
        
        br(), br(),
        
        
        ## Comparaison des modeles ----
        p("Le tableau suivant présente les résultats pour l’ensemble des modèles testés. Le modèle le mieux adapté aux données est celui avec le plus faible AICc."),
        
        h3("Table de sélection du modèle de mortalité"),
        withSpinner(reactableOutput("comparaison_mortalite_table"), type = myspinner),
        download_button_ui("download_comparaison_mortalite_table"),
        textOutput("phrase_mortalite"),
        br(),
        
        ## Graphique du modele choisi ----
        h3("Distribution d'âge et modèle de mortalité retenu"),
        
        div(
          style = "max-width: 900px; margin: auto;",
          withSpinner(plotOutput("plot_mortalite", height = "500px"), type = myspinner),
          br(),
          downloadButton("download_plot_mortalite", "Téléchargement du graphique")
        ),
        
        br(),
        
        ## Chapman-Robson ----
        p("La mortalité estimée selon le modèle de Chapman-Robson est présentée à titre comparatif seulement, car son utilisation n’est pas recommandée."),
        
        h3("Chapman-Robson"),
        withSpinner(uiOutput("table_chaprob"), type = myspinner),
        download_button_ui("download_chaprob_df")
      ),
      
      # Panel 6
      
      # Maturite sexuelle ----
      tabPanel(title = "Maturité sexuelle",
               tabsetPanel(
                 ## Longueur a maturite ----
                 
                 mod_maturite_l50_ui("maturite_l50_1"),
                 
                 ## Age a maturite ----
                 
                 mod_maturite_a50_ui("maturite_a50_1")
                 
               ))
      
    )
  )
}
