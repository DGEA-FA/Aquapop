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
      
      # Indice de condition ----
    
     mod_wri_ui("wri_1"),
      
      # Croissance ----
     
     mod_croissance_ui("croissance_1"),
      
      # Mortalite ----
     mod_mortalite_ui("mortalite_1"),
      
      
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
