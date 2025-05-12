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
      mod_telechargement_ui("telechargement"),
      
      # Panel 1 Abondance et biomasse
      
      tabPanel(title = "Abondance et biomasse",
               tabsetPanel(
                 
                 # CPUE - Abondance ----
                 mod_abondance_cpue_ui("cpue"),
                 
                 # BPUE - Biomasse ----
                 mod_biomasse_bpue_ui("biomasse")
                 
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
