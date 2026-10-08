#' UI - Module de téléchargement des données
#'
#' @param id Identifiant du module
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_telechargement_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Téléchargement",
    icon = icon("upload"),
    
    sidebarLayout(
      sidebarPanel(
        tags$div(
          includeMarkdown(path = './texte/instruction_texte.rmd'),
          style = "font-size: 85%; color: #555;"
        ),
        tags$head(
          tags$style(HTML("
            .btn-file {
              background-color: #007bff !important;
              color: white !important;
              font-weight: bold !important;
            }
          "))
        ),
        fileInput(ns("upload"), "Téléchargez vos données (*.xlsx)",
                  buttonLabel = "Téléchargement...", multiple = FALSE, accept = ".xlsx"),
        uiOutput(ns("message_upload")),
        uiOutput(ns("ui_typ_pech")),
        uiOutput(ns("ui_no_lac")),
        uiOutput(ns("ui_annee")),
        uiOutput(ns("visualiser"))
      ),
      mainPanel(
        tableOutput(ns("recap_intro_table")),
        tabsetPanel(
          id = ns("switcher"),
          type = "hidden",
          selected = NULL,
          tabPanelBody("data_lac", DTOutput(ns("table_lac"))),
          tabPanelBody("data_station", DTOutput(ns("table_station"))),
          tabPanelBody("specimen_tous", DTOutput(ns("table_specimen_tous"))),
          tabPanelBody("data_recolte", DTOutput(ns("table_data_recolte")))
        )
      )
    )
  )
}


#' Server - Module de téléchargement des données
#'
#' @param id Identifiant du module
#'
#' @return Une liste de réactifs : data_lac, capture, specimen_tous, specimen_valide,specimen_hasard_valide,
#' data_station, station_valide, station_hasard_valide, filename_suffix, nom_lac
#' @noRd
mod_telechargement_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    erreur_upload <- reactiveVal(FALSE)
    
    output$message_upload <- renderUI({
      
      req(erreur_upload())
      
      div(
        HTML(
          "<strong>Votre base de données n’est pas formatée tel que requis.</strong><br>
       Veuillez vérifier les noms des feuilles et les colonnes obligatoires."
        ),
        style = "
      color: #B00020;
      margin-top: 5px;
      margin-bottom: 10px;
    "
      )
    })
    
    # ---- Chargement et validation du fichier ----
    
    data_temp <- eventReactive(input$upload, {
      
      req(input$upload)
      
      # On retire un éventuel message d'erreur précédent
      erreur_upload(FALSE)
      
      tryCatch(
        {
          lac <- load_lac(
            path = input$upload$datapath,
            namesheet = "Lac",
            verbose = FALSE
          )
          
          station <- load_station(
            path = input$upload$datapath,
            namesheet = "Stations",
            verbose = FALSE
          )
          
          recolte <- load_recolte(
            path = input$upload$datapath,
            namesheet = "Recolte",
            verbose = FALSE
          )
          
          specimen <- load_specimen(
            path = input$upload$datapath,
            namesheet = "Specimens",
            verbose = FALSE
          )
          
          list(
            lac = lac,
            station = station,
            recolte = recolte,
            specimen = specimen
          )
        },
        
        error = function(e) {
          
          # Message détaillé dans la console
          message("[Téléchargement] ", conditionMessage(e))
          
          # Active le message d'erreur l'interface
          erreur_upload(TRUE)
          
          return(NULL)
        }
      )
    })
    
    # ---- Type de pêche ----
    output$ui_typ_pech <- renderUI({
      req(data_temp())
      radioButtons(
        ns("typ_pech"),
        "Sélectionner le type de pêche normalisée",
        choices = unique(data_temp()$lac$typ_pech),
        selected = character(0)
      )
    })
    
    # ---- Filtrage type de pêche ----
    df_filtered1 <- reactive({
      req(data_temp(), input$typ_pech)
      filter_by_pen_lac_annee(
        data_temp()$lac,
        typ_pech = input$typ_pech
      )
    })
    

    output$ui_no_lac <- renderUI({
      
      req(df_filtered1())
      
      data_lacs <- df_filtered1() |>
        distinct(no_lac, nom_lac) |>
        arrange(no_lac)
      
      etiquettes <- paste0(
        data_lacs$no_lac,
        " — ",
        data_lacs$nom_lac
      )
      
      # L'étiquette est affichée, mais la valeur retournée demeure no_lac
      choices_lacs <- setNames(
        as.character(data_lacs$no_lac),
        etiquettes
      )
      
      selectInput(
        ns("no_lac"),
        "Sélectionner le lac",
        choices = choices_lacs,
        selected = NULL
      )
    })
    
    df_filtered2 <- reactive({
      req(df_filtered1(), input$no_lac)
      filter_by_pen_lac_annee(df_filtered1(), no_lac = input$no_lac)
    })
    
    output$ui_annee <- renderUI({
      req(df_filtered2())
      tagList(
        checkboxGroupInput(ns("annee"), "Sélectionner les années à considérer",
                           choices = sort(unique(df_filtered2()$annee))),
        p("Si plus d'une année est sélectionnée, l'ensemble des données seront compilées dans un seul inventaire.",
          style = "font-size: 85%; color: #555;")
      )
    })
   
    # ---- Données du lac sélectionné ---- 
    data_lac <- reactive({
      req(df_filtered2(), input$annee)
      filter_by_pen_lac_annee(df_filtered2(), annee = input$annee)
    })
    
    nom_lac_reactif <- reactive({
      req(data_lac(), input$no_lac)
      nom <- unique(filter(data_lac(), no_lac == input$no_lac)$nom_lac)
      if (length(nom) == 1 && !is.na(nom) && nzchar(as.character(nom))) as.character(nom) else NULL
    })
    
    info_pen_reactive <- reactive({
      req(input$typ_pech)
      get_info_pen(input$typ_pech)
    })
    
    # ---- Préparation des données d'analyse ----
    
    analysis_data <- reactive({
      
      req(
        data_temp(),
        input$typ_pech,
        input$no_lac,
        input$annee
      )
      
      get_analysis_data(
        data_station = data_temp()$station,
        data_specimen = data_temp()$specimen,
        data_recolte = data_temp()$recolte,
        typ_pech = input$typ_pech,
        no_lac = input$no_lac,
        annee = input$annee
      )
    })
    
    # ---- Tableau récapitulatif ----
    output$recap_intro_table <- renderTable({
      req(data_lac(), analysis_data()$data_station)
      generate_recapitulatif_inventaire(data_lac(), analysis_data()$data_station)
    })
    
    # ---- Visualisation ----
    output$visualiser <- renderUI({
      req(data_lac())
      selectInput(ns("controller"), "Visualiser les données", 
                  choices = c(
                    "Lac" = "data_lac",
                    "Stations" = "data_station",
                    "Récolte" = "data_recolte",
                    "Spécimens" = "specimen_tous"
                  ), selected = NULL)
    })
    
    observeEvent(input$controller, {
      updateTabsetPanel(session = session, inputId = "switcher", selected = input$controller)
    })
    
    # ---- Tables ----
    output$table_lac <- renderDT(data_lac(), selection = "none", options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE))
    output$table_station <- renderDT(analysis_data()$data_station, selection = "none", options = list(searching = FALSE, lengthMenu = -1, lengthChange = FALSE, paging = FALSE))
    output$table_specimen_tous <- renderDT(analysis_data()$specimen_tous, selection = "none", options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE))
    output$table_data_recolte <- renderDT(analysis_data()$data_recolte, selection = "none", options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE))
    
    # ---- Nom de fichier ----
    filename_suffix <- reactive({
      generate_filename_suffix(
        typ_pech = input$typ_pech,
        annee = input$annee,
        no_lac = input$no_lac,
        nom_lac = nom_lac_reactif()
      )
    })
    
    analysis_label <- reactive({
      
      req(
        input$typ_pech,
        input$no_lac,
        nom_lac_reactif(),
        input$annee
      )
      
      generate_analysis_label(
        typ_pech = input$typ_pech,
        annee = input$annee,
        no_lac = input$no_lac,
        nom_lac = nom_lac_reactif()
      )
    })
    
    # ---- Retour du module ----
    return(list(
      data_lac = data_lac,
      data_recolte = reactive(analysis_data()$data_recolte),
      capture = reactive(analysis_data()$capture),
      specimen_tous = reactive(analysis_data()$specimen_tous),
      specimen_hasard_valide = reactive(analysis_data()$specimen_hasard_valide),
      specimen_valide = reactive(analysis_data()$specimen_valide),
      data_station = reactive(analysis_data()$data_station),
      station_valide = reactive(analysis_data()$station_valide),
      station_hasard_valide = reactive(analysis_data()$station_hasard_valide),
      filename_suffix = filename_suffix,
      analysis_label = analysis_label,
      nom_lac = nom_lac_reactif,
      info_pen = info_pen_reactive
    ))
  })
}

