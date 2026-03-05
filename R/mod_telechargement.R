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
          tabPanelBody("specimen", DTOutput(ns("table_specimen"))),
          tabPanelBody("specimen_valide", DTOutput(ns("table_specimen_valid"))),
          tabPanelBody("capture", DTOutput(ns("table_capture")))
        )
      )
    )
  )
}


#' Server - Module de téléchargement des données
#'
#' @param id Identifiant du module
#'
#' @return Une liste de réactifs : data_lac, capture, specimen, specimen_valide,
#' data_station, station_valides, station_hasard_valide, filename_suffix, nom_lac
#' @noRd
mod_telechargement_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_temp <- eventReactive(input$upload, {
      load_lac(path = input$upload$datapath, namesheet = "Lac", verbose = FALSE)
    })
    
    output$ui_typ_pech <- renderUI({
      req(data_temp())
      radioButtons(ns("typ_pech"), "Sélectionner le type de pêche normalisée",
                   choices = unique(data_temp()$typ_pech), selected = character(0))
    })
    
    df_filtered1 <- reactive({
      req(data_temp(), input$typ_pech)
      filter_by_pen_lac_annee(data_temp(), typ_pech = input$typ_pech)
    })
    
    output$ui_no_lac <- renderUI({
      req(df_filtered1())
      selectInput(ns("no_lac"), "Sélectionner le numéro de lac",
                  choices = sort(unique(df_filtered1()$no_lac)), selected = NULL)
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
        p("Si plus d’une année d’inventaire est sélectionnée...",
          style = "font-size: 85%; color: #555;")
      )
    })
    
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
    
    analysis_data <- reactive({
      req(input$upload, input$typ_pech, input$no_lac, input$annee)
      get_analysis_data(
        path = input$upload$datapath,
        typ_pech = input$typ_pech,
        no_lac = input$no_lac,
        annee = input$annee,
        verbose = FALSE
      )
    })
    
    output$recap_intro_table <- renderTable({
      req(data_lac(), analysis_data()$data_station)
      generate_recapitulatif_inventaire(data_lac(), analysis_data()$data_station)
    })
    
    output$visualiser <- renderUI({
      req(data_lac())
      selectInput(ns("controller"), "Visualiser les données", 
                  choices = c(
                    "Lac" = "data_lac",
                    "Stations" = "data_station",
                    "Spécimens" = "specimen",
                    "Spécimens valides" = "specimen_valide",
                    "Capture" = "capture"
                  ), selected = NULL)
    })
    
    observeEvent(input$controller, {
      updateTabsetPanel(session = session, inputId = "switcher", selected = input$controller)
    })
    
    output$table_lac <- renderDT(data_lac())
    output$table_station <- renderDT(analysis_data()$data_station)
    output$table_specimen <- renderDT(analysis_data()$specimen)
    output$table_specimen_valid <- renderDT(analysis_data()$specimen_valide)
    output$table_capture <- renderDT(analysis_data()$capture)
    
    filename_suffix <- reactive({
      generate_filename_suffix(
        typ_pech = input$typ_pech,
        annee = input$annee,
        no_lac = input$no_lac,
        nom_lac = nom_lac_reactif()
      )
    })
    
    return(list(
      data_lac = data_lac,
      capture = reactive(analysis_data()$capture),
      specimen = reactive(analysis_data()$specimen),
      specimen_valide = reactive(analysis_data()$specimen_valide),
      data_station = reactive(analysis_data()$data_station),
      station_valides = reactive(analysis_data()$station_valides),
      station_hasard_valide = reactive(analysis_data()$station_hasard_valide),
      filename_suffix = filename_suffix,
      nom_lac = nom_lac_reactif
    ))
  })
}

