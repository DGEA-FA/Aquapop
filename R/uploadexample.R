uploadexampleUI <- function(id) {
  tagList(
   
    actionButton(
      inputId = NS(id, "show"),
      label = "Voir un exemple de fichier"),
    
     bsModal(NS(id, "modalExample"),
            "Format des données à télécharger *.xlsx",
            NS(id, "show"),
            size = "large",
            uiOutput(NS(id, "mytabs"))
    )
   
  )
}

uploadexampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    sheets <- readxl::excel_sheets(path = "./data/exempledata.xlsx") # Extract the sheet names as a character string vector
    exemple <- purrr::map2("./data/exempledata.xlsx", sheets, ~ read_excel(path = .x, sheet = .y))
    
    output$mytabs <- renderUI({
      tabsetPanel(
        tabPanel("Instructions", renderText(instructions_upload)),
        tabPanel("Lac", renderDataTable(exemple[[1]], options = list(paging = FALSE, lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE))),
        tabPanel("Stations", renderDataTable(exemple[[2]], options = list(lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE))),
        tabPanel("Recolte", renderDataTable(exemple[[3]], options = list(lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE))),
        tabPanel("Specimens", renderDataTable(exemple[[4]], options = list(lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE))),
        tabPanel("Profil", renderDataTable(exemple[[5]], options = list(lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE))),
        tabPanel("Parametres", renderDataTable(exemple[[6]], options = list(lengthChange = FALSE, ordering = FALSE, scrollX = TRUE, pageLength = 20, autoWidth = TRUE, searching = FALSE)))
      )
    })
    
  })
}