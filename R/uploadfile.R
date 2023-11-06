uploadfile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Telechargement des donnees xlsx
    fileInput(ns("upload"), "Téléchargez vos données (*.xlsx)", buttonLabel = "Téléchargement...", multiple = FALSE, accept = c(".xlsx"))
  )
}

uploadfile_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    # We return a reactive function from this server, 
    # that can be passed along to other modules
    return(
      reactive({
        input$upload
      })
    )
  }
  )
}
