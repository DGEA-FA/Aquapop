#' Crée un bouton de téléchargement conditionnel pour un data.frame (.xlsx)
#'
#' Cette fonction crée à la fois l’élément UI (`uiOutput`) et la logique de téléchargement
#' pour un bouton `downloadButton()` qui exporte un `data.frame` au format Excel.
#' Le bouton n’apparaît que si les données sont prêtes (`req`).
#'
#' @param id Identifiant de base du bouton (ex: "dl_resultats"). Le `uiOutput` correspondant
#'        sera nommé automatiquement `paste0(id, "_ui")`.
#' @param data_reactive Une expression `reactive()` retournant un `data.frame`.
#' @param filename Nom du fichier `.xlsx` à télécharger. Par défaut : `id`.
#' @param label Texte à afficher sur le bouton. Par défaut : "Télécharger (.xlsx)".
#'
#' @export
render_download_button_ui <- function(id,
                                      data_reactive,
                                      filename = NULL,
                                      label = "Télécharger (.xlsx)") {
  output <- get("output", envir = parent.frame())
  
  if (is.null(filename)) filename <- id
  
  # 1. Affichage conditionnel du bouton via uiOutput
  output[[paste0(id, "_ui")]] <- renderUI({
    req(data_reactive)
    downloadButton(id, label)
  })
  
  # 2. Logique de téléchargement
  output[[id]] <- downloadHandler(
    filename = function() paste0(filename, ".xlsx"),
    content = function(file) {
      req(data_reactive)
      download_data(data_reactive, file)
    }
  )
}
