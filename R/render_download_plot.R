#' Crée un bouton de téléchargement pour un graphique ggplot
#'
#' @param id Identifiant pour le bouton
#' @param plot_reactive Expression `reactive()` retournant un objet ggplot
#' @param filename Nom du fichier de sortie (sans l'extension)
#' @param width Largeur du PNG (en pouces)
#' @param height Hauteur du PNG (en pouces)
#' @param dpi Résolution
#' @param label Texte affiché sur le bouton
#'
#' @export
render_download_plot <- function(id,
                                        plot_reactive,
                                        filename = NULL,
                                        width = 7, height = 5, dpi = 300,
                                        label = "Téléchargement du graphique") {
  output <- get("output", envir = parent.frame())
  
  if (is.null(filename)) filename <- id
  
  output[[id]] <- downloadHandler(
    filename = function() paste0(filename, ".png"),
    content = function(file) {
      req(plot_reactive())
      ggsave(file,
             plot = plot_reactive(),
             width = width,
             height = height,
             dpi = dpi,
             device = "png")
    }
  )
}
