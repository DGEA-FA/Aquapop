#' Affiche un graphique ggplot dans Shiny avec gestion des cas vides
#'
#' @param output_id Identifiant du `plotOutput()`
#' @param plot_reactive Une expression `reactive()` qui retourne un objet ggplot
#' @param width Largeur (px)
#' @param height Hauteur (px)
#' @param res Résolution (DPI)
#' @param message_si_vide Message à afficher si le graphique est vide ou NULL (optionnel)
#'
#' @export
render_plot_ggplot <- function(output_id, plot_reactive,
                               width = 600, height = 400, res = 96,
                               message_si_vide = NULL) {
  output <- get("output", envir = parent.frame())
  
  output[[output_id]] <- renderPlot({
    p <- plot_reactive()
    
    # Graphique manquant ou vide
    if (is.null(p) || !inherits(p, "gg")) {
      if (!is.null(message_si_vide)) {
        showNotification(message_si_vide, type = "warning", duration = 5)
      }
      return(NULL)
    }
    
    # Graphique sans couches (ex: ggplot vide)
    if (length(p$layers) == 0 && !is.null(message_si_vide)) {
      showNotification(message_si_vide, type = "warning", duration = 5)
      return(NULL)
    }
    
    p
  }, width = width, height = height, res = res)
}
