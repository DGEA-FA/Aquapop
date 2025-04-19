#' Affiche un graphique ggplot dans Shiny avec largeur dynamique
#'
#' @param output_id Identifiant du `plotOutput()`
#' @param plot_reactive Une expression `reactive()` qui retourne un objet ggplot
#' @param height Hauteur (px), par défaut 500
#' @param res Résolution (DPI)
#' @param message_si_vide Message à afficher si le graphique est vide ou NULL
#'
#' @export
render_plot_ggplot <- function(output_id, plot_reactive,
                                    height = 500, res = 96,
                                    message_si_vide = NULL) {
  output <- get("output", envir = parent.frame())
  session <- getDefaultReactiveDomain()
  
  output[[output_id]] <- renderPlot({
    p <- plot_reactive()
    
    if (is.null(p) || !inherits(p, "gg")) {
      if (!is.null(message_si_vide)) {
        showNotification(message_si_vide, type = "warning", duration = 5)
      }
      return(NULL)
    }
    
    if (length(p$layers) == 0 && !is.null(message_si_vide)) {
      showNotification(message_si_vide, type = "warning", duration = 5)
      return(NULL)
    }
    
    p
  },
  width = function() session$clientData[[paste0("output_", output_id, "_width")]],
  height = height,
  res = res
  )
}
