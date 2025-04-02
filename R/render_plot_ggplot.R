#' Affiche un graphique ggplot dans Shiny
#'
#' @param output_id Identifiant du `plotOutput()`
#' @param plot_reactive Une expression `reactive()` qui retourne un objet ggplot
#' @param width Largeur (px)
#' @param height Hauteur (px)
#' @param res Résolution
#'
#' @export
render_plot_ggplot <- function(output_id, plot_reactive,
                             width = 600, height = 400, res = 96) {
  output <- get("output", envir = parent.frame())
  
  output[[output_id]] <- renderPlot({
    req(plot_reactive())
    plot_reactive()
  }, width = width, height = height, res = res)
}
