#' Affiche un tableau flextable dans une application Shiny
#'
#' Cette fonction génère dynamiquement un élément `uiOutput()` affichant un objet `flextable` dans l’interface
#' d’une application Shiny. Le `flextable` est converti en HTML pour un rendu visuel propre.
#'
#' @param output_id Identifiant de sortie (`output`) utilisé dans `uiOutput()` pour afficher le tableau.
#' @param flextable_reactive Une expression `reactive()` qui retourne un objet `flextable`.
#'
#' @details
#' La fonction utilise `flextable::save_as_html()` pour convertir l’objet `flextable` en code HTML, 
#' qui est ensuite injecté dans l’UI via `renderUI()`.  
#' Elle suppose que l’appel est fait dans le contexte du serveur d’une application Shiny.
#'
#' @return Aucun objet retourné. La fonction affecte directement `output[[output_id]]` dans l’environnement serveur.
#'
#' @examples
#' \dontrun{
#' # Dans le serveur :
#' ft_resultats <- reactive({ mon_tableau %>% flextable::flextable() })
#' render_table_flextable("table_resultats", ft_resultats)
#'
#' # Dans l’UI :
#' uiOutput("table_resultats")
#' }
#'
#' @seealso [render_download_button()], [taille_masse_age()]
#' @export
render_table_flextable <- function(output_id, flextable_reactive) {
  output <- get("output", envir = parent.frame())
  
  output[[output_id]] <- renderUI({
    req(flextable_reactive())
    
    tmpfile <- tempfile(fileext = ".html")
    flextable::save_as_html(flextable_reactive(), path = tmpfile)
    htmltools::HTML(readLines(tmpfile, warn = FALSE))
  })
}


