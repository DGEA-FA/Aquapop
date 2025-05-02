# =============================================================================
# Utilitaires pour affichage et téléchargement dans une application Shiny
# Projet AquaPop – Fonctions modulaires compatibles avec ou sans Shiny
# =============================================================================

#' Convertit un objet en reactive() si ce n’est pas déjà le cas
#' @noRd
as_reactive <- function(x) {
  if (inherits(x, "reactive")) x else reactive(x)
}

#' Affiche un tableau flextable dans une application Shiny
#'
#' @param output_id Identifiant utilisé dans `uiOutput()`
#' @param flextable Une expression, un objet `flextable`, ou une fonction `reactive()`
#'
#' @export
render_table_flextable <- function(output_id, flextable) {
  output <- get("output", envir = parent.frame())
  
  # Crée une fonction réactive différee quelle que soit l'entrée
  get_ft <- as_reactive(force_lazy(flextable))
  
  output[[output_id]] <- renderUI({
    ft <- get_ft()
    req(ft)
    
    tmpfile <- tempfile(fileext = ".html")
    flextable::save_as_html(ft, path = tmpfile)
    htmltools::HTML(readLines(tmpfile, warn = FALSE))
  })
}

#' Force une évaluation différée (lazy) même pour obj()$x
#' @noRd
force_lazy <- function(expr) {
  if (inherits(expr, "reactive")) return(expr)
  if (is.function(expr)) return(expr)
  force_expr <- substitute(expr)
  reactive(eval(force_expr, envir = parent.frame()))
}



#' Affiche un graphique ggplot dans Shiny avec largeur dynamique
#'
#' @param output_id Identifiant de `plotOutput()`
#' @param plot Objet ggplot ou `reactive()` le retournant
#' @param height Hauteur en pixels
#' @param res Résolution en DPI
#' @param message_si_vide Message à afficher si graphique vide ou invalide
#'
#' @export
render_plot_ggplot <- function(output_id, plot,
                               height = 500, res = 96,
                               message_si_vide = NULL) {
  output <- get("output", envir = parent.frame())
  session <- getDefaultReactiveDomain()
  get_plot <- as_reactive(plot)
  
  output[[output_id]] <- renderPlot({
    p <- get_plot()
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

#' Crée un bouton de téléchargement pour un tableau (.xlsx)
#'
#' @param id Identifiant du bouton (ex: "masselongueur_dl")
#' @param data Un objet `data.frame` ou un `reactive()` le retournant
#' @param filename Nom du fichier à enregistrer (.xlsx). Peut être une chaîne ou un `reactive()`
#' @param label (non utilisé ici – texte défini dans le `ui`)
#'
#' @export
render_download_table <- function(id,
                                  data,
                                  filename = NULL,
                                  label = NULL) {
  
  output <- get("output", envir = parent.frame())
  
  get_data     <- if (inherits(data, "reactive"))     data     else reactive(data)
  get_filename <- if (inherits(filename, "reactive")) filename else reactive(filename)
  
  output[[id]] <- downloadHandler(
    filename = function() get_filename(),
    content  = function(file) {
      writexl::write_xlsx(get_data(), path = file)
    }
  )
}



#' Crée un bouton de téléchargement pour un graphique ggplot
#'
#' @param id Identifiant du bouton
#' @param plot Objet ggplot ou `reactive()` le retournant
#' @param filename Nom complet du fichier PNG (sans extension)
#' @param filename_suffix Suffixe à ajouter automatiquement (ex: `"PENT_2022_LacX"`)
#' @param width Largeur (en pouces)
#' @param height Hauteur (en pouces)
#' @param dpi Résolution (DPI)
#' @param label Libellé du bouton
#' 
#' @importFrom ggplot2 ggsave 
#'
#' @export
render_download_plot <- function(id,
                                 plot,
                                 filename = NULL,
                                 filename_suffix = NULL,
                                 width = 7, height = 5, dpi = 300,
                                 label = "Télécharger le graphique") {
  output <- get("output", envir = parent.frame())
  get_plot <- as_reactive(plot)
  
  get_filename <- reactive({
    if (!is.null(filename)) {
      filename
    } else if (!is.null(filename_suffix)) {
      suffix <- if (inherits(filename_suffix, "reactive")) filename_suffix() else filename_suffix
      paste0("figure_", suffix)
    } else {
      id
    }
  })
  
  output[[id]] <- downloadHandler(
    filename = function() paste0(get_filename(), ".png"),
    content = function(file) {
      ggsave(file,
             plot = get_plot(),
             width = width,
             height = height,
             dpi = dpi,
             device = "png")
    }
  )
}

#' Crée un bouton de téléchargement simple
#'
#' @param id Identifiant du bouton (doit correspondre à celui utilisé dans `render_download_*`)
#' @param label Texte affiché (défaut : "Télécharger (.xlsx)")
#'
#' @return Un bouton téléchargeable (à insérer dans `app_ui()`)
#' @export
download_button_ui <- function(id, label = "Télécharger (.xlsx)") {
  shiny::downloadButton(outputId = id, label = label)
}

#' Rendre le fichier user_guide.rmd en HTML (si nécessaire)
#'
#' Vérifie si le fichier HTML est plus vieux que le .Rmd, et le recompilé si besoin.
#' Utile pour afficher dans l'app Shiny via iframe.
#'
#' @param rmd_path Chemin vers le fichier .Rmd
#' @param html_path Chemin vers le fichier .html à générer
render_user_guide_if_needed <- function(rmd_path = "texte/user_guide.rmd",
                                        html_path = "www/user_guide.html") {
  if (!file.exists(html_path) ||
      file.info(rmd_path)$mtime > file.info(html_path)$mtime) {
    
    message("🛠️  Rendu de user_guide.rmd → HTML...")
    
    rmarkdown::render(
      input = rmd_path,
      output_file = basename(html_path),
      output_dir = dirname(html_path),
      quiet = TRUE
    )
  }
}


