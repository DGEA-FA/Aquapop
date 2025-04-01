#' Enregistre un data.frame en fichier Excel (.xlsx)
#'
#' @param data Un data.frame ou une liste de data.frames
#' @param path Chemin de sortie du fichier .xlsx
#'
#' @return NULL (fichier écrit sur disque)
#' @export
download_data <- function(data, path) {
  writexl::write_xlsx(
    x = data,
    path = path,
    col_names = TRUE,
    format_headers = TRUE
  )
}
