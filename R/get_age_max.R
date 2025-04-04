#' Trouver l’âge maximal dans les données
#'
#' Cette fonction retourne l’âge maximal observé, en ignorant les valeurs manquantes.
#'
#' @param data Un `data.frame` contenant une colonne `age`.
#'
#' @return Un entier correspondant à l’âge maximal
#' @export
#'
#' @examples
#' data_filtered <- dplyr::filter(specimen, sp == "SANA")
#' get_age_max(data_filtered)
get_age_max <- function(data) {
  if (!"age" %in% names(data)) stop("La colonne `age` est manquante.")
  if (nrow(data) == 0) return(NA_integer_)
  
  ages_clean <- na.omit(data$age)
  if (length(ages_clean) == 0) return(NA_integer_)
  
  max_age <- max(ages_clean)
  return(max_age)
}
