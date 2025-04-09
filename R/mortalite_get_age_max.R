#' Trouver l’âge maximal dans les données de mortalité
#'
#' Cette fonction retourne l’âge maximal observé dans les données de mortalité, en ignorant les valeurs manquantes.
#'
#' @param data Un `data.frame` contenant une colonne `age`.
#'
#' @return Un entier correspondant à l’âge maximal
#' @export
mortalite_get_age_max <- function(data) {
  if (!"age" %in% names(data)) stop("La colonne `age` est manquante.")
  if (nrow(data) == 0) return(NA_integer_)
  
  ages_cleaned <- na.omit(data$age)
  if (length(ages_cleaned) == 0) return(NA_integer_)
  
  age_max <- max(ages_cleaned)
  return(age_max)
}
