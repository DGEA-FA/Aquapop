#' Trouver l’âge maximal dans les données de mortalité
#'
#' Cette fonction retourne l’âge maximal observé dans les données de mortalité, en ignorant les valeurs manquantes.
#' Elle vérifie également la présence de la colonne `age` et la présence de données valides.
#'
#' @param data Un `data.frame` contenant les données de spécimens. Doit inclure une colonne nommée `age` de type numérique ou entier.
#'
#' @return Un entier (`integer`) correspondant à l’âge maximal observé. Retourne `NA_integer_` si aucun âge valide n’est présent.
#'
#' @examples
#' # Exemple avec des âges valides
#' df <- data.frame(age = c(1, 2, 3, NA, 4))
#' mortalite_get_age_max(df)
#'
#' # Exemple sans âges valides
#' df2 <- data.frame(age = c(NA, NA))
#' mortalite_get_age_max(df2)
#'
#' # Exemple sans colonne 'age'
#' try(mortalite_get_age_max(data.frame(other = 1:5)))
#'
#' @export
mortalite_get_age_max <- function(data) {
  # Validations robustes avec checkmate
  checkmate::assert_data_frame(data, min.rows = 0, col.names = "named")
  checkmate::assert_subset("age", colnames(data), empty.ok = FALSE)
  
  # Extraire les âges sans NA
  ages_cleaned <- na.omit(data$age)
  
  # Vérifier qu’il reste des valeurs valides
  if (length(ages_cleaned) == 0) {
    return(NA_integer_)
  }
  
  # Retourner l’âge maximal
  return(max(ages_cleaned))
}
