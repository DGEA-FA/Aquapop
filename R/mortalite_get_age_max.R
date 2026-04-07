#' Trouver l'âge maximal dans les données de mortalité
#'
#' Cette fonction détermine l'âge maximal observé dans les données de mortalité,
#' en ignorant les valeurs manquantes de la colonne `age`.
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si le calcul est impossible
#'   \item `value` : âge maximal observé, ou `NULL` si indisponible
#' }
#'
#' La fonction retourne `success = FALSE` dans les cas suivants :
#' \itemize{
#'   \item les données sont `NULL` ou ne sont pas un `data.frame`
#'   \item la colonne `age` est absente
#'   \item aucun âge valide n'est disponible
#' }
#'
#' @param data Un `data.frame` contenant les données de spécimens. Doit inclure
#'   une colonne nommée `age` de type numérique ou entier.
#'
#' @return Une liste avec les éléments suivants :
#' \describe{
#'   \item{success}{Un booléen indiquant si le calcul a réussi.}
#'   \item{message}{Un message informatif si le calcul est impossible, sinon `NULL`.}
#'   \item{value}{Un entier correspondant à l'âge maximal observé, ou `NULL` si le calcul est impossible.}
#' }
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
#' mortalite_get_age_max(data.frame(other = 1:5))
#'
#' @export
mortalite_get_age_max <- function(data) {
  # Validation de base ====
  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      success = FALSE,
      message = "Les données fournies sont invalides.",
      value = NULL
    ))
  }
  
  if (!"age" %in% names(data)) {
    return(list(
      success = FALSE,
      message = "La colonne `age` est absente des données.",
      value = NULL
    ))
  }
  
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun spécimen n'est disponible pour déterminer l'âge maximal.",
      value = NULL
    ))
  }
  
  # Nettoyage des âges ====
  ages_clean <- na.omit(data$age)
  
  if (length(ages_clean) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun âge valide n'est disponible pour déterminer l'âge maximal.",
      value = NULL
    ))
  }
  
  # Sortie ====
  list(
    success = TRUE,
    message = NULL,
    value = max(ages_clean)
  )
}