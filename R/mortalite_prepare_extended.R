#' Étendre artificiellement les données de fréquence d'âge avec des zéros
#'
#' Cette fonction ajoute des classes d'âge fictives avec un nombre de captures nul
#' (`number = 0`) au-delà de l'âge maximal observé, jusqu'à trois fois cet âge.
#' Cette étape permet d'améliorer l'ajustement de certains modèles de mortalité.
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si la préparation est impossible
#'   \item `data` : tableau étendu des fréquences d'âge, ou `NULL` si indisponible
#' }
#'
#' @param df_corrigee Un `data.frame` contenant les colonnes `age` (âge) et
#'   `number` (fréquence), généralement produit par `mortalite_prepare_corr()`.
#' @param age_max Un entier indiquant l'âge maximal observé, généralement issu
#'   de `mortalite_get_age_max()`.
#'
#' @return Une liste avec les éléments suivants :
#' \describe{
#'   \item{success}{Un booléen indiquant si la préparation a réussi.}
#'   \item{message}{Un message informatif si la préparation est impossible, sinon `NULL`.}
#'   \item{data}{Un `data.frame` combinant les âges observés avec les âges fictifs ajoutés, ou `NULL` si la préparation est impossible.}
#' }
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' df <- data.frame(age = 2:5, number = c(4, 3, 2, 1))
#' mortalite_prepare_extended(df, age_max = 5)
mortalite_prepare_extended <- function(df_corrigee, age_max) {
  # Validation de base ====
  if (is.null(df_corrigee) || !is.data.frame(df_corrigee)) {
    return(list(
      success = FALSE,
      message = "Les données corrigées sont invalides.",
      data = NULL
    ))
  }
  
  if (nrow(df_corrigee) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune donnée corrigée n'est disponible pour étendre la structure d'âge.",
      data = NULL
    ))
  }
  
  if (!all(c("age", "number") %in% names(df_corrigee))) {
    return(list(
      success = FALSE,
      message = "Le tableau `df_corrigee` doit contenir les colonnes `age` et `number`.",
      data = NULL
    ))
  }
  
  if (is.null(age_max) || length(age_max) != 1 || is.na(age_max)) {
    return(list(
      success = FALSE,
      message = "L'âge maximal (`age_max`) est invalide ou manquant.",
      data = NULL
    ))
  }
  
  if (!is.numeric(age_max) || age_max < 0) {
    return(list(
      success = FALSE,
      message = "L'âge maximal (`age_max`) doit être un nombre supérieur ou égal à 0.",
      data = NULL
    ))
  }
  
  # Cas limite : aucun âge fictif à ajouter (ex: si age_max = 0) ====
  if (age_max * 3 <= age_max) {
    return(list(
      success = TRUE,
      message = NULL,
      data = df_corrigee |>
        arrange(.data$age)
    ))
  }
  
  # Génération des âges fictifs ====
  ages_fictifs <- tibble(
    age = (age_max + 1):(age_max * 3),
    number = 0
  )
  
  # Fusion et tri ====
  df_etendue <- bind_rows(df_corrigee, ages_fictifs) |>
    arrange(.data$age)
  
  list(
    success = TRUE,
    message = NULL,
    data = df_etendue
  )
}