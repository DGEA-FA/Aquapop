#' Préparer les données corrigées de fréquence d'âge pour l'estimation de la mortalité
#'
#' Cette fonction applique `agesurv()` avec `type = 1` pour générer les données
#' individuelles de fréquence d'âge sur la partie descendante de la courbe de capture.
#' Elle filtre d'abord les spécimens ayant un âge valide, applique l'estimation,
#' puis complète les classes d'âge manquantes avec des zéros pour produire une
#' table uniforme.
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si la préparation est impossible
#'   \item `data` : tableau de fréquences corrigées, ou `NULL` si indisponible
#' }
#'
#' @param data Un `data.frame` contenant les spécimens d'une seule espèce, avec
#'   une colonne nommée `age`. Les valeurs manquantes (`NA`) sont automatiquement exclues.
#' @param age_peak_plus Un entier indiquant l'âge à partir duquel commence
#'   l'analyse de mortalité.
#' @param age_max Un entier indiquant l'âge maximum à considérer pour
#'   l'estimation de la mortalité.
#'
#' @return Une liste avec les éléments suivants :
#' \describe{
#'   \item{success}{Un booléen indiquant si la préparation a réussi.}
#'   \item{message}{Un message informatif si la préparation est impossible, sinon `NULL`.}
#'   \item{data}{Un `data.frame` contenant les colonnes `age` et `number`, ou `NULL` si la préparation est impossible.}
#' }
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom fishmethods agesurv
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#'
#' @export
#'
#' @examples
#' data_exemple <- data.frame(age = c(2, 3, 3, 4, 5, 5, 5, 6, 7, NA))
#' mortalite_prepare_corr(data = data_exemple, age_peak_plus = 5, age_max = 7)
mortalite_prepare_corr <- function(data, age_peak_plus, age_max) {
  # Validation de base ====
  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      success = FALSE,
      message = "Les données fournies sont invalides.",
      data = NULL
    ))
  }
  
  if (!"age" %in% names(data)) {
    return(list(
      success = FALSE,
      message = "La colonne `age` est absente des données.",
      data = NULL
    ))
  }
  
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun spécimen n'est disponible pour préparer les données de mortalité.",
      data = NULL
    ))
  }
  
  if (is.null(age_peak_plus) || length(age_peak_plus) != 1 || is.na(age_peak_plus)) {
    return(list(
      success = FALSE,
      message = "L'âge de départ (`age_peak_plus`) est invalide ou manquant.",
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
  
  if (!is.numeric(age_peak_plus) || !is.numeric(age_max)) {
    return(list(
      success = FALSE,
      message = "Les arguments `age_peak_plus` et `age_max` doivent être numériques.",
      data = NULL
    ))
  }
  
  if (age_peak_plus < 0 || age_max < 0) {
    return(list(
      success = FALSE,
      message = "Les âges doivent être supérieurs ou égaux à 0.",
      data = NULL
    ))
  }
  
  # Nettoyage des âges ====
  data_valid <- data |>
    filter(!is.na(.data$age))
  
  if (nrow(data_valid) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune valeur d'âge valide n'est disponible après suppression des NA.",
      data = NULL
    ))
  }
  
  age_observe_max <- max(data_valid$age)
  
  if (age_peak_plus > age_observe_max) {
    return(list(
      success = FALSE,
      message = "`age_peak_plus` est supérieur à l'âge maximum observé dans les données.",
      data = NULL
    ))
  }
  
  if (age_peak_plus >= age_max) {
    return(list(
      success = FALSE,
      message = paste(
        "L'âge de départ doit être strictement inférieur à l'âge maximal.",
        "Veuillez choisir manuellement une valeur de `age_peak_plus` plus petite."
      ),
      data = NULL
    ))
  }
  
  # Estimation avec agesurv ====
  resultat <- tryCatch(
    agesurv(
      type = 1,
      age = data_valid$age,
      full = age_peak_plus,
      last = age_max,
      estimate = "z",
      method = "cr"
    ),
    error = function(e) NULL
  )
  
  if (is.null(resultat) || is.null(resultat$data)) {
    return(list(
      success = FALSE,
      message = "La préparation des données de mortalité avec `agesurv()` a échoué.",
      data = NULL
    ))
  }
  
  data_agesurv <- resultat$data
  
  if (!is.data.frame(data_agesurv) || nrow(data_agesurv) < 2) {
    return(list(
      success = FALSE,
      message = paste(
        "L'ajustement `agesurv()` a retourné un jeu de données trop incomplet",
        "(moins de 2 âges exploitables)."
      ),
      data = NULL
    ))
  }
  
  # Complétion des classes d'âge ====
  data_final <- tibble(age = seq(min(data_agesurv$age), max(data_agesurv$age))) |>
    left_join(data_agesurv, by = "age") |>
    mutate(number = replace_na(.data$number, 0L)) |>
    arrange(.data$age)
  
  list(
    success = TRUE,
    message = NULL,
    data = data_final
  )
}