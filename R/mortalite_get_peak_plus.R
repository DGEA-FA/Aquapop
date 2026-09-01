#' Déterminer l'âge Peak Plus (âge de départ) à partir de la structure d'âge
#'
#' Cette fonction détermine l'âge de départ (`peak plus`) utilisé dans l'analyse
#' de mortalité à partir de la structure d'âge observée. Elle identifie l'âge le
#' plus fréquent (le mode) dans la colonne `age`, puis ajoute 1 à cette valeur.
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si le calcul est impossible
#'   \item `value` : valeur numérique de l'âge Peak Plus, ou `NULL` si indisponible
#' }
#'
#' Règles appliquées :
#' \itemize{
#'   \item les valeurs manquantes (`NA`) sont ignorées
#'   \item si plusieurs âges partagent la fréquence maximale, le plus petit âge est retenu
#'   \item si aucune donnée exploitable n'est disponible, la fonction retourne
#'   `success = FALSE` et `value = NULL`
#'   \item si la colonne `age` est absente, la fonction retourne aussi
#'   `success = FALSE`
#' }
#'
#' @param data Un `data.frame` contenant une colonne `age`, de type numérique ou entier.
#'
#' @return Une liste avec les éléments suivants :
#' \describe{
#'   \item{success}{Un booléen indiquant si le calcul a réussi.}
#'   \item{message}{Un message informatif si le calcul est impossible, sinon `NULL`.}
#'   \item{value}{Un entier correspondant au Peak Plus, ou `NULL` si le calcul est impossible.}
#' }
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' # Jeu de données simple avec un mode clair
#' df <- data.frame(age = c(1, 2, 2, 3, 3, 3, 4))
#' mortalite_get_peak_plus(df)
#'
#' # Cas avec ex aequo : le plus petit âge est retenu
#' df <- data.frame(age = c(2, 2, 3, 3))
#' mortalite_get_peak_plus(df)
#'
#' # Données avec NA
#' df <- data.frame(age = c(NA, 2, 2, NA, 3))
#' mortalite_get_peak_plus(df)
#'
#' # Données vides ou toutes manquantes
#' mortalite_get_peak_plus(data.frame(age = numeric(0)))
#' mortalite_get_peak_plus(data.frame(age = c(NA, NA)))
mortalite_get_peak_plus <- function(data, sp) {

    # Validation des données ====
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
      message = "Aucun spécimen n'est disponible pour déterminer l'âge Peak Plus.",
      value = NULL
    ))
  }
  
  # Validation de l'espèce
  if (is.null(sp) ||
      !is.character(sp) ||
      length(sp) != 1 ||
      is.na(sp) ||
      sp == "") {
    
    return(list(
      success = FALSE,
      message = "Le code d'espèce n'est pas valide.",
      value = NULL
    ))
  }
  
  # Déterminer l'ajustement selon l'espèce
  if (sp == "SAFO") {
    increment <- 0L
  } else if (sp %in% c("SANA", "SAVI")) {
    increment <- 1L
  } else {
    return(list(
      success = FALSE,
      message = paste0(
        "Aucune règle Peak Plus n'est définie pour l'espèce `",
        sp,
        "`."
      ),
      value = NULL
    ))
  }
  
  # Nettoyage des âges ====
  ages_clean <- na.omit(data$age)
  
  if (length(ages_clean) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun âge valide n'est disponible pour déterminer l'âge Peak Plus.",
      value = NULL
    ))
  }
  
  # Calcul du mode et du peak plus ====
  age_table <- table(ages_clean)
  mode_age <- as.integer(names(age_table)[age_table == max(age_table)]) |> min()
  
  peak_plus <- mode_age + increment
  
  # Sortie ====
  list(
    success = TRUE,
    message = NULL,
    value = peak_plus,
    sp = sp,
    mode_age = mode_age
  )
}