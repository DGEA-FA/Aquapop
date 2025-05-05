#' Déterminer l’âge Peak Plus à partir de la structure d’âge
#'
#' Cette fonction retourne l’âge le plus fréquent (le mode) dans la colonne `age`,
#' augmenté de 1, conformément à la définition du Peak Plus utilisée en analyse de mortalité.
#' Elle suppose que les données ont été filtrées au préalable pour une seule espèce.
#'
#' La fonction gère les cas particuliers suivants :
#' - Si plusieurs âges partagent la fréquence maximale, le plus petit âge est retenu.
#' - Les valeurs manquantes (`NA`) sont ignorées.
#' - Si toutes les valeurs sont manquantes ou si aucune ligne n'est présente, la fonction retourne `NA_integer_`.
#' - Une erreur est générée si la colonne `age` est absente.
#'
#' @param data Un `data.frame` contenant une colonne `age`, de type numérique ou entier.
#'
#' @return Un entier (`integer`) correspondant au Peak Plus, ou `NA_integer_` si le calcul est impossible.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' # Jeu de données simple avec un mode clair
#' df <- data.frame(age = c(1, 2, 2, 3, 3, 3, 4))
#' mortalite_get_peak_plus(df) # retourne 4 (mode = 3, +1)
#'
#' # Cas avec ex aequo : le plus petit est retenu
#' df <- data.frame(age = c(2, 2, 3, 3))
#' mortalite_get_peak_plus(df) # retourne 3
#'
#' # Données avec NA
#' df <- data.frame(age = c(NA, 2, 2, NA, 3))
#' mortalite_get_peak_plus(df) # retourne 3
#'
#' # Données vides ou toutes manquantes
#' mortalite_get_peak_plus(data.frame(age = numeric(0))) # retourne NA_integer_
#' mortalite_get_peak_plus(data.frame(age = c(NA, NA)))   # retourne NA_integer_
mortalite_get_peak_plus <- function(data) {
  if (!"age" %in% names(data)) stop("La colonne `age` est manquante.")
  if (nrow(data) == 0) return(NA_integer_)
  
  ages_clean <- na.omit(data$age)
  if (length(ages_clean) == 0) return(NA_integer_)
  
  tab <- table(ages_clean)
  mode_age <- as.integer(names(tab)[tab == max(tab)]) |> min()
  peak_plus <- mode_age + 1
  
  return(peak_plus)
}
