#' Déterminer l’âge Peak Plus à partir de la structure d’âge
#'
#' Cette fonction retourne l’âge le plus fréquent (le mode) dans la structure d’âge,
#' augmenté de 1 (selon la définition du Peak Plus).
#'
#' @param data Un `data.frame` contenant une colonne `age` et déjà filtré pour une seule espèce.
#'
#' @return Un entier correspondant au Peak Plus
#' @export
mortalite_get_peak_plus <- function(data) {
  if (!"age" %in% names(data)) stop("La colonne `age` est manquante.")
  if (nrow(data) == 0) return(NA_integer_)
  
  ages_clean <- na.omit(data$age)
  if (length(ages_clean) == 0) return(NA_integer_)
  
  peak <- ages_clean |> table() |> which.max() |> as.integer()
  peak_plus <- peak + 1
  
  return(peak_plus)
}
