#' Appliquer la méthode de Chapman-Robson pour estimer Z et A
#'
#' Cette fonction applique la méthode Chapman-Robson via `fishmethods::agesurv()` pour estimer
#' le coefficient de mortalité (Z) à partir d'une distribution d'âge, puis calcule le taux de mortalité (A).
#' Elle retourne à la fois un tableau brut (`data.frame`) et une version formatée (`flextable`).
#'
#' @param specimen Un `data.frame` produit par `mortalite_prepare_corr()`, contenant une colonne `age`.
#' @param pp Valeur de "peak-plus", soit la classe d’âge au-delà de laquelle les âges sont regroupés.
#' @param age_max Âge maximum à considérer dans le calcul.
#'
#' @return Une liste avec deux éléments :
#' \describe{
#'   \item{data}{Un `data.frame` contenant les estimations de mortalité. Colonnes :  `z`, `se`, `a`, `ic_95`}
#'   \item{flextable}{Une version formatée du tableau, prête à être affichée ou exportée}
#' }
#'
#' @importFrom fishmethods agesurv
#' @importFrom dplyr transmute
#' @importFrom glue glue
#' @importFrom flextable flextable set_caption set_header_labels
#' @export
#'
#' @examples
#' fake_data <- data.frame(age = c(2, 3, 3, 4, 4, 4, 5, 5, 6))
#' mort <- mortalite_chaprob(fake_data, pp = 3, age_max = 6)
#' mort$data
#' mort$flextable
mortalite_chaprob <- function(specimen, pp, age_max) {
  
  # Validation des arguments ----
  if (!"age" %in% colnames(specimen)) {
    stop("La colonne `age` est requise dans le jeu de données `specimen`.")
  }
  if (!is.numeric(pp) || length(pp) != 1 || pp <= 0) {
    stop("L'argument `pp` doit être un numérique strictement positif.")
  }
  if (!is.numeric(age_max) || length(age_max) != 1 || age_max <= 0) {
    stop("L'argument `age_max` doit être un numérique strictement positif.")
  }
  
  # Préparation des données ----
  data_age <- subset(specimen, !is.na(age))
  
  if (nrow(data_age) == 0) {
    stop("Aucune donnée d’âge valide pour Chapman-Robson.")
  }
  if (length(unique(data_age$age)) < 2) {
    stop("La méthode Chapman-Robson nécessite au moins deux classes d’âge différentes.")
  }
  
  # Application de la méthode Chapman-Robson ----
  res_cr <- agesurv(
    type     = 1,
    age      = data_age$age,
    full     = pp,
    last     = age_max,
    estimate = "z",
    method   = "cr"
  )
  
  # Construction du tableau brut ----
  table_resultats <- res_cr$results |>
    transmute(
      z       = .data$Estimate,
      se      = .data$SE,
      a       = round((1 - exp(-z)) * 100, 1),
      ic_95   = glue("[{round((1 - exp(-(z - se))) * 100, 1)}-{round((1 - exp(-(z + se))) * 100, 1)}]")
    )
  
  # Mise en forme flextable ----
  tableau_formate <- flextable(table_resultats) |>
    set_caption("Estimation de la mortalité par la méthode de Chapman-Robson") |>
    set_header_labels(
      z       = "Z (coefficient de mortalité)",
      se      = "Erreur standard",
      a       = "Taux de mortalité (A%)",
      ic_95   = "Intervalle de confiance à 95%"
    ) |>
    style_flextable_aquapop()
  
  # Résultat ----
  return(list(
    data      = table_resultats,
    flextable = tableau_formate
  ))
}
