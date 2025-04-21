#' Appliquer la méthode de Chapman-Robson pour estimer Z et A
#'
#' Cette fonction applique la méthode Chapman-Robson via `fishmethods::agesurv()`
#' et retourne une liste contenant un tableau brut (`data.frame`) et une version formatée (`flextable`).
#'
#' @param specimen Un `data.frame` produit par `mortalite_prepare_corr()`, contenant une colonne `age`.
#' @param pp Valeur du peak-plus.
#' @param age_max Âge maximum observé.
#'
#' @return Une liste avec deux éléments : `data` (tableau brut), `flextable` (tableau formaté).
#' @export
#'
#' @examples
#' res <- mortalite_chaprob(specimen, pp = 2, age_max = 8)
#' res$data
#' res$flextable
mortalite_chaprob <- function(specimen, pp, age_max) {
  df <- subset(specimen, !is.na(age))
  
  if (nrow(df) == 0) {
    stop("Aucune donnée d’âge valide pour Chapman-Robson.")
  }
  if (length(unique(df$age)) < 2) {
    stop("La méthode Chapman-Robson nécessite au moins deux classes d’âge différentes.")
  }
  
  res <- fishmethods::agesurv(
    type = 1,
    age = df$age,
    full = pp,
    last = age_max,
    estimate = "z",
    method = "cr"
  )
  
  result <- res$results |>
    dplyr::transmute(
      methode = "Chapman-Robson",
      z = Estimate,
      se = SE,
      A = round((1 - exp(-z)) * 100, 1),
      ic_95 = glue::glue("[{round((1 - exp(-(z - se))) * 100, 1)}-{round((1 - exp(-(z + se))) * 100, 1)}]")
    )
  
  ft <- flextable::flextable(result) |>
    flextable::set_caption("Estimation de la mortalité par la méthode de Chapman-Robson") |>
    flextable::set_header_labels(
      methode = "Méthode",
      z = "Z (coefficient de mortalité)",
      se = "Erreur standard",
      A = "Taux de mortalité (A%)",
      ic_95 = "Intervalle de confiance à 95%"
    ) |>
    flextable::align(align = "center", part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::autofit()
  
  return(list(
    data = result,
    flextable = ft
  ))
}
