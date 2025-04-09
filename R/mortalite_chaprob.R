#' Appliquer la méthode de Chapman-Robson pour estimer Z et A
#'
#' Cette fonction applique la méthode Chapman-Robson via `fishmethods::agesurv()`
#' et retourne un tableau résumant Z, A (%) et IC95%.
#'
#' @param specimen Un `data.frame` produit par `mortalite_prepare_corr()`, contenant une colonne `age`.
#' @param pp Valeur du peak-plus.
#' @param age_max Âge maximum observé.
#' @param format Format de sortie : `"data.frame"` (défaut) ou `"flextable"`.
#'
#' @return Un tableau résumé avec une seule ligne.
#' @export
#'
#' @examples
#' mortalite_chaprob(specimen, pp = 2, age_max = 8, format = "data.frame")
#' mortalite_chaprob(specimen, pp = 2, age_max = 8, format = "flextable")
mortalite_chaprob <- function(specimen, pp, age_max, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
  df <- subset(specimen, !is.na(age))
  
  res <- fishmethods::agesurv(
    type = 1,
    age = df$age,
    full = pp,
    last = age_max,
    estimate = "z",
    method = "cr"  # plus rapide et précis
  )
  
  result <- res$results |>
    dplyr::transmute(
      methode = "Chapman-Robson",
      z = Estimate,
      se = SE,
      A = round((1 - exp(-z)) * 100, 1),
      ic_95 = glue::glue("[{round((1 - exp(-(z - se))) * 100, 1)}-{round((1 - exp(-(z + se))) * 100, 1)}]")
    )
  
  if (format == "data.frame") return(result)
  
  # Format flextable
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
  
  return(ft)
}
