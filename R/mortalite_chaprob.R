#' Appliquer la méthode de Chapman-Robson pour estimer Z et A
#'
#' Cette fonction applique la méthode Chapman-Robson via
#' `fishmethods::agesurv()` pour estimer le coefficient de mortalité (`Z`) à
#' partir d'une distribution d'âge, puis calcule le taux de mortalité (`A`).
#' Elle retourne à la fois un tableau brut (`data.frame`) et une version
#' formatée (`flextable`).
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si le calcul est impossible
#'   \item `data` : tableau brut des estimations, ou `NULL`
#'   \item `flextable` : tableau formaté, ou `NULL`
#' }
#'
#' @param specimen Un `data.frame` contenant une colonne `age`.
#' @param pp Valeur de "peak-plus", soit la classe d'âge au-delà de laquelle les
#'   âges sont regroupés.
#' @param age_max Âge maximum à considérer dans le calcul.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Un booléen indiquant si l'estimation a réussi.}
#'   \item{message}{Un message informatif si l'estimation est impossible, sinon `NULL`.}
#'   \item{data}{Un `data.frame` contenant les colonnes `z`, `se`, `a`, `ic_95`, ou `NULL`.}
#'   \item{flextable}{Une version formatée du tableau, prête à être affichée ou exportée, ou `NULL`.}
#' }
#'
#' @importFrom fishmethods agesurv
#' @importFrom dplyr transmute filter
#' @importFrom flextable flextable set_caption set_header_labels
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' fake_data <- data.frame(age = c(2, 3, 3, 4, 4, 4, 5, 5, 6))
#' mort <- mortalite_chaprob(fake_data, pp = 3, age_max = 6)
#' mort$data
#' mort$flextable
mortalite_chaprob <- function(specimen, pp, age_max) {
  # Validation de base ====
  if (is.null(specimen) || !is.data.frame(specimen) || nrow(specimen) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune donnée n'est disponible pour appliquer la méthode de Chapman-Robson.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (!"age" %in% names(specimen)) {
    return(list(
      success = FALSE,
      message = "La colonne `age` est requise pour appliquer la méthode de Chapman-Robson.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (is.null(pp) || length(pp) != 1 || is.na(pp) || !is.numeric(pp) || pp <= 0) {
    return(list(
      success = FALSE,
      message = "L'argument `pp` doit être un numérique strictement positif.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (is.null(age_max) || length(age_max) != 1 || is.na(age_max) || !is.numeric(age_max) || age_max <= 0) {
    return(list(
      success = FALSE,
      message = "L'argument `age_max` doit être un numérique strictement positif.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (pp >= age_max) {
    return(list(
      success = FALSE,
      message = "La méthode Chapman-Robson requiert un `pp` strictement inférieur à `age_max`.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Préparation des données ====
  data_age <- specimen |>
    filter(!is.na(.data$age))
  
  if (nrow(data_age) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune donnée d'âge valide n'est disponible pour Chapman-Robson.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (length(unique(data_age$age)) < 2) {
    return(list(
      success = FALSE,
      message = "La méthode Chapman-Robson nécessite au moins deux classes d'âge différentes.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Application de la méthode Chapman-Robson ====
  res_cr <- tryCatch(
    agesurv(
      type = 1,
      age = data_age$age,
      full = pp,
      last = age_max,
      estimate = "z",
      method = "cr"
    ),
    error = function(e) NULL
  )
  
  if (is.null(res_cr) || is.null(res_cr$results) || nrow(res_cr$results) == 0) {
    return(list(
      success = FALSE,
      message = "L'estimation Chapman-Robson n'a pas pu être calculée pour ce jeu de données.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Construction du tableau brut ====
  table_resultats <- tryCatch(
    {
      res_cr$results |>
        transmute(
          z = .data$Estimate,
          se = .data$SE,
          a = round((1 - exp(-.data$z)) * 100, 1),
          ic_95 = {
            
            lower <- format(
              round((1 - exp(-(.data$z - .data$se))) * 100, 1),
              nsmall = 1,
              decimal.mark = ","
            )
            
            upper <- format(
              round((1 - exp(-(.data$z + .data$se))) * 100, 1),
              nsmall = 1,
              decimal.mark = ","
            )
            
            glue("[{lower} – {upper}]")
          }
        )
    },
    error = function(e) NULL
  )
  
  if (is.null(table_resultats) || nrow(table_resultats) == 0) {
    return(list(
      success = FALSE,
      message = "Les résultats Chapman-Robson n'ont pas pu être mis en forme.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Mise en forme flextable ====
  tableau_formate <- tryCatch(
    {
      flextable(table_resultats) |>
        set_caption("Estimation de la mortalité par la méthode de Chapman-Robson") |>
        set_header_labels(
          z = "Coefficient de mortalité (Z)",
          se = "Erreur standard",
          a = "Taux de mortalité (A%)",
          ic_95 = "A IC 95% (%)"
        ) |>
        style_flextable_aquapop()|>
        colformat_double(j = "z", digits = 3, decimal.mark = ",", big.mark =  " ") |>
        colformat_double(j = "se", digits = 3, decimal.mark = ",", big.mark =  " ") |>
        colformat_double(j = "a", digits = 1, decimal.mark = ",", big.mark =  " ")
    },
    error = function(e) NULL
  )
  
  list(
    success = TRUE,
    message = NULL,
    data = table_resultats,
    flextable = tableau_formate
  )
}