#' Calculer l’indice PSD-Q global pour une espèce cible
#'
#' Cette fonction calcule l’indice PSD (Proportional Size Distribution de type Q) pour une espèce donnée.
#' Elle retourne un tableau contenant la valeur de l’indice PSD-Q et son intervalle de confiance à 95 %,
#' à la fois sous forme brute (`data.frame`) et sous forme formatée (`flextable`).
#'
#' @param data Un `data.frame` contenant au moins les colonnes `ltm` (longueur totale en mm) et `sp`.
#'             Les données doivent être filtrées pour une seule espèce.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` avec la valeur de l’indice PSD-Q et l’intervalle de confiance 95 %.}
#'   \item{`flextable`}{Une version formatée du tableau pour affichage ou export (Word, Shiny, etc.).}
#' }
#' @export
psd_q <- function(data) {
  sp <- unique(data$sp)
  if (length(sp) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  info <- get_info_pen(sp)
  if (is.null(info)) stop("Espèce non supportée.")
  
  breakClass <- info$breaks
  limInfStock <- breakClass[2]
  
  bunch <- data %>%
    dplyr::filter(ltm >= limInfStock) %>%
    dplyr::mutate(gcat = FSA::lencat(ltm, breaks = breakClass, droplevels = TRUE))
  
  gfreq <- xtabs(~ gcat, data = bunch)
  psdtable <- prop.table(gfreq) * 100
  psdtable <- apply(psdtable, 1, sum)
  
  weights <- rep(1, length(psdtable)); weights[1] <- 0  # Ne pas inclure la première classe
  
  PSDresult <- FSA::psdCI(
    weights,
    ptbl = psdtable / 100,
    n = sum(gfreq),
    method = "binomial",
    label = "PSD Q"
  ) %>%
    as.data.frame() %>%
    dplyr::rename(
      Q   = Estimate,
      LCI = `95% LCI`,
      UCI = `95% UCI`
    ) %>%
    dplyr::mutate(`IC 95%` = glue::glue("[{round(LCI, 1)}-{round(UCI, 1)}]")) %>%
    dplyr::select(Q, `IC 95%`)
  
  PSD_flex <- flextable::flextable(PSDresult) %>%
    flextable::autofit() %>%
    flextable::align(align = "center", part = "all")
  
  return(list(
    data = PSDresult,
    flextable = PSD_flex
  ))
}
