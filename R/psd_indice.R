#' Calcule l’indice PSD global pour une espèce cible
#'
#' Cette fonction calcule l’indice PSD (Proportional Size Distribution) pour une espèce donnée.
#' Elle retourne un tableau contenant la valeur de l’indice PSD et son intervalle de confiance.
#' Le résultat peut être retourné sous forme brute (`data.frame`) ou mise en forme (`flextable`).
#'
#' @param data Un `data.frame` contenant au moins les colonnes `ltm` (longueur totale en mm) et `sp`.
#'             Les données doivent être filtrées pour une seule espèce.
#' @param format Format de sortie : `"data.frame"` (par défaut) ou `"flextable"`.
#'
#' @return Un tableau contenant la valeur du PSD (Q) et son intervalle de confiance 95 %, au format spécifié.
#' @export
psd_indice <- function(data, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
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
  
  if (format == "flextable") {
    PSDresult <- flextable::flextable(PSDresult) %>%
      flextable::autofit() %>%
      flextable::align(align = "center", part = "all") 
  }
  
  return(PSDresult)
}
