#' Supprimer les warnings liés aux probabilités ajustées à 0 ou 1 dans glm()
#'
#' Cette fonction évalue une expression de type `glm()` tout en filtrant les avertissements
#' du type « les probabilités ont été ajustées numériquement à 0 ou 1 », souvent bénins
#' et dus à la séparation quasi-parfaite dans les données binaires.
#'
#' @param expr Une expression (non entre guillemets) à évaluer, typiquement un appel à `glm()`
#'
#' @return Le résultat de `expr`, avec les warnings filtrés si pertinents
#'
#' @keywords internal
sans_warning_proba <- function(expr) {
  withCallingHandlers(
    expr = force(expr),
    warning = function(w) {
      if (grepl("probabilités ont été ajustées numériquement à 0 ou 1", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
