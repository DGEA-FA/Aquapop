# ==== helper mortalité - ligne d'échec ========================================

#' Construire une ligne de résultat standardisée pour un modèle de mortalité échoué
#'
#' Cette fonction interne crée une ligne de sortie standardisée lorsqu'un modèle
#' de mortalité n'a pas pu être ajusté ou interprété. Elle permet d'assurer une
#' structure homogène entre tous les modèles comparés.
#'
#' @param methode Nom du modèle.
#' @param commentaire Message expliquant pourquoi le modèle est indisponible.
#'
#' @return Un `tibble` d'une ligne avec les colonnes standard du tableau de
#'   comparaison des modèles de mortalité.
#'
#' @importFrom tibble tibble
#' @noRd
#' @keywords internal
build_failed_mortalite_row <- function(methode, commentaire) {
  tibble(
    methode = methode,
    ajustement_hnp = NA_real_,
    aicc = NA_real_,
    Z = NA_real_,
    SE = NA_real_,
    A = NA_real_,
    ic95 = NA_character_,
    commentaire = commentaire,
    convergence = FALSE,
    nb_iterations_hnp = NA_real_
  )
}