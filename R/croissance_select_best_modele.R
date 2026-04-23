#' Sélectionner le meilleur modèle de croissance selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle parmi les modèles
#' de croissance comparés par `croissance_compare_modele()`, en retenant celui
#' ayant la plus faible valeur de critère d'information corrigé (AICc) parmi les
#' modèles convergés.
#'
#' Seuls les modèles dont la colonne `convergence` vaut `"Convergé"` et dont la
#' valeur `aicc` est numérique sont considérés. Les modèles non convergents ou
#' ne possédant pas de valeur AICc exploitable sont ignorés.
#'
#' Si aucun modèle admissible n'est disponible, la fonction retourne
#' `NA_character_` et émet un avertissement.
#'
#' @param tablemodele Un `data.frame` produit par `croissance_compare_modele()`,
#'   contenant au minimum les colonnes suivantes :
#'   \describe{
#'     \item{methode}{Nom du modèle de croissance.}
#'     \item{aicc}{Valeur du critère d'information corrigé, sous forme numérique
#'     ou caractère.}
#'     \item{convergence}{Statut de convergence du modèle.}
#'   }
#'
#' @return Une chaîne de caractères correspondant au nom du meilleur modèle
#'   sélectionné, ou `NA_character_` si aucun modèle ne peut être sélectionné.
#'
#' @details
#' En cas d'égalité sur l'AICc minimal, la fonction retourne le premier modèle
#' rencontré dans le tableau filtré.
#'
#' Cette fonction est principalement utilisée dans le module Shiny de croissance
#' afin de présélectionner automatiquement le modèle à afficher dans le tableau
#' interactif et dans le graphique.
#'
#' @examples
#' # Exemple avec deux modèles convergés et un modèle non convergé
#' tablemodele <- data.frame(
#'   methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
#'   aicc = c("120.35", "118.42", "-"),
#'   convergence = c("Convergé", "Convergé", "Le modèle n'a pas convergé")
#' )
#'
#' croissance_select_best_modele(tablemodele)
#'
#' # Exemple où aucun modèle ne peut être sélectionné
#' tablemodele_aucun <- data.frame(
#'   methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
#'   aicc = c("-", "-", "-"),
#'   convergence = c(
#'     "Le modèle n'a pas convergé",
#'     "Le modèle n'a pas convergé",
#'     "Le modèle n'a pas convergé"
#'   )
#' )
#'
#' croissance_select_best_modele(tablemodele_aucun)
#'
#' @export
#'
#' @importFrom dplyr filter mutate pull
croissance_select_best_modele <- function(tablemodele) {
  
  if (is.null(tablemodele) || !is.data.frame(tablemodele) || nrow(tablemodele) == 0) {
    warning("Aucun modèle n'a pu être sélectionné.")
    return(NA_character_)
  }
  
  if (!all(c("methode", "aicc", "convergence") %in% names(tablemodele))) {
    warning("Aucun modèle n'a pu être sélectionné.")
    return(NA_character_)
  }
  
  table_filtre <- tablemodele |>
    mutate(aicc_num = suppressWarnings(as.numeric(aicc))) |>
    filter(
      convergence == TRUE,
      !is.na(aicc_num)
    )
  
  if (nrow(table_filtre) == 0) {
    warning("Aucun modèle n'a pu être sélectionné.")
    return(NA_character_)
  }
  
  best_row <- table_filtre |>
    filter(aicc_num == min(aicc_num, na.rm = TRUE)) |>
    pull(methode)
  
  best_row[[1]]
}