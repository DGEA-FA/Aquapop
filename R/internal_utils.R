#' Exécute une expression glm() en filtrant le warning "probabilités ajustées à 0 ou 1"
#'
#' @param expr Une expression `glm()` passée sans guillemets
#'
#' @return Le résultat de `glm()`, sans émettre de warning si celui-ci correspond à l'ajustement numérique
#' @importFrom glue glue
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

#' Extraire un coefficient d’un modèle de maturité selon le sexe
#'
#' @param modele_glm Un objet `glm`
#' @param sexe `"sexeF"` ou `"sexeM"` selon le coefficient à extraire
#' @param interaction Logique. Si `TRUE`, cible une interaction.
#'
#' @return La valeur du coefficient correspondant
#' @keywords internal
maturite_get_coef <- function(modele_glm, sexe = c("sexeF", "sexeM"), interaction = FALSE) {
  sexe <- match.arg(sexe)
  pattern <- if (interaction) paste0(":", sexe) else sexe
  coef_nom <- names(coef(modele_glm))
  nom_cible <- coef_nom[grepl(pattern, coef_nom)]
  
  if (length(nom_cible) == 0) {
    stop(glue("❌ Aucun coefficient ne correspond au motif '{pattern}' dans le modèle."))
  }
  
  coef(modele_glm)[[nom_cible]]
}
