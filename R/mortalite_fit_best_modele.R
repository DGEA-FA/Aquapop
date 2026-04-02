#' Extraire le modèle de mortalité correspondant à la meilleure méthode sélectionnée
#'
#' Cette fonction ajuste les cinq modèles de mortalité (Poisson, NB1, NB2, CMP, GP),
#' sélectionne automatiquement le meilleur via `mortalite_select_best_modele()`,
#' et retourne l'objet `modele` correspondant. Si `methode` est précisé, il est utilisé directement.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes `age` et `number`
#'   (habituellement produit par `mortalite_prepare()`)
#' @param methode Chaîne de caractères optionnelle ("poisson", "nb1", "nb2", "cmp", "gp").
#'   Si précisée, ce modèle est ajusté directement sans comparaison.
#'
#' @return Un objet de classe `glm`, `glm.nb` ou `glmmTMB`, selon la méthode choisie
#' @export
#'
#' @importFrom glmmTMB glmmTMB genpois compois nbinom1
#' @importFrom MASS glm.nb
mortalite_fit_best_modele <- function(data, methode = NULL) {
  
  if (is.null(methode)) {
    res_compare <- mortalite_compare_modele(data = data)$data
    methode <- mortalite_select_best_modele(res_compare)
  }
  
  stopifnot(
    !is.null(methode),
    methode %in% c("poisson", "nb1", "nb2", "cmp", "gp")
  )
  
  # Ajuster le modèle selon la méthode sélectionnée
  model <- switch(methode,
                  poisson = glm(number ~ age, family = poisson, data = data),
                  nb1     = glmmTMB(number ~ age, family = nbinom1(), data = data),
                  nb2     = glm.nb(number ~ age, data = data),
                  cmp     = glmmTMB(number ~ age, family = compois(), data = data),
                  gp      = glmmTMB(number ~ age, family = genpois(), data = data)
  )
  
  return(model)
}
