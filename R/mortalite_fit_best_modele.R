#' Extraire le modèle de mortalité correspondant à la meilleure méthode sélectionnée
#'
#' Cette fonction ajuste les 5 modèles de mortalité, sélectionne le meilleur
#' via `mortalite_select_best_modele()` et retourne l’objet `modele` correspondant.
#'
#' @param data Un data.frame contenant `age` et `number`(habituellement df_age_etendue)
#' @param methode Optionnel. Si fourni (e.g. "NB2"), retourne ce modèle directement.
#'
#' @return Un objet de classe `glm`, `glm.nb` ou `glmmTMB`
#' @export
mortalite_fit_best_modele <- function(data, methode = NULL) {
  if (is.null(methode)) {
    mortalite_compare_modele_res_data <- mortalite_compare_modele(data = data)$data
    methode <- mortalite_select_best_modele(mortalite_compare_modele_res_data)
  }
  
  stopifnot(!is.null(methode), methode %in% c("poisson", "nb1", "nb2", "cmp", "gp"))
  
  # Ajuster et retourner le modèle brut
  model <- switch(methode,
                  poisson = glm(number ~ age, family = poisson, data = data),
                  nb1     = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::nbinom1(), data = data),
                  nb2     = MASS::glm.nb(number ~ age, data = data),
                  cmp     = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::compois(), data = data),
                  gp      = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::genpois(), data = data)
  )
  
  return(model)
}
