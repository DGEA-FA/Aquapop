#' Extraire le modèle de mortalité correspondant à la meilleure méthode sélectionnée
#'
#' Cette fonction ajuste les 5 modèles de mortalité, sélectionne le meilleur
#' via `select_best_mortalite_model()` et retourne l’objet `modele` correspondant.
#'
#' @param df_age_etendue Un data.frame contenant `age` et `number`
#' @param methode Optionnel. Si fourni (e.g. "NB2"), retourne ce modèle directement.
#'
#' @return Un objet de classe `glm`, `glm.nb` ou `glmmTMB`
#' @export
get_best_mortalite_model <- function(df_age_etendue, methode = NULL) {
  if (is.null(methode)) {
    resume <- mortalite_modele_comparaison(df_age_etendue, format = "data.frame")
    methode <- select_best_mortalite_model(resume)
  }
  
  stopifnot(!is.null(methode), methode %in% c("poisson", "nb1", "nb2", "cmp", "gp"))
  
  # Ajuster et retourner le modèle brut
  model <- switch(methode,
                  poisson = glm(number ~ age, family = poisson, data = df_age_etendue),
                  nb1     = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::nbinom1(), data = df_age_etendue),
                  nb2     = MASS::glm.nb(number ~ age, data = df_age_etendue),
                  cmp     = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::compois(), data = df_age_etendue),
                  gp      = glmmTMB::glmmTMB(number ~ age, family = glmmTMB::genpois(), data = df_age_etendue)
  )
  
  return(model)
}
