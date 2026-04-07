#' Extraire le modèle de mortalité correspondant à la meilleure méthode sélectionnée
#'
#' Cette fonction ajuste le modèle de mortalité correspondant à la méthode
#' sélectionnée. Si `methode` n'est pas précisée, elle compare d'abord les
#' modèles disponibles à l'aide de `mortalite_compare_modele()` puis sélectionne
#' automatiquement le meilleur via `mortalite_select_best_modele()`.
#'
#' La fonction retourne `NULL` si aucun modèle ne peut être sélectionné ou ajusté.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes `age` et `number`,
#'   généralement produit par `mortalite_prepare_extended()`.
#' @param methode Chaîne de caractères optionnelle parmi `"poisson"`, `"nb1"`,
#'   `"nb2"`, `"cmp"` ou `"gp"`. Si précisée, ce modèle est ajusté directement
#'   sans étape de comparaison.
#'
#' @return Un objet de classe `glm`, `glm.nb` ou `glmmTMB`, selon la méthode
#'   choisie, ou `NULL` si aucun modèle n'a pu être ajusté.
#'
#' @importFrom glmmTMB compois genpois glmmTMB nbinom1
#' @importFrom MASS glm.nb
#'
#' @export
mortalite_fit_best_modele <- function(data, methode = NULL) {
  # Validation de base ====
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  if (!all(c("age", "number") %in% names(data))) {
    return(NULL)
  }
  
  # Déterminer la méthode si elle n'est pas fournie ====
  if (is.null(methode)) {
    res_compare <- mortalite_compare_modele(data = data)
    
    if (is.null(res_compare) || isFALSE(res_compare$success) || is.null(res_compare$data)) {
      return(NULL)
    }
    
    methode <- mortalite_select_best_modele(res_compare$data)
  }
  
  # Validation de la méthode ====
  methodes_valides <- c("poisson", "nb1", "nb2", "cmp", "gp")
  
  if (is.null(methode) || !methode %in% methodes_valides) {
    return(NULL)
  }
  
  # Ajuster le modèle selon la méthode sélectionnée ====
  model <- switch(
    methode,
    poisson = tryCatch(
      glm(number ~ age, family = poisson, data = data),
      error = function(e) NULL
    ),
    nb1 = tryCatch(
      glmmTMB(number ~ age, family = nbinom1(), data = data),
      error = function(e) NULL
    ),
    nb2 = tryCatch(
      glm.nb(number ~ age, data = data),
      error = function(e) NULL
    ),
    cmp = tryCatch(
      glmmTMB(number ~ age, family = compois(), data = data),
      error = function(e) NULL
    ),
    gp = tryCatch(
      glmmTMB(number ~ age, family = genpois(), data = data),
      error = function(e) NULL
    )
  )
  
  model
}