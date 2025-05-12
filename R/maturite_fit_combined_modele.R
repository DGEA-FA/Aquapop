#' Ajuster les modèles de maturité combinés (L50 ou A50)
#'
#' Cette fonction ajuste automatiquement douze modèles logistiques binaires pour
#' estimer la maturité sexuelle en fonction de la taille (`ltm`) ou de l’âge (`age`),
#' en tenant compte du sexe. Elle retourne un ensemble de modèles combinés incluant :
#' \itemize{
#'   \item TLO : modèle sans sexe (`~ variable`)
#'   \item ADD : effet additif du sexe (`~ variable + sexe`)
#'   \item INT : interaction complète (`~ variable * sexe`)
#'   \item COM : interaction explicitée (`~ variable + sexe + variable:sexe`)
#' }
#' Chaque forme est testée avec trois liens : logit, probit et cloglog.
#'
#' @param df Un `data.frame` contenant les colonnes `maturite`, `sexe` et la
#'   variable quantitative choisie (`ltm` ou `age`).
#' @param variable Chaîne `"ltm"` (par défaut) ou `"age"`, indiquant la variable
#'   à utiliser comme prédicteur principal.
#'
#' @return Une liste nommée contenant les 12 modèles combinés (objets `glm`).
#'   Retourne une liste vide avec un avertissement si un seul sexe est présent.
#'
#' @export
#' @importFrom stats glm binomial
maturite_fit_combined_modele <- function(df, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # --- Étape 1 : Vérification des colonnes requises ---
  required_cols <- c("maturite", "sexe", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # --- Étape 2 : Vérification de la présence des deux sexes ---
  if (!all(c("M", "F") %in% df$sexe)) {
    warning("Les données ne contiennent qu’un seul sexe. Modèles combinés non ajustés.")
    return(list())
  }
  
  # --- Étape 3 : Ajustement des 12 modèles combinés ---
  if (variable == "ltm") {
    list(
      TLO_logit   = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df),
      TLO_probit  = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df),
      TLO_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df),
      
      ADD_logit   = glm(maturite ~ ltm + sexe, family = binomial(link = "logit"), data = df),
      ADD_probit  = glm(maturite ~ ltm + sexe, family = binomial(link = "probit"), data = df),
      ADD_cloglog = glm(maturite ~ ltm + sexe, family = binomial(link = "cloglog"), data = df),
      
      INT_logit   = glm(maturite ~ ltm * sexe, family = binomial(link = "logit"), data = df),
      INT_probit  = glm(maturite ~ ltm * sexe, family = binomial(link = "probit"), data = df),
      INT_cloglog = glm(maturite ~ ltm * sexe, family = binomial(link = "cloglog"), data = df),
      
      COM_logit   = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "logit"), data = df),
      COM_probit  = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "probit"), data = df),
      COM_cloglog = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "cloglog"), data = df)
    )
  } else {
    list(
      TLO_logit   = glm(maturite ~ age, family = binomial(link = "logit"), data = df),
      TLO_probit  = glm(maturite ~ age, family = binomial(link = "probit"), data = df),
      TLO_cloglog = glm(maturite ~ age, family = binomial(link = "cloglog"), data = df),
      
      ADD_logit   = glm(maturite ~ age + sexe, family = binomial(link = "logit"), data = df),
      ADD_probit  = glm(maturite ~ age + sexe, family = binomial(link = "probit"), data = df),
      ADD_cloglog = glm(maturite ~ age + sexe, family = binomial(link = "cloglog"), data = df),
      
      INT_logit   = glm(maturite ~ age * sexe, family = binomial(link = "logit"), data = df),
      INT_probit  = glm(maturite ~ age * sexe, family = binomial(link = "probit"), data = df),
      INT_cloglog = glm(maturite ~ age * sexe, family = binomial(link = "cloglog"), data = df),
      
      COM_logit   = glm(maturite ~ age + sexe + age:sexe, family = binomial(link = "logit"), data = df),
      COM_probit  = glm(maturite ~ age + sexe + age:sexe, family = binomial(link = "probit"), data = df),
      COM_cloglog = glm(maturite ~ age + sexe + age:sexe, family = binomial(link = "cloglog"), data = df)
    )
  }
}
