#' Ajuster les modèles de maturité combinés (L50 ou A50)
#'
#' Cette fonction ajuste automatiquement douze modèles binaires de maturité
#' à l’aide de fonctions de lien logit, probit et cloglog.
#' estimer la maturité sexuelle en fonction de la taille (`ltm`) ou de l'âge (`age`),
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

maturite_get_formule <- function(modele = c("TLO", "ADD", "COM", "INT"),
                                 variable = c("ltm", "age")) {
  
  modele <- match.arg(modele)
  variable <- match.arg(variable)
  
  switch(
    modele,
    
    TLO = as.formula(paste("maturite ~", variable)),
    ADD = as.formula(paste("maturite ~", variable, "+ sexe")),
    COM = as.formula(paste("maturite ~", variable, ":sexe")),
    INT = as.formula(paste("maturite ~", variable, "* sexe"))
  )
}

maturite_fit_combined_modele <- function(df, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  required_cols <- c("maturite", "sexe", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  formules <- list(
    TLO = maturite_get_formule("TLO", variable),
    ADD = maturite_get_formule("ADD", variable),
    COM = maturite_get_formule("COM", variable),
    INT = maturite_get_formule("INT", variable)
  )
  
  liens <- c("logit", "probit", "cloglog")
  
  fit_one <- function(formule, lien) {
    
    tryCatch(
      glm(
        formula = formule,
        family = binomial(link = lien),
        data = df
      ),
      error = function(e) NULL
    )
  }
  
  models <- list()
  
  for (modele in names(formules)) {
    
    for (lien in liens) {
      
      id <- paste0(modele, "_", lien)
      
      models[[id]] <- fit_one(
        formules[[modele]],
        lien
      )
    }
  }
  
  models
}
