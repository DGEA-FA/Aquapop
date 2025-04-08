#' Ajuste les modèles de maturité pour une approche sexes combinés (L50 ou A50)
#'
#' @param df Un dataframe contenant les colonnes `maturite`, `sexe` et la variable quantitative (`ltm` ou `age`)
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Une liste contenant les 12 modèles combinés
#' @export
fit_maturite_combined_models <- function(df, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Vérification des colonnes requises
  required_cols <- c("maturite", "sexe", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # Vérifier que les données contiennent au moins 2 sexes
  if (!all(c("M", "F") %in% levels(df$sexe))) {
    stop("Les données doivent contenir au moins deux sexes : 'M' et 'F'.")
  }
  
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
