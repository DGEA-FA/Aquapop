#' Ajuste les modèles L50 pour une approche sexes combinés
#'
#' @param df Un dataframe contenant les colonnes `maturite`, `ltm`, `sexe`
#'
#' @return Une liste contenant les 12 modèles combinés.
#' @export
fit_L50_combined_models <- function(df) {
  # Vérification des colonnes requises
  required_cols <- c("maturite", "ltm", "sexe")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # Vérifier que les données contiennent au moins 2 sexes
  if (!all(c("M", "F") %in% levels(df$sexe))) {
    stop("Les données doivent contenir au moins deux sexes : 'M' et 'F'.")
  }
  
  # Ajuster les 12 modèles combinés (TLO, ADD, INT, COM x 3 liens)
  list(
    TLO_logit = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df),
    TLO_probit = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df),
    TLO_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df),
    
    ADD_logit = glm(maturite ~ ltm + sexe, family = binomial(link = "logit"), data = df),
    ADD_probit = glm(maturite ~ ltm + sexe, family = binomial(link = "probit"), data = df),
    ADD_cloglog = glm(maturite ~ ltm + sexe, family = binomial(link = "cloglog"), data = df),
    
    INT_logit = glm(maturite ~ ltm * sexe, family = binomial(link = "logit"), data = df),
    INT_probit = glm(maturite ~ ltm * sexe, family = binomial(link = "probit"), data = df),
    INT_cloglog = glm(maturite ~ ltm * sexe, family = binomial(link = "cloglog"), data = df),
    
    COM_logit = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "logit"), data = df),
    COM_probit = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "probit"), data = df),
    COM_cloglog = glm(maturite ~ ltm + sexe + ltm:sexe, family = binomial(link = "cloglog"), data = df)
  )
}
