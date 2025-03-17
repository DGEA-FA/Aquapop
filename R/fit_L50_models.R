#' Ajuste les modèles L50 séparés par sexe
#'
#' @param df Un dataframe contenant les colonnes `maturite`, `ltm`, `sexe`
#'
#' @return Une liste contenant les modèles logistiques (`logit`, `probit`, `cloglog`) pour M et F.
#' @export
fit_L50_models <- function(df) {
  # Vérification des colonnes requises
  required_cols <- c("maturite", "ltm", "sexe")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # Vérifier que les données contiennent les deux sexes
  if (!all(c("M", "F") %in% levels(df$sexe))) {
    stop("Les données doivent contenir les sexes 'M' et 'F'.")
  }
  
  # Ajuster les modèles
  list(
    M_logit = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df[df$sexe == "M", ]),
    M_probit = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df[df$sexe == "M", ]),
    M_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "M", ]),
    
    F_logit = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df[df$sexe == "F", ]),
    F_probit = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df[df$sexe == "F", ]),
    F_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "F", ])
  )
}
