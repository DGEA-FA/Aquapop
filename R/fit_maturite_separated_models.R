#' Ajuste les modèles de maturité séparés par sexe (L50 ou A50)
#'
#' @param df Un dataframe contenant les colonnes `maturite`, `sexe` et la variable quantitative (`ltm` ou `age`)
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Une liste contenant les modèles logistiques (`logit`, `probit`, `cloglog`) pour M et F.
#' @export
fit_maturite_separated_models <- function(df, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Vérification des colonnes requises
  required_cols <- c("maturite", "sexe", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # Vérifier que les deux sexes sont présents
  if (!all(c("M", "F") %in% levels(df$sexe))) {
    stop("Les données doivent contenir les sexes 'M' et 'F'.")
  }
  
  if (variable == "ltm") {
    list(
      M_logit   = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df[df$sexe == "M", ]),
      M_probit  = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df[df$sexe == "M", ]),
      M_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "M", ]),
      
      F_logit   = glm(maturite ~ ltm, family = binomial(link = "logit"), data = df[df$sexe == "F", ]),
      F_probit  = glm(maturite ~ ltm, family = binomial(link = "probit"), data = df[df$sexe == "F", ]),
      F_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "F", ])
    )
  } else {
    list(
      M_logit   = glm(maturite ~ age, family = binomial(link = "logit"), data = df[df$sexe == "M", ]),
      M_probit  = glm(maturite ~ age, family = binomial(link = "probit"), data = df[df$sexe == "M", ]),
      M_cloglog = glm(maturite ~ age, family = binomial(link = "cloglog"), data = df[df$sexe == "M", ]),
      
      F_logit   = glm(maturite ~ age, family = binomial(link = "logit"), data = df[df$sexe == "F", ]),
      F_probit  = glm(maturite ~ age, family = binomial(link = "probit"), data = df[df$sexe == "F", ]),
      F_cloglog = glm(maturite ~ age, family = binomial(link = "cloglog"), data = df[df$sexe == "F", ])
    )
  }
}
