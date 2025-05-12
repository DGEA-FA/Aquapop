#' Ajuster les modèles de maturité séparés par sexe (L50 ou A50)
#'
#' Cette fonction ajuste six modèles logistiques binaires (logit, probit, cloglog)
#' pour les sexes mâle et femelle séparément, en fonction de la taille (`ltm`) ou
#' de l’âge (`age`). Elle retourne une liste contenant les six modèles si les deux
#' sexes sont présents. Si un seul sexe est observé, elle retourne `NULL` avec un avertissement.
#'
#' @param df Un `data.frame` contenant les colonnes `maturite`, `sexe`, et la variable
#'   quantitative sélectionnée (`ltm` ou `age`).
#' @param variable Chaîne `"ltm"` (par défaut) ou `"age"`, indiquant la variable
#'   à utiliser comme prédicteur principal.
#'
#' @return Une liste contenant les six modèles séparés (`glm`), avec les noms :
#'   `M_logit`, `M_probit`, `M_cloglog`, `F_logit`, `F_probit`, `F_cloglog`.
#'   Retourne `NULL` si un seul sexe est présent dans les données.
#'
#' @examples
#' df <- tibble::tibble(
#'   maturite = rep(c("N", "O"), each = 30),
#'   sexe = rep(c("M", "F"), each = 30),
#'   ltm = c(stats::runif(30, 100, 250), stats::runif(30, 120, 260))
#' )
#' models <- maturite_fit_separated_modele(df, variable = "ltm")
#'
#' @export
#' @importFrom stats glm binomial
maturite_fit_separated_modele <- function(df, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # --- Étape 1 : Vérification des colonnes requises ---
  required_cols <- c("maturite", "sexe", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Le dataframe doit contenir les colonnes : ", paste(missing_cols, collapse = ", "))
  }
  
  # --- Étape 2 : Vérification de la présence des deux sexes ---
  if (!all(c("M", "F") %in% df$sexe)) {
    warning("Un seul sexe observé. L’ajustement des modèles séparés est impossible.")
    return(NULL)
  }
  
  # --- Étape 3 : Ajustement des modèles séparés par sexe ---
  if (variable == "ltm") {
    list(
      M_logit   = glm(maturite ~ ltm, family = binomial(link = "logit"),   data = df[df$sexe == "M", ]),
      M_probit  = glm(maturite ~ ltm, family = binomial(link = "probit"),  data = df[df$sexe == "M", ]),
      M_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "M", ]),
      
      F_logit   = glm(maturite ~ ltm, family = binomial(link = "logit"),   data = df[df$sexe == "F", ]),
      F_probit  = glm(maturite ~ ltm, family = binomial(link = "probit"),  data = df[df$sexe == "F", ]),
      F_cloglog = glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = df[df$sexe == "F", ])
    )
  } else {
    list(
      M_logit   = glm(maturite ~ age, family = binomial(link = "logit"),   data = df[df$sexe == "M", ]),
      M_probit  = glm(maturite ~ age, family = binomial(link = "probit"),  data = df[df$sexe == "M", ]),
      M_cloglog = glm(maturite ~ age, family = binomial(link = "cloglog"), data = df[df$sexe == "M", ]),
      
      F_logit   = glm(maturite ~ age, family = binomial(link = "logit"),   data = df[df$sexe == "F", ]),
      F_probit  = glm(maturite ~ age, family = binomial(link = "probit"),  data = df[df$sexe == "F", ]),
      F_cloglog = glm(maturite ~ age, family = binomial(link = "cloglog"), data = df[df$sexe == "F", ])
    )
  }
}
