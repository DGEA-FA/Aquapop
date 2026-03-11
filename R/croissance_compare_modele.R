#' Comparer trois modèles de croissance (Von Bertalanffy, Gompertz, Logistique)
#'
#' Cette fonction ajuste trois modèles de croissance non linéaire à un jeu de données de spécimens
#' et retourne un tableau comparatif des paramètres estimés, des intervalles de confiance
#' et du critère d'information corrigé (aicc).
#'
#' @param data Un `data.frame` contenant au minimum : `sp`, `ltm`, `age`, `no_specimen`
#' @param format Format de sortie souhaité : `"data.frame"` (par défaut) ou `"flextable"`
#'
#' @return Une liste avec deux éléments :
#' \describe{
#'   \item{data}{`data.frame` résumant les résultats des trois modèles}
#'   \item{flextable}{Tableau formaté prêt pour insertion dans un document}
#' }
#'
#' @export
#' @importFrom dplyr if_else mutate left_join rename select filter arrange n_distinct
#' @importFrom AICcmodavg aictab
#' @importFrom stats confint coef
#' @importFrom fishmethods growth
#' @importFrom FSA vbStarts
#' @importFrom flextable flextable set_caption set_header_labels
croissance_compare_modele <- function(data, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
  df <- data |>
    filter(!is.na(ltm), !is.na(age)) |>
    select(ltm, age, no_specimen)
  
  rownames(df) <- seq_len(nrow(df))
  
  # Validation minimale ----
  if (nrow(df) < 3) {
    stop(
      "La modélisation de croissance requiert au moins 3 spécimens ayant une longueur et un âge valides.",
      call. = FALSE
    )
  }
  
  nb_ages_distincts <- n_distinct(df$age)
  
  if (nb_ages_distincts < 3) {
    stop(
      paste0(
        "La modélisation de croissance requiert au moins 3 âges distincts. ",
        "Or, le jeu de données des spécimens de cette pêche n’en contient que ",
        nb_ages_distincts,
        ". Cette situation peut également être observée dans la figure de structure d’âge."
      ),
      call. = FALSE
    )
  }
  
  pi <- vbStarts(ltm ~ age, data = df)
  
  result <- growth(
    intype = 1, unit = 1, size = df$ltm, age = df$age,
    calctype = 1, wgtby = 1, error = 1,
    Sinf = pi$Linf, K = pi$K, t0 = pi$t0,
    graph = FALSE,
    control = list(maxiter = 10000, minFactor = 1 / 1024, tol = 1e-5)
  )
  
  modele_names <- c("Von Bertalanffy", "Gompertz", "Logistique")
  
  tableresult <- data.frame(
    methode = modele_names,
    l_inf = c(
      extract_coef(result$vout, "Sinf"),
      extract_coef(result$gout, "Sinf"),
      extract_coef(result$lout, "Sinf")
    ),
    k = c(
      extract_coef(result$vout, "K"),
      extract_coef(result$gout, "K"),
      extract_coef(result$lout, "K")
    ),
    t0 = c(
      extract_coef(result$vout, "t0"),
      extract_coef(result$gout, "t0"),
      extract_coef(result$lout, "t0")
    ),
    l_inf_ic = mapply(function(res) {
      ic <- extract_param_ic(res, 1)
      if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
        paste0("[", round(ic[1]), "-", round(ic[2]), "]")
      } else {
        "IC non calculable"
      }
    }, list(result$vout, result$gout, result$lout)),
    k_ic = mapply(function(res) {
      ic <- extract_param_ic(res, 2)
      if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
        paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]")
      } else {
        "IC non calculable"
      }
    }, list(result$vout, result$gout, result$lout)),
    t0_ic = mapply(function(res) {
      ic <- extract_param_ic(res, 3)
      if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
        paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]")
      } else {
        "IC non calculable"
      }
    }, list(result$vout, result$gout, result$lout)),
    converged = c(
      result$vout$convInfo$stopMessage,
      result$gout$convInfo$stopMessage,
      result$lout$convInfo$stopMessage
    )
  )
  
  aic_tab <- aictab(
    list(result$vout, result$gout, result$lout),
    modnames = modele_names
  ) |>
    rename(
      methode = .data$Modnames,
      aicc = .data$AICc,
      delta_aicc = .data$Delta_AICc,
      aiccwt = .data$AICcWt
      ) |>
    select(methode, aicc, delta_aicc, aiccwt) 
  
  final <- left_join(tableresult, aic_tab, by = "methode") |>
    mutate(
      converged = if_else(converged == "converged", "convergé", converged),
      l_inf = round(l_inf, 0),
      k = round(as.numeric(k), 3),
      t0 = round(as.numeric(t0), 3),
      aicc = round(aicc, 2),
      delta_aicc = round(delta_aicc, 2),
      aiccwt = round(aiccwt, 2)
    ) |>
    select(
      methode, l_inf, l_inf_ic,
      k, k_ic, t0, t0_ic,
      aicc, delta_aicc, aiccwt,
      converged
    ) |>
    arrange(aicc)
  
  ft <- flextable(final) |>
    set_caption("Paramètres des modèles de croissance (VB, Gompertz, Logistique)") |>
    set_header_labels(values = list(
      methode = "Modèles",
      l_inf = "L∞", l_inf_ic = "L∞ IC 95%",
      k = "K", k_ic = "K IC 95%",
      t0 = "t\u2080", t0_ic = "t\u2080 IC 95%",
      aicc = "AICc",
      delta_aicc = "Δ AICc",
      aiccwt = "Poids d’Akaike",
      converged = "Convergence"
    )
    ) |>
    style_flextable_aquapop()
  
  return(list(data = final, flextable = ft))
}


#' Extraire l’intervalle de confiance d’un paramètre d’un modèle de croissance
#'
#' Fonction interne utilisée par `croissance_compare_modele()` pour tenter
#' d’extraire l’intervalle de confiance à 95 % d’un paramètre estimé par un
#' modèle `nls` ajusté via `growth()`.
#'
#' Le paramètre est identifié par sa position dans la matrice retournée par
#' `confint()` (1 = L∞, 2 = K, 3 = t0). Si le calcul des intervalles de confiance
#' échoue (ce qui peut survenir pour certains ajustements `nls`), la fonction
#' retourne `NA`.
#'
#' @param res Un objet de modèle (`nls`) provenant de `growth()`.
#' @param index Position du paramètre (1 = L∞, 2 = K, 3 = t0).
#'
#' @return Un vecteur numérique de longueur 2 (borne inférieure et supérieure),
#' ou `NA` si l’intervalle de confiance ne peut pas être calculé.
#' @keywords internal
extract_param_ic <- function(res, index) {
  tryCatch(confint(res, level = 0.95)[index, , drop = TRUE], error = function(e) NA)
}

#' Extraire la valeur estimée d’un paramètre d’un modèle de croissance
#'
#' Fonction interne utilisée par `croissance_compare_modele()` pour récupérer
#' la valeur estimée d’un paramètre (`Sinf`, `K` ou `t0`) à partir d’un modèle
#' `nls` ajusté avec `growth()`.
#'
#' La valeur est extraite directement à partir des coefficients estimés du
#' modèle (`coef()`), ce qui permet de récupérer les paramètres même lorsque
#' le calcul des intervalles de confiance échoue.
#'
#' @param mod Un objet `nls` contenu dans `growth()`.
#' @param param_name Nom du paramètre à extraire (`"Sinf"`, `"K"` ou `"t0"`).
#'
#' @return Un nombre correspondant à la valeur estimée du paramètre, ou
#' `NA_real_` en cas d’erreur.
#' @keywords internal
extract_coef <- function(mod, param_name = "Sinf") {
  tryCatch(
    coef(mod)[[param_name]],
    error = function(e) NA_real_
  )
}