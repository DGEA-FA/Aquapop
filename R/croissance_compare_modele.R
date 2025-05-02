#' Comparer trois modèles de croissance (Von Bertalanffy, Gompertz, Logistique)
#'
#' Cette fonction ajuste trois modèles de croissance non linéaire (Von Bertalanffy, Gompertz, Logistique)
#' à un jeu de données de spécimens. Elle retourne un tableau comparatif des paramètres estimés, des
#' intervalles de confiance et du critère d'information corrigé (AICc) pour chacun des modèles.
#'
#' @param data Un `data.frame` de spécimens, contenant au minimum les colonnes `sp`, `ltm`, `age` et `no_specimen`.
#' @param format Format de sortie souhaité : `"data.frame"` (par défaut) ou `"flextable"`.
#'
#' @return Une liste avec deux éléments :
#' \describe{
#'   \item{data}{Un `data.frame` résumant les résultats des trois modèles (paramètres, IC, AICc).}
#'   \item{flextable}{Une version formatée (`flextable`) du tableau comparatif.}
#' }
#'
#' @importFrom labelled set_variable_labels
#' @importFrom dplyr if_else mutate left_join rename select filter arrange
#' @importFrom AICcmodavg aictab
#' @importFrom stats confint
#' @importFrom fishmethods growth
#' @importFrom FSA vbStarts
#' @importFrom flextable set_caption flextable

#' @export
#'
#' @examples
#' croissance_compare_modele(data = specimen)$data
#' croissance_compare_modele(data = specimen)$flextable
croissance_compare_modele <- function(data, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
  df <- data |>
    filter(!is.na(ltm), !is.na(age)) |>
    select(ltm, age, no_specimen)
  rownames(df) <- seq_len(nrow(df))
  
  pi <- vbStarts(ltm ~ age, data = df)
  
  result <- growth(
    intype = 1, unit = 1, size = df$ltm, age = df$age,
    calctype = 1, wgtby = 1, error = 1,
    Sinf = pi$Linf, K = pi$K, t0 = pi$t0,
    graph = FALSE,
    control = list(maxiter = 10000, minFactor = 1 / 1024, tol = 1e-5)
  )
  
  modele_names <- c("Von Bertalanffy", "Gompertz", "Logistique")
  
  extract_param <- function(res, index) {
    tryCatch(confint(res, level = 0.95)[index, , drop = TRUE], error = handle_error)
  }
  
  extract_from_env <- function(mod) {
    environment(mod[["m"]][["deviance"]])[["env"]][["Sinf"]]
  }
  
  tableresult <- data.frame(
    methode = modele_names,
    l_inf = c(
      extract_from_env(result[["vout"]]),
      extract_from_env(result[["gout"]]),
      extract_from_env(result[["lout"]])
    ),
    k = c(
      extract_param(result[["vout"]], 2)[1],
      extract_param(result[["gout"]], 2)[1],
      extract_param(result[["lout"]], 2)[1]
    ),
    t0 = c(
      extract_param(result[["vout"]], 3)[1],
      extract_param(result[["gout"]], 3)[1],
      extract_param(result[["lout"]], 3)[1]
    ),
    l_inf_ic = mapply(
      function(res) {
        ic <- extract_param(res, 1)
        if (is.numeric(ic)) paste0("[", round(ic[1]), "-", round(ic[2]), "]") else ""
      },
      list(result[["vout"]], result[["gout"]], result[["lout"]])
    ),
    k_ic = mapply(
      function(res) {
        ic <- extract_param(res, 2)
        if (is.numeric(ic)) paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]") else ""
      },
      list(result[["vout"]], result[["gout"]], result[["lout"]])
    ),
    t0_ic = mapply(
      function(res) {
        ic <- extract_param(res, 3)
        if (is.numeric(ic)) paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]") else ""
      },
      list(result[["vout"]], result[["gout"]], result[["lout"]])
    ),
    converged = c(
      result[["vout"]][["convInfo"]][["stopMessage"]],
      result[["gout"]][["convInfo"]][["stopMessage"]],
      result[["lout"]][["convInfo"]][["stopMessage"]]
    )
  )
  
  aic_tab <- aictab(
    list(result[["vout"]], result[["gout"]], result[["lout"]]),
    modnames = modele_names
  ) |>
    rename(methode = Modnames) |>
    select(-c("K", "LL", "Cum.Wt", "ModelLik"))
  
  final <- left_join(tableresult, aic_tab, by = "methode") |>
    mutate(
      converged = if_else(converged == "converged", "convergé", converged),
      l_inf = round(l_inf, 0),
      k = round(as.numeric(k), 3),
      t0 = round(as.numeric(t0), 3),
      AICc = round(AICc, 2),
      Delta_AICc = round(Delta_AICc, 2),
      AICcWt = round(AICcWt, 2)
    ) |>
    set_variable_labels(
      methode = "Modèles",
      l_inf = "L∞", l_inf_ic = "L∞ IC 95%",
      k = "K", k_ic = "K IC 95%",
      t0 = "t\u2080", t0_ic = "t\u2080 IC 95%",
      AICc = "AICc",
      Delta_AICc = "Δ AICc",
      AICcWt = "Poids d’Akaike",
      converged = "Convergence"
    ) |>
    select(
      methode, l_inf, l_inf_ic,
      k, k_ic, t0, t0_ic,
      AICc, Delta_AICc, AICcWt,
      converged
    ) |>
    arrange(AICc)
  
  ft <- flextable(final) |>
    set_caption("Paramètres des modèles de croissance (VB, Gompertz, Logistique)") |>
    style_flextable_aquapop() 
  
  return(list(data = final, flextable = ft))
}
