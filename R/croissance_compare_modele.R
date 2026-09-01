#' Comparer trois modèles de croissance (Von Bertalanffy, Gompertz, Logistique)
#'
#' Cette fonction ajuste trois modèles de croissance non linéaire à un jeu de
#' données de spécimens et retourne un tableau comparatif des paramètres estimés,
#' des intervalles de confiance à 95 % et du critère d'information corrigé
#' (AICc).
#'
#' Si les données ne permettent pas la modélisation, par exemple en présence de
#' moins de 3 spécimens valides ou de moins de 3 âges distincts, la fonction
#' retourne un résultat avec `success = FALSE` et un message explicatif.
#'
#' Les valeurs initiales de départ sont définies avec `findGrowthStarts()`Si possible,
#' sinon avec `vbStarts()`. Dans le cas où des deux méthodes échoueraient, des valeurs
#' initiales, fixe sont imposées : 
#' \itemize{
#'   \item `Linf` = longueur du plus grand spécimen de l'échantillon
#'   \item `K = 0.3`
#'   \item `t0 = 0`
#' }
#'
#' Un message est  inclus dans la sortie afin d’informer l’utilisateur de la
#' méthodes utilisées pour déterminer les valeurs initiales.
#'
#' Si un ou plusieurs modèles ne convergent pas, la fonction retourne quand même
#' un tableau comparatif. Les valeurs numériques associées aux modèles non
#' convergents sont alors laissées à `NA`, les intervalles de confiance sont
#' laissés vides, et la colonne `convergence` indique `FALSE`.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes `ltm`, `age`
#'   et `no_specimen`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Un indicateur logique précisant si l'analyse a pu être
#'   réalisée.}
#'   \item{data}{Un `data.frame` résumant les résultats des trois modèles, ou
#'   `NULL` si l'analyse n'a pas pu être effectuée.}
#'   \item{flextable}{Un objet `flextable` formaté pour affichage ou exportation,
#'   ou `NULL` si l'analyse n'a pas pu être effectuée.}
#'   \item{message}{Un message informatif ou explicatif, ou `NULL` si aucun
#'   message n'est nécessaire.}
#' }
#'
#' @details
#' Les modèles sont comparés uniquement à partir des ajustements convergés.
#' Le calcul de l'AICc est effectué seulement pour ces modèles. Si aucun modèle
#' ne converge, le tableau final est tout de même retourné et un message global
#' l'indique.
#'
#' Les intervalles de confiance sont extraits individuellement pour chaque
#' paramètre. Lorsqu'un intervalle de confiance ne peut pas être calculé, la
#' valeur `"IC non calculable"` est utilisée.
#'
#' @export
#'
#' @importFrom dplyr arrange bind_rows filter if_else left_join mutate n_distinct
#' @importFrom dplyr rename select
#' @importFrom AICcmodavg aictab
#' @importFrom stats confint coef
#' @importFrom fishmethods growth
#' @importFrom FSA vbStarts
#' @importFrom flextable flextable set_caption set_header_labels
croissance_compare_modele <- function(data) {
  
  df <- data |>
    filter(!is.na(.data$ltm), !is.na(.data$age)) |>
    select("ltm", "age", "no_specimen")
  
  rownames(df) <- seq_len(nrow(df))
  
  # Validation minimale ----
  
  if (nrow(df) < 3) {
    
    message <- paste(
      "La modélisation de croissance requiert au moins 3 spécimens ayant",
      "une longueur et un âge valides."
    )
    
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = message
    ))
    
  }
  
  nb_ages_distincts <- n_distinct(df$age)
  
  if (nb_ages_distincts < 3) {
    
    message <- paste0(
      "La modélisation de croissance requiert au moins 3 âges distincts. ",
      "Or, le jeu de données des spécimens de cette pêche n’en contient que ",
      nb_ages_distincts,
      ". Cette situation peut également être observée dans la figure de ",
      "structure d’âge."
    )
    
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = message
    ))
    
  }
  
  # Valeurs initiales ----
  
  pi_res <- get_growth_start_values(df)
  pi <- pi_res$pi
  
  # Ajustement des modèles ----
  
  result <- tryCatch(
    growth(
      intype = 1,
      unit = 1,
      size = df$ltm,
      age = df$age,
      calctype = 1,
      wgtby = 1,
      error = 1,
      Sinf = pi$Linf,
      K = pi$K,
      t0 = pi$t0,
      graph = FALSE,
      control = list(
        maxiter = 10000,
        minFactor = 1 / 1024,
        tol = 1e-5
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    
    message_parts <- c(
      pi_res$message,
      "La modélisation de croissance a échoué malgré l’utilisation de ces valeurs initiales."
    )
    
    message <- paste(message_parts[!is.null(message_parts)], collapse = "\n\n")
    
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = message
    ))
    
  }
  
  modele_info <- data.frame(
    methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
    composante = c("vout", "gout", "lout"),
    stringsAsFactors = FALSE
  )
  
  # Extraction des résultats par modèle ----
  
  tableresult <- lapply(seq_len(nrow(modele_info)), function(index_modele) {
    
    methode_i <- modele_info$methode[[index_modele]]
    composante_i <- modele_info$composante[[index_modele]]
    
    mod <- extract_growth_model(result, composante_i)

    if (!is_growth_model_available(mod)) {
      return(build_growth_failure_row(methode_i))
    }
    
    convergence_message <- extract_growth_convergence(mod)
    is_converged <- extract_growth_convergence(mod)
    
    if (!is_converged) {
      return(build_growth_failure_row(methode_i))
    }
    
    data.frame(
      methode = methode_i,
      l_inf = extract_coef(mod, "Sinf"),
      k = extract_coef(mod, "K"),
      t0 = extract_coef(mod, "t0"),
      l_inf_ic = format_growth_ic(mod, 1, digits = 0),
      k_ic = format_growth_ic(mod, 2, digits = 3),
      t0_ic = format_growth_ic(mod, 3, digits = 3),
      convergence = TRUE,
      stringsAsFactors = FALSE
    )
    
  }) |>
    bind_rows()
  
  # Calcul AICc seulement pour les modèles convergés ----
  
  modeles_valides <- lapply(modele_info$composante, function(composante_i) {
    mod <- extract_growth_model(result, composante_i)
    
    if (!is_growth_model_available(mod)) {
      return(NULL)
    }
    
    if (!extract_growth_convergence(mod)) {
      return(NULL)
    }
    
    mod
  })
  
  noms_modeles_valides <- modele_info$methode[!vapply(modeles_valides, is.null, logical(1))]
  modeles_valides <- Filter(Negate(is.null), modeles_valides)
  
  if (length(modeles_valides) > 0) {
    
    aic_tab <- tryCatch(
      aictab(modeles_valides, modnames = noms_modeles_valides) |>
        rename(
          methode = Modnames,
          aicc = AICc,
          delta_aicc = Delta_AICc
        ) |>
        select("methode", "aicc", "delta_aicc"),
      error = function(e) {
        data.frame(
          methode = noms_modeles_valides,
          aicc = NA_real_,
          delta_aicc = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    )
    
  } else {
    
    aic_tab <- data.frame(
      methode = modele_info$methode,
      aicc = NA_real_,
      delta_aicc = NA_real_,
      stringsAsFactors = FALSE
    )
    
  }
  
  # Tableau final ----
  
  final <- tableresult |>
    left_join(aic_tab, by = "methode") |>
    mutate(
      aicc_sort = if_else(is.na(.data$aicc), Inf, .data$aicc)
    ) |>
    arrange(.data$aicc_sort, .data$methode) |>
    mutate(
      l_inf = if_else(.data$convergence, .data$l_inf, NA_real_),
      k = if_else(.data$convergence, .data$k, NA_real_),
      t0 = if_else(.data$convergence, .data$t0, NA_real_),
      l_inf_ic = if_else(.data$convergence, .data$l_inf_ic, NA_character_),
      k_ic = if_else(.data$convergence, .data$k_ic, NA_character_),
      t0_ic = if_else(.data$convergence, .data$t0_ic, NA_character_)
    ) |>
    select(
      "methode",
      "l_inf", "l_inf_ic",
      "k", "k_ic",
      "t0", "t0_ic",
      "aicc",
      "delta_aicc",
      "convergence"
    )
  
  # Message global ----
  
  aucun_modele_converge <- all(!final$convergence)
  
  message_parts <- c()
  
  if (!is.null(pi_res$message)) {
    message_parts <- c(message_parts, pi_res$message)
  }
  
  if (aucun_modele_converge) {
    message_parts <- c(
      message_parts,
      "Aucun des modèles de croissance n'a convergé pour ce jeu de données."
    )
  }
  
  message <- if (length(message_parts) == 0) {
    NULL
  } else {
    paste(message_parts, collapse = "\n\n")
  }
  
  # Flextable ----
  
  ft <- flextable(final) |>
    set_caption("Paramètres des modèles de croissance (VB, Gompertz, Logistique)") |>
    style_flextable_aquapop()|>
    set_header_labels(values = list(
      methode = "Modèles",
      l_inf = "L∞",
      l_inf_ic = "L∞ IC 95%",
      k = "K",
      k_ic = "K IC 95%",
      t0 = "t\u2080",
      t0_ic = "t\u2080 IC 95%",
      aicc = "AICc",
      delta_aicc = "Δ AICc",
      convergence = "Convergence"
    )) |>
    
    # Ajustement spécifique
    colformat_double(j = "k", digits = 3, decimal.mark = ",", big.mark = " ", na_str = "-") |>
    colformat_double(j = "t0", digits = 3, decimal.mark = ",", big.mark = " ", na_str = "-") |>
    colformat_double(j = "l_inf", digits = 0, decimal.mark = ",", big.mark = " ", na_str = "-") |>
  colformat_double(j = c("aicc", "delta_aicc"), digits = 2, decimal.mark = ",", big.mark = " ", na_str = "-")
  
  return(list(
    success = TRUE,
    data = final,
    flextable = ft,
    message = message
  ))
}

#' Déterminer les valeurs initiales à utiliser pour les modèles de croissance
#'
#' Cette fonction tente d’abord d’estimer automatiquement les valeurs initiales
#' des paramètres de croissance (`Linf`, `K`, `t0`) à l’aide de `findGrowthStarts()`
#' puis, si cela échoue, avec `vbStarts()`.
#'
#' Dans certains cas (ex. structure d’âge déséquilibrée ou peu informative),
#' ces estimation peuvent échouer. Une stratégie de rechange est alors utilisée
#' afin de permettre la modélisation :
#' \itemize{
#'   \item `Linf` : longueur maximale observée dans l’échantillon
#'   \item `K` : valeur fixée à 0.3
#'   \item `t0` : valeur fixée à 0
#' }
#'
#' Ces valeurs par défaut sont issues de recommandations générales (Ogle 2016)
#' et permettent d’initialiser les modèles même lorsque les données ne permettent
#' pas une estimation automatique robuste.
#'
#' @param df Un `data.frame` contenant au minimum les colonnes `ltm` et `age`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{pi}{Une liste nommée avec `Linf`, `K` et `t0`}
#'   \item{message}{Un message d’avertissement à afficher à l’utilisateur, ou
#'   `NULL` si `vbStarts()` a réussi}
#' }
#' @keywords internal
get_growth_start_values <- function(df) {
  
  # Fonction interne de validation biologique des paramètres VB
  is_valid_vb <- function(pi) {

  if (is.null(pi$Linf) || is.null(pi$K) || is.null(pi$t0)) {
    return(FALSE)
  }
  
  if (!is.finite(pi$Linf) || !is.finite(pi$K) || !is.finite(pi$t0)) {
    return(FALSE)
  }
  
# L'infini et K doivent être positifs
   if (pi$Linf <= 0) {
    return(FALSE)
  }
  
  if (pi$K <= 0) {
    return(FALSE)
  }
  
  TRUE
}
  
  pi_find <- tryCatch(
    findGrowthStarts(
      ltm ~ age,
      data = df,
      type = "von Bertalanffy"
      ),
    error = function(e) NULL
    )
  
  if (!is.null(pi_find)) {
    
    pi <- list(
      Linf = unname(pi_find["Linf"]),
      K = unname(pi_find["K"]),
      t0 = unname(pi_find["t0"])
    )
    
    if (is_valid_vb(pi)) {
      
      return(list(
        pi = pi,
        message = paste(
          "Valeurs initiales obtenues avec findGrowthStarts().",
          sprintf(
            "Linf = %.3f, K = %.5f, t0 = %.3f",
            pi$Linf,
            pi$K,
            pi$t0
          ),
          sep = "\n"
        )
      ))
    }
  }

# --------------------------------------------------
# 2) Tentative avec vbStarts()
# --------------------------------------------------

  pi_vbstarts <- tryCatch(
    vbStarts(
      ltm ~ age,
      data = df
    ),
    error = function(e) NULL
  )  
  
  if (!is.null(pi_vbstarts)) {
    
    pi <- list(
      Linf = unname(pi_vbstarts$Linf),
      K = unname(pi_vbstarts$K),
      t0 = unname(pi_vbstarts$t0)
    )
    
    if (is_valid_vb(pi)) {
      
      return(list(
        pi = pi,
        message = paste(
          "Valeurs initiales obtenues avec vbStarts().",
          sprintf(
            "Linf = %.3f, K = %.5f, t0 = %.3f",
            pi$Linf,
            pi$K,
            pi$t0
          ),
          sep = "\n"
        )
      ))
    }
  }


# --------------------------------------------------
# 3) Valeurs fixes de secours
# --------------------------------------------------

  pi_fallback <- list(
    Linf = max(df$ltm, na.rm = TRUE),
    K = 0.3,
    t0 = 0
  )
  
  
  return(list(
    pi = pi_fallback,
    message = paste(
      "Les valeurs des paramètres initiaux pi n’ont pas pu être estimées automatiquement à partir des données.",
      "Elles ont été fixées à L∞ = longueur du plus grand spécimen de l’échantillon, K = 0.3 et t₀ = 0 (Ogle 2016).",
      sep = "\n"
    )
  ))
}

#' Vérifier si un modèle de croissance retourné par `growth()` est disponible
#'
#' @param mod Objet supposé correspondre à un modèle individuel (`nls`).
#'
#' @return `TRUE` si le modèle semble disponible, `FALSE` sinon.
#' @keywords internal
is_growth_model_available <- function(mod) {
  
  if (is.null(mod)) {
    return(FALSE)
  }
  
  if (inherits(mod, "try-error")) {
    return(FALSE)
  }
  
  if (!inherits(mod, "nls")) {
    return(FALSE)
  }
  
  if (is.null(coef(mod))) {
    return(FALSE)
  }
  
  if (is.null(mod$convInfo)) {
    return(FALSE)
  }
  
  TRUE
}
#' Extraire un modèle individuel depuis l'objet retourné par `growth()`
#'
#' @param result Objet retourné par `growth()`.
#' @param component Nom de la composante à extraire (`"vout"`, `"gout"`,
#'   `"lout"`).
#'
#' @return Le modèle demandé, ou `NULL` si absent.
#' @keywords internal
extract_growth_model <- function(result, component) {
  
  if (is.null(result)) {
    return(NULL)
  }
  
  if (!component %in% names(result)) {
    return(NULL)
  }
  
  result[[component]]
}

#' Extraire l'état de convergence d'un modèle de croissance
#'
#' @param mod Un objet de modèle (`nls`) provenant de `growth()`.
#'
#' @return Une chaîne de caractères décrivant l'état de convergence.
#' @keywords internal
extract_growth_convergence <- function(mod) {
  
  tryCatch(
    {
      isTRUE(mod$convInfo$isConv)
    },
    error = function(e) FALSE
  )
}

#' Construire une ligne de résultat pour un modèle non convergent
#'
#' @param methode Nom du modèle.
#'
#' @return Un `data.frame` d'une ligne.
#' @keywords internal
build_growth_failure_row <- function(methode) {
  
  data.frame(
    methode = methode,
    l_inf = NA_real_,
    k = NA_real_,
    t0 = NA_real_,
    l_inf_ic = NA_character_,
    k_ic = NA_character_,
    t0_ic = NA_character_,
    convergence = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Formater l'intervalle de confiance d'un paramètre de croissance
#'
#' @param res Un objet de modèle (`nls`) provenant de `growth()`.
#' @param index Position du paramètre (1 = L∞, 2 = K, 3 = t0).
#' @param digits Nombre de décimales à conserver.
#'
#' @return Une chaîne de caractères formatée, ou `"IC non calculable"`.
#' @keywords internal
format_growth_ic <- function(res, index, digits = 3) {
  
  ic <- extract_param_ic(res, index)
  
  if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
    
    lower <- format(
      round(ic[1], digits),
      nsmall = digits,
      decimal.mark = ","
    )
    
    upper <- format(
      round(ic[2], digits),
      nsmall = digits,
      decimal.mark = ","
    )
    
    return(paste0("[", lower, "-", upper, "]"))
  }
  
  "IC non calculable"
}

#' Extraire l’intervalle de confiance d’un paramètre d’un modèle de croissance
#'
#' Fonction interne utilisée par `croissance_compare_modele()` pour tenter
#' d’extraire l’intervalle de confiance à 95 % d’un paramètre estimé par un
#' modèle `nls` ajusté via `growth()`.
#'
#' Le paramètre est identifié par sa position dans la matrice retournée par
#' `confint()` (1 = L∞, 2 = K, 3 = t0). Si le calcul des intervalles de confiance
#' échoue, la fonction retourne `NA`.
#'
#' @param res Un objet de modèle (`nls`) provenant de `growth()`.
#' @param index Position du paramètre (1 = L∞, 2 = K, 3 = t0).
#'
#' @return Un vecteur numérique de longueur 2, ou `NA` si l’intervalle de
#'   confiance ne peut pas être calculé.
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
#' @param mod Un objet `nls` contenu dans `growth()`.
#' @param param_name Nom du paramètre à extraire (`"Sinf"`, `"K"` ou `"t0"`).
#'
#' @return Un nombre correspondant à la valeur estimée du paramètre, ou
#'   `NA_real_` en cas d’erreur.
#' @keywords internal
extract_coef <- function(mod, param_name = "Sinf") {
  tryCatch(
    coef(mod)[[param_name]],
    error = function(e) NA_real_
  )
}