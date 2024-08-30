courbe_croissance_comparaison <- function(dfspecimen, sp_pen) {
  # Filtrer les données pour l'espèce spécifiée
  df <- dfspecimen %>% filter(sp == sp_pen)
  
  # Supprimer les enregistrements avec des valeurs manquantes pour ltm et age
  df <- df %>%
    filter(!is.na(ltm) & !is.na(age))
  
  # Sélectionner uniquement les colonnes nécessaires
  df <- df %>% select(ltm, age, no_specimen)
  
  # Renommer les rangées séquentiellement de 1 à n
  rownames(df) <- seq(nrow(df))

  # https://rdrr.io/cran/fishmethods/man/growthlrt.html  pas assez clair pour reproduire, besoin de plus d'infos
  #A la place je me suis inspiree de Mainguy pour definir les modeles, puis de Ogle (Chapitre 12) Dans Quist and Isermann (2017) pour comparer les modeles
  
  # Initialisation des données et estimation des paramètres initiaux pour le modèle de croissance
  pi <- FSA::vbStarts(ltm ~ age, data = df)  # Estimation initiale des paramètres Linf, K et t0
  
  # Exécution du modèle de croissance avec les paramètres estimés
  result <- fishmethods::growth(
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
  )
  
  # Fonction de gestion des erreurs pour capturer les messages en cas d'échec des intervalles de confiance
  handle_error <- function(e) {
    return(conditionMessage(e))
  }
  
  
  # Construction du tableau des résultats en extrayant les paramètres pour chaque modèle
  tableresult <- data.frame(
    methode = c("Von Bertalanffy",
                "Gompertz" ,
                "Logistique"),
    l_inf = c(
      environment(result[["vout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
      environment(result[["gout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
      environment(result[["lout"]][["m"]][["deviance"]])[["env"]][["Sinf"]]
    ),
    k = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 1], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 1], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 1], error = handle_error)
    ),
    t0 = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 1], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 1], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 1], error = handle_error)
    ),
    l_ci_inf = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[1, 1], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[1, 1], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[1, 1], error = handle_error)
    ),
    u_ci_inf = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[1, 2], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[1, 2], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[1, 2], error = handle_error)
    ),
    l_ci_k = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 1], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 1], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 1], error = handle_error)
    ),
    u_ci_k = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 2], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 2], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 2], error = handle_error)
    ),
    l_ci_t0 = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 1], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 1], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 1], error = handle_error)
    ),
    u_ci_t0 = c(
      tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 2], error = handle_error),
      tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 2], error = handle_error),
      tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 2], error = handle_error)
    ),
    
    converged = c(result[["vout"]][["convInfo"]][["stopMessage"]],
                  result[["gout"]][["convInfo"]][["stopMessage"]],
                  result[["lout"]][["convInfo"]][["stopMessage"]])
  )
  
  

  # Calcul des AIC pour comparer les modèles
  result_aic_df <- AICcmodavg::aictab(
    list(result[["vout"]], result[["gout"]], result[["lout"]]),
    c("Von Bertalanffy", "Gompertz", "Logistique")
  ) %>% as.data.frame()
  
  # Nettoyage et formatage du tableau final
  result_aic_df <- result_aic_df %>% rename(methode = Modnames)
  result_aic_df <- result_aic_df %>% dplyr::select(-c("K"))
  final_table  <- merge(tableresult, result_aic_df, by = "methode")
  final_table <- final_table %>% dplyr::select(-c("LL", "Cum.Wt", "ModelLik"))
  
  # Traduction des résultats de convergence
  final_table[final_table == "converged"] <- "convergé"
  
  # Renommer les colonnes pour une meilleure compréhension conforme au tidyverse
  final_table <- final_table %>% rename(
    aicc = AICc,
    delta_aicc = Delta_AICc,
    aicc_wt = AICcWt
  )
  
  # Arrondir les valeurs numériques pour une présentation plus claire
  final_table$l_inf <- round(final_table$l_inf, digits = 0)
  if (is.numeric(final_table$k)) final_table$k <- round(final_table$k, digits = 3)
  if (is.numeric(final_table$t0)) final_table$t0 <- round(final_table$t0, digits = 3)
  final_table$aicc <- round(final_table$aicc, digits = 2)
  final_table$delta_aicc <- round(final_table$delta_aicc, digits = 2)
  final_table$aicc_wt <- round(final_table$aicc_wt, digits = 2)
  
  # Calcul et création des intervalles de confiance pour L∞, K et t0
  if (is.numeric(final_table$u_ci_inf)) final_table$u_ci_inf <- round(final_table$u_ci_inf, digits = 0)
  if (is.numeric(final_table$l_ci_inf)) final_table$l_ci_inf <- round(final_table$l_ci_inf, digits = 0)
  if (is.numeric(final_table$l_ci_inf) & is.numeric(final_table$u_ci_inf)) {
    final_table <- final_table %>% mutate(l_inf_ic = paste0("[", l_ci_inf, "-", u_ci_inf, "]"))
  } else {
    final_table <- final_table %>% mutate(l_inf_ic = "")
  }
  final_table <- final_table %>% dplyr::select(-c("l_ci_inf", "u_ci_inf"))
  
  if (is.numeric(final_table$u_ci_k)) final_table$u_ci_k <- round(final_table$u_ci_k, digits = 3)
  if (is.numeric(final_table$l_ci_k)) final_table$l_ci_k <- round(final_table$l_ci_k, digits = 3)
  if (is.numeric(final_table$l_ci_k) & is.numeric(final_table$u_ci_k)) {
    final_table <- final_table %>% mutate(k_ic = paste0("[", l_ci_k, "-", u_ci_k, "]"))
  } else {
    final_table <- final_table %>% mutate(k_ic = "")
  }
  final_table <- final_table %>% dplyr::select(-c("l_ci_k", "u_ci_k"))
  
  if (is.numeric(final_table$u_ci_t0)) final_table$u_ci_t0 <- round(final_table$u_ci_t0, digits = 3)
  if (is.numeric(final_table$l_ci_t0)) final_table$l_ci_t0 <- round(final_table$l_ci_t0, digits = 3)
  if (is.numeric(final_table$l_ci_t0) & is.numeric(final_table$u_ci_t0)) {
    final_table <- final_table %>% mutate(t0_ic = paste0("[", l_ci_t0, "-", u_ci_t0, "]"))
  } else {
    final_table <- final_table %>% mutate(t0_ic = "")
  }
  final_table <- final_table %>% dplyr::select(-c("l_ci_t0", "u_ci_t0"))
  
  # Application des labels aux colonnes pour une meilleure lisibilité
  final_table <- final_table %>% labelled::set_variable_labels(
    methode = "Modèles",
    l_inf = "L∞",
    l_inf_ic = "L∞ IC 95%",
    k = "K",
    k_ic = "K IC 95%",
    t0 = "t\u2080",  # Utilisation de Unicode pour afficher t avec un 0 en indice
    t0_ic = "t\u2080 IC 95%",  # Même chose ici pour l'IC
    aicc = "AICc",
    delta_aicc = "Δ AICc",
    aicc_wt = "Poids d’Akaike",
    converged = "Convergence"
  )
  
  final_table <-
    final_table %>% dplyr::select(
      c(
        "methode",
        "l_inf",
        "l_inf_ic",
        "k",
        "k_ic",
        "t0",
        "t0_ic",
        "aicc",
        "delta_aicc",
        "aicc_wt",
        "converged"
      )
    )
  
  # Trier le tableau final par AICc croissant
  final_table <- final_table %>% dplyr::arrange(aicc)
  
  return(final_table)
 }

