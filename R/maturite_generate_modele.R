#' Ajuste un modèle de maturité (L50 ou A50)
#'
#' Cette fonction ajuste un modèle de maturité en fonction du type de modèle (`TLO`, `ADD`, `COM`, `INT`) 
#' et du lien (`logit`, `probit`, `cloglog`) spécifiés. Elle retourne un tableau de résultats, 
#' un graphique de l'ogive de maturité, un commentaire sur l'ajustement, et les prédictions associées.
#'
#' @param data Jeu de données contenant les colonnes `maturite`, `sexe`, et `ltm` ou `age`
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#' @param modele Type de modèle : `"TLO"`, `"ADD"`, `"COM"` ou `"INT"`
#' @param lien Lien à utiliser dans le glm : `"probit"` (par défaut), `"logit"`, `"cloglog"`
#' @param nboot Nombre de tirages Monte Carlo pour les intervalles (défaut : 10000)
#'
#' @return Une liste avec :
#' \describe{
#'   \item{success}{Indique si le modèle a pu être ajusté}
#'   \item{table_resultats}{Tableau brut des coefficients et points 50 %}
#'   \item{table_resultats_flextable}{Version formatée}
#'   \item{commentaire}{Commentaire sur l'ajustement}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#'   \item{graphique}{Ogive de maturité}
#'   \item{donnees_ogive}{Données prédictives}
#' }
#'
#' @export
#' @importFrom stats glm binomial predict anova coef update plogis pnorm
#' @importFrom glue glue
#' @importFrom dplyr bind_rows filter mutate pull
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point labs annotate theme
#' @importFrom FSA Summarize
#' @importFrom flextable flextable compose as_paragraph as_sub set_header_labels add_footer_lines
maturite_generate_modele <- function(data,
                                     variable = c("ltm", "age"),
                                     modele = c("TLO", "ADD", "COM", "INT"),
                                     lien = c("probit", "logit", "cloglog"),
                                     nboot = 10000) {
  
  # Validation des arguments optionnels ----
  if (is.null(modele) || is.null(lien) || is.null(variable)) {
    return(list(
      success = FALSE,
      message = "Modèle non disponible pour ce groupe.",
      commentaire = NULL,
      table_resultats = NULL,
      table_resultats_flextable = NULL,
      graphique = NULL,
      donnees_ogive = NULL
    ))
  }
  
  variable <- match.arg(variable)
  modele <- match.arg(modele)
  lien <- match.arg(lien)
  
  if (!all(c(variable, "maturite", "sexe") %in% names(data))) {
    stop(glue("❌ Le jeu de données doit contenir les colonnes `{variable}`, `maturite` et `sexe`."))
  }
  
  # Validation minimale des cas limites ----
  validation_res <- maturite_validate_data(
    specimen_data = data,
    variable = variable  )
  
  if (!isTRUE(validation_res$success)) {
    return(list(
      success = FALSE,
      table_resultats = NULL,
      table_resultats_flextable = NULL,
      commentaire = NULL,
      message = validation_res$message,
      graphique = NULL,
      donnees_ogive = NULL
    ))
  }
  
  donnees_modeles <- maturite_prepare(data, variable = variable)
  
  # Ajustement du modèle ----------------------------------------------------
  modele_glm <- switch(modele,
                       "TLO" = switch(lien,
                                      "logit"   = if (variable == "ltm") glm(maturite ~ ltm, family = binomial(link = "logit"), data = donnees_modeles)
                                      else glm(maturite ~ age, family = binomial(link = "logit"), data = donnees_modeles),
                                      "probit"  = if (variable == "ltm") glm(maturite ~ ltm, family = binomial(link = "probit"), data = donnees_modeles)
                                      else glm(maturite ~ age, family = binomial(link = "probit"), data = donnees_modeles),
                                      "cloglog" = if (variable == "ltm") glm(maturite ~ ltm, family = binomial(link = "cloglog"), data = donnees_modeles)
                                      else glm(maturite ~ age, family = binomial(link = "cloglog"), data = donnees_modeles),
                                      stop("❌ Lien non supporté pour le modèle TLO.")
                       ),
                       
                       "ADD" = switch(lien,
                                      "logit"   = if (variable == "ltm") glm(maturite ~ ltm + sexe, family = binomial(link = logit), data = donnees_modeles)
                                      else glm(maturite ~ age + sexe, family = binomial(link = "logit"), data = donnees_modeles),
                                      "probit"  = if (variable == "ltm") glm(maturite ~ ltm + sexe, family = binomial(link = "probit"), data = donnees_modeles)
                                      else glm(maturite ~ age + sexe, family = binomial(link = "probit"), data = donnees_modeles),
                                      "cloglog" = if (variable == "ltm") glm(maturite ~ ltm + sexe, family = binomial(link = "cloglog"), data = donnees_modeles)
                                      else glm(maturite ~ age + sexe, family = binomial(link = "cloglog"), data = donnees_modeles),
                                      stop("❌ Lien non supporté pour le modèle ADD.")
                       ),
                       
                       "COM" = switch(lien,
                                      "logit"   = if (variable == "ltm") glm(maturite ~ ltm:sexe, family = binomial(link = "logit"), data = donnees_modeles)
                                      else glm(maturite ~ age:sexe, family = binomial(link = "logit"), data = donnees_modeles),
                                      "probit"  = if (variable == "ltm") glm(maturite ~ ltm:sexe, family = binomial(link = "probit"), data = donnees_modeles)
                                      else glm(maturite ~ age:sexe, family = binomial(link = "probit"), data = donnees_modeles),
                                      "cloglog" = if (variable == "ltm") glm(maturite ~ ltm:sexe, family = binomial(link = "cloglog"), data = donnees_modeles)
                                      else glm(maturite ~ age:sexe, family = binomial(link = "cloglog"), data = donnees_modeles),
                                      stop("❌ Lien non supporté pour le modèle COM.")
                       ),
                       
                       "INT" = switch(lien,
                                      "logit"   = if (variable == "ltm") glm(maturite ~ ltm * sexe, family = binomial(link = "logit"), data = donnees_modeles)
                                      else glm(maturite ~ age * sexe, family = binomial(link = "logit"), data = donnees_modeles),
                                      "probit"  = if (variable == "ltm") glm(maturite ~ ltm * sexe, family = binomial(link = "probit"), data = donnees_modeles)
                                      else glm(maturite ~ age * sexe, family = binomial(link = "probit"), data = donnees_modeles),
                                      "cloglog" = if (variable == "ltm") glm(maturite ~ ltm * sexe, family = binomial(link = "cloglog"), data = donnees_modeles)
                                      else glm(maturite ~ age * sexe, family = binomial(link = "cloglog"), data = donnees_modeles),
                                      stop("❌ Lien non supporté pour le modèle INT.")
                       ),
                       
                       stop("❌ Modèle non supporté.")
  ) |> sans_warning_proba()
  
  # Tests d'ajustement du modèle -----------------------------------------------
  pval_ajustement <- o.r.test(modele_glm)
  eta2 <- predict(modele_glm)^2
  modele_avec_eta2 <- sans_warning_proba(update(modele_glm, . ~ . + eta2))
  pval_lien <- anova(modele_glm, modele_avec_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  # Valeurs par défaut sûres
  commentaire <- "Modèle généré, sans commentaire spécifique."
  
  # Si convergence connue → on peut évaluer
  if (!isTRUE(modele_glm$converged)) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (isTRUE(pval_ajustement < 0.05) || isTRUE(pval_lien < 0.05)) {
    commentaire <- "Ce modèle ne s'ajuste pas bien aux données. Il est préférable de choisir un autre modèle."
  }
  
  
  # Estimation des coefficients ------------------------------------------------
  if (modele == "TLO") {
    b0 <- coef(modele_glm)[["(Intercept)"]]
    b1 <- coef(modele_glm)[[variable]]
    
  } else if (modele == "ADD") {
    b0 <- coef(modele_glm)[["(Intercept)"]]
    b1 <- coef(modele_glm)[[variable]]
    b2 <- coef(modele_glm)[["sexeM"]]
    
  } else if (modele == "COM") {
    b0 <- coef(modele_glm)[["(Intercept)"]]
    b1 <- maturite_get_coef(modele_glm, "sexeF")
    b2 <- maturite_get_coef(modele_glm, "sexeM")
    
  } else if (modele == "INT") {
    b0 <- coef(modele_glm)[["(Intercept)"]]
    b1 <- coef(modele_glm)[[variable]]
    b2 <- coef(modele_glm)[["sexeM"]]
    b3 <- maturite_get_coef(modele_glm, sexe = "sexeM", interaction = TRUE)
    
  } else {
    stop("❌ Modèle non reconnu.")
  }
  
  
  
  # Estimation du point de 50 % (L50 ou A50) -----------------------------------
  if (modele == "TLO") {
    ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    
    point50 <- round(ic[2])
    point50_inf <- round(ic[1])
    point50_sup <- round(ic[3])
    
  } else {
    kappa <- if (lien == "cloglog") 0.3665129 else 0
    
    point50 <- switch(modele,
                      "ADD" = list(
                        fem = round((-b0 - kappa) / b1),
                        male = round((-b0 - b2 - kappa) / b1)
                      ),
                      "COM" = list(
                        fem = round((-b0 - kappa) / b1),
                        male = round((-b0 - kappa) / b2)
                      ),
                      "INT" = list(
                        fem = round((-b0 - kappa) / b1),
                        male = round((-b0 - b2 - kappa) / (b1 + b3))
                      ),
                      stop("❌ Modèle non supporté pour le calcul du point50.")
    )
    
    point50_inf <- point50_sup <- NA
  }
  
  # Calcul des prédictions pour la courbe ---------------------------------------
  if (modele == "TLO") {
    x_min <- min(donnees_modeles[[variable]], na.rm = TRUE)
    x_max <- max(donnees_modeles[[variable]], na.rm = TRUE)
    donnees_prediction <- data.frame(temp = seq(from = x_min, to = x_max, by = 1))
    names(donnees_prediction) <- variable
  } else {
    form <- stats::as.formula(glue("{variable} ~ sexe"))
    var_minmax <- Summarize(form, data = donnees_modeles)
    donnees_prediction <- bind_rows(
      data.frame(sexe = "F", temp = seq(from = var_minmax$min[var_minmax$sexe == "F"], to = var_minmax$max[var_minmax$sexe == "F"], by = 1)),
      data.frame(sexe = "M", temp = seq(from = var_minmax$min[var_minmax$sexe == "M"], to = var_minmax$max[var_minmax$sexe == "M"], by = 1))
    )
    names(donnees_prediction)[names(donnees_prediction) == "temp"] <- variable
  }
  
  prediction_model <- predict(modele_glm, donnees_prediction, full = TRUE, type = "link", se.fit = TRUE)
  
  donnees_ogive <- donnees_prediction |>
    mutate(
      maturite = switch(lien,
                        probit = pnorm(prediction_model$fit),
                        logit = plogis(prediction_model$fit),
                        cloglog = 1 - exp(-exp(prediction_model$fit))),
      lim_inf = switch(lien,
                       probit = pnorm(prediction_model$fit - 1.96 * prediction_model$se.fit),
                       logit = plogis(prediction_model$fit - 1.96 * prediction_model$se.fit),
                       cloglog = 1 - exp(-exp(prediction_model$fit - 1.96 * prediction_model$se.fit))),
      lim_sup = switch(lien,
                       probit = pnorm(prediction_model$fit + 1.96 * prediction_model$se.fit),
                       logit = plogis(prediction_model$fit + 1.96 * prediction_model$se.fit),
                       cloglog = 1 - exp(-exp(prediction_model$fit + 1.96 * prediction_model$se.fit)))
    )
  
  # Table récapitulative --------------------------------------------------------
  etiquette <- if (variable == "ltm") "L" else "A"
  
  if (modele == "TLO") {
    table_resultats <- data.frame(
      intervalle = glue("[{point50_inf}-{point50_sup}]"),
      b0 = round(b0, 3),
      b1 = round(b1, 3)
    )
    
    col_point50 <- paste0(tolower(etiquette), "50")
    table_resultats[[col_point50]] <- point50
    table_resultats <- table_resultats[, c(col_point50, "intervalle", "b0", "b1")]
    
  } else {
    col_point50_f <- paste0(tolower(etiquette), "50_f")
    col_point50_m <- paste0(tolower(etiquette), "50_m")
    
    table_resultats <- data.frame(
      b0 = round(b0, 3),
      b1 = round(b1, 3),
      sexe = if (exists("b2")) round(b2, 3) else NA,
      interaction = if (exists("b3")) round(b3, 3) else NA
    )
    
    table_resultats[[col_point50_f]] <- point50$fem
    table_resultats[[col_point50_m]] <- point50$male
    
    ordre_cols <- switch(modele,
                         "ADD" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe"),
                         "COM" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe"),
                         "INT" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe", "interaction"))
    table_resultats <- table_resultats[, ordre_cols]
  }
  
  # Table flextable --------------------------------------------------------
  ft <- flextable(table_resultats)
  
  labels <- list(
    b0 = "b0",
    b1 = "b1"
  )
  
  if ("sexe" %in% names(table_resultats)) labels$sexe <- "sexe"
  if ("interaction" %in% names(table_resultats)) labels$interaction <- "interaction"
  if ("intervalle" %in% names(table_resultats)) labels$intervalle <- "IC 95%"
  if ("l50" %in% names(table_resultats)) labels$l50 <- "tmp"
  if ("a50" %in% names(table_resultats)) labels$a50 <- "tmp"
  if ("l50_f" %in% names(table_resultats)) labels$l50_f <- "tmp_f"
  if ("l50_m" %in% names(table_resultats)) labels$l50_m <- "tmp_m"
  if ("a50_f" %in% names(table_resultats)) labels$a50_f <- "tmp_f"
  if ("a50_m" %in% names(table_resultats)) labels$a50_m <- "tmp_m"
  
  ft <- do.call(set_header_labels, c(list(ft), labels))
  
  ft <- ft |>
    compose(j = "b0", part = "header", value = as_paragraph("b", as_sub("0"))) |>
    compose(j = "b1", part = "header", value = as_paragraph("b", as_sub("1")))
  
  if ("l50" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "l50", part = "header", value = as_paragraph("L", as_sub("50")))
  }
  if ("a50" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "a50", part = "header", value = as_paragraph("A", as_sub("50")))
  }
  if ("l50_f" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "l50_f", part = "header", value = as_paragraph("L", as_sub("50"), " – Femelle"))
  }
  if ("l50_m" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "l50_m", part = "header", value = as_paragraph("L", as_sub("50"), " – Mâle"))
  }
  if ("a50_f" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "a50_f", part = "header", value = as_paragraph("A", as_sub("50"), " – Femelle"))
  }
  if ("a50_m" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "a50_m", part = "header", value = as_paragraph("A", as_sub("50"), " – Mâle"))
  }
  if ("intervalle" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "intervalle", part = "header", value = as_paragraph("IC 95%"))
  }
  if ("sexe" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "sexe", part = "header", value = as_paragraph("sexe"))
  }
  if ("interaction" %in% names(table_resultats)) {
    ft <- ft |> compose(j = "interaction", part = "header", value = as_paragraph("interaction"))
  }
  
  ft <- ft |>
    style_flextable_aquapop()
  
  ft <- add_footer_lines(ft, values = glue("Modèle: {modele}, lien: {lien}"))
  
  table_resultats_flextable <- ft
  
  # Graphique -------------------------------------------------------------------
  color_by_sex <- "sexe" %in% names(donnees_ogive)
  
  graphique <- ggplot(data = donnees_ogive, aes(x = .data[[variable]], y = .data$maturite)) +
    { if (color_by_sex) geom_line(aes(color = .data$sexe)) else geom_line(color = couleur_default) } +
    geom_ribbon(aes(ymin = .data$lim_inf, ymax = .data$lim_sup), alpha = 0.1, fill = couleur_default) +
    { 
      if (color_by_sex) scale_color_manual(
        values = group_colors$sexe,
        labels = group_labels$sexe
      ) else NULL 
    } +
    labs(
      x = ifelse(variable == "ltm", "Longueur totale maximale (mm)", "Âge"),
      y = "Proportion reproducteurs actifs",
      title = "Ogive de maturité",
      color = "Sexe"
    ) +
    theme_aquapop()
  
  if (modele == "TLO") {
    graphique <- graphique +
      annotate("segment", x = point50, xend = point50, y = 0, yend = 0.5, color = couleur_default, lty = 2) +
      annotate("segment", x = x_min, xend = point50, y = 0.5, yend = 0.5, color = couleur_default, lty = 2)
  }
  
  if (modele %in% c("COM", "ADD", "INT")) {
    if (modele == "COM") {
      vmin_m <- min(donnees_modeles |> filter(.data$sexe == "M") |> pull(.data[[variable]]))
      vmin_f <- min(donnees_modeles |> filter(.data$sexe == "F") |> pull(.data[[variable]]))
    } else {
      form <- stats::as.formula(glue("{variable} ~ sexe"))
      var_minmax <- Summarize(form, data = donnees_modeles)
      vmin_m <- var_minmax |> filter(.data$sexe == "M") |> pull(.data$min)
      vmin_f <- var_minmax |> filter(.data$sexe == "F") |> pull(.data$min)
    }
    
    graphique <- graphique +
      annotate("segment", x = point50$male, xend = point50$male, y = 0, yend = 0.5, color = group_colors$sexe["M"], lty = 2) +
      annotate("segment", x = vmin_m, xend = point50$male, y = 0.5, yend = 0.5, color = group_colors$sexe["M"], lty = 2) +
      annotate("segment", x = point50$fem, xend = point50$fem, y = 0, yend = 0.5, color = group_colors$sexe["F"], lty = 2) +
      annotate("segment", x = vmin_f, xend = point50$fem, y = 0.5, yend = 0.5, color = group_colors$sexe["F"], lty = 2)
    
  }
  
  graphique <- graphique +
    geom_point(
      data = donnees_modeles,
      mapping = aes(
        x = .data[[variable]],
        y = as.numeric(.data$maturite) - 1,
        color = if (color_by_sex) .data$sexe else NULL
      ),
      alpha = 0.5
    )
  
  graphique <- graphique +
    labs(caption = glue("Modèle : {modele}, lien : {lien}"))
  
  return(list(
    success = TRUE,
    table_resultats = table_resultats,
    table_resultats_flextable = table_resultats_flextable,
    commentaire = commentaire,
    message = NULL,
    graphique = graphique,
    donnees_ogive = donnees_ogive
  ))
}


#' Extraire un coefficient d'un modèle de maturité selon le sexe
#'
#' Fonction interne utilisée par `maturite_generate_modele()` pour extraire
#' un coefficient ciblé (par sexe ou interaction) à partir d'un modèle `glm`.
#'
#' @param modele_glm Un objet `glm`
#' @param sexe `"sexeF"` ou `"sexeM"` selon le coefficient à extraire
#' @param interaction Logique. Si `TRUE`, cible une interaction
#'
#' @return La valeur du coefficient ciblé, ou `NA` si absent
#'
#' @keywords internal
#'
#' @importFrom stats coef
maturite_get_coef <- function(modele_glm,
                              sexe = c("sexeF", "sexeM"),
                              interaction = FALSE) {
  
  sexe <- match.arg(sexe)
  
  pattern <- if (interaction) {
    paste0(":", sexe)
  } else {
    sexe
  }
  
  coef_nom <- names(coef(modele_glm))
  nom_cible <- coef_nom[grep(pattern, coef_nom)]
  
  # Si aucun coef trouvé → retourner NA au lieu de planter
  if (length(nom_cible) == 0) {
    return(NA_real_)
  }
  
  coef(modele_glm)[nom_cible]
}