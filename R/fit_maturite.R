#' Ajuste un modèle de maturité selon le type spécifié (TLO, ADD, COM, INT) et le lien (logit, probit, cloglog)
#'
#' @param data Jeu de données contenant les colonnes `maturite`, `sexe` et `ltm` ou `age`
#' @param variable Variable quantitative à utiliser : "ltm" (par défaut) ou "age"
#' @param modele Type de modèle : "TLO", "ADD", "COM" ou "INT"
#' @param lien Lien à utiliser dans le glm : "probit" (par défaut), "logit" ou "cloglog"
#' @param nboot Nombre de tirages Monte Carlo (par défaut: 10000)
#'
#' @return Liste avec `table_resultats`, `table_resultats_flextable`, `commentaire`, `graphique`, `donnees_ogive`
#' @export
fit_maturite <- function(data, variable = c("ltm", "age"), modele = c("TLO", "ADD", "COM", "INT"), lien = c("probit", "logit", "cloglog"), nboot = 10000) {
  variable <- match.arg(variable)
  modele <- match.arg(modele)
  lien <- match.arg(lien)
  
  if (!all(c(variable, "maturite", "sexe") %in% names(data))) {
    stop(glue::glue("❌ Le jeu de données doit contenir les colonnes `{variable}`, `maturite` et `sexe`."))
  }
  
  donnees_modeles <- prepare_maturite_data(data, variable = variable)
  
  if (nrow(donnees_modeles) < 10) {
    stop(glue::glue("❌ Trop peu d’individus après nettoyage (n = {nrow(donnees_modeles)})."))
  }
  
  
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
  ) %>% sans_warning_proba()
  
  
  
  # Tests d'ajustement du modèle -----------------------------------------------
  pval_ajustement <- o.r.test(modele_glm)
  eta2 <- predict(modele_glm)^2
  modele_avec_eta2 <- sans_warning_proba(update(modele_glm, . ~ . + eta2))
  pval_lien <- anova(modele_glm, modele_avec_eta2, test = "Chisq")$`Pr(>Chi)`[2]
  
  commentaire <- NA_character_
  if (!modele_glm$converged) {
    commentaire <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if (pval_ajustement < 0.05 || pval_lien < 0.05) {
    commentaire <- "Ce modèle ne s’ajuste pas bien aux données. Il est préférable de choisir un autre modèle."
  }
  
  # Estimation du point de 50 % ------------------------------------------------
  if (modele == "TLO" && variable == "ltm" && lien == "logit") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
    
  } else if (modele == "TLO" && variable == "ltm" && lien == "probit") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
    
  } else if (modele == "TLO" && variable == "ltm" && lien == "cloglog") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
    
  } else if (modele == "TLO" && variable == "age" && lien == "logit") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
    
  } else if (modele == "TLO" && variable == "age" && lien == "probit") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
    
  } else if (modele == "TLO" && variable == "age" && lien == "cloglog") {
    point50_ic <- confint_L(modele_glm, method = "montecarlo", interval_type = "bca", nboot = nboot)
    point50 <- round(point50_ic[2])
    point50_inf <- round(point50_ic[1])
    point50_sup <- round(point50_ic[3])
  } else if (modele == "ADD") {
    coef_mod <- coef(modele_glm)
    
    if (variable == "ltm" && lien == "logit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / b1)
      
    } else if (variable == "ltm" && lien == "probit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / b1)
      
    } else if (variable == "ltm" && lien == "cloglog") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      kappa <- 0.3665129
      v50_f <- round((-(b0) - kappa) / b1)
      v50_m <- round((-(b0 + b2) - kappa) / b1)
      
    } else if (variable == "age" && lien == "logit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / b1)
      
    } else if (variable == "age" && lien == "probit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / b1)
      
    } else if (variable == "age" && lien == "cloglog") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      kappa <- 0.3665129
      v50_f <- round((-(b0) - kappa) / b1)
      v50_m <- round((-(b0 + b2) - kappa) / b1)
    }
    
    point50 <- list(fem = v50_f, male = v50_m)
    point50_inf <- point50_sup <- NA
    
  } else if (modele == "INT") {
    coef_mod <- coef(modele_glm)
    
    if (variable == "ltm" && lien == "logit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["ltm:sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / (b1 + b3))
      
    } else if (variable == "ltm" && lien == "probit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["ltm:sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / (b1 + b3))
      
    } else if (variable == "ltm" && lien == "cloglog") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["ltm"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["ltm:sexeM"]
      kappa <- 0.3665129
      v50_f <- round((-b0 - kappa) / b1)
      v50_m <- round((-b0 - b2 - kappa) / (b1 + b3))
      
    } else if (variable == "age" && lien == "logit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["age:sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / (b1 + b3))
      
    } else if (variable == "age" && lien == "probit") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["age:sexeM"]
      v50_f <- round((-b0) / b1)
      v50_m <- round((-b0 - b2) / (b1 + b3))
      
    } else if (variable == "age" && lien == "cloglog") {
      b0 <- coef_mod["(Intercept)"]
      b1 <- coef_mod["age"]
      b2 <- coef_mod["sexeM"]
      b3 <- coef_mod["age:sexeM"]
      kappa <- 0.3665129
      v50_f <- round((-b0 - kappa) / b1)
      v50_m <- round((-b0 - b2 - kappa) / (b1 + b3))
    }
    
    point50 <- list(fem = v50_f, male = v50_m)
    point50_inf <- point50_sup <- NA
    
  } else if (modele == "COM") {
    coef_mod <- coef(modele_glm)
    
    if (variable == "ltm" && lien == "logit") {
      b1 <- coef_mod["ltm:sexeF"]
      b2 <- coef_mod["ltm:sexeM"]
      v50_f <- round(-log(2) / b1)
      v50_m <- round(-log(2) / b2)
      
    } else if (variable == "ltm" && lien == "probit") {
      b1 <- coef_mod["ltm:sexeF"]
      b2 <- coef_mod["ltm:sexeM"]
      v50_f <- round(-qnorm(0.5) / b1)
      v50_m <- round(-qnorm(0.5) / b2)
      
    } else if (variable == "ltm" && lien == "cloglog") {
      kappa <- 0.3665129
      b1 <- coef_mod["ltm:sexeF"]
      b2 <- coef_mod["ltm:sexeM"]
      v50_f <- round(-kappa / b1)
      v50_m <- round(-kappa / b2)
      
    } else if (variable == "age" && lien == "logit") {
      b1 <- coef_mod["age:sexeF"]
      b2 <- coef_mod["age:sexeM"]
      v50_f <- round(-log(2) / b1)
      v50_m <- round(-log(2) / b2)
      
    } else if (variable == "age" && lien == "probit") {
      b1 <- coef_mod["age:sexeF"]
      b2 <- coef_mod["age:sexeM"]
      v50_f <- round(-qnorm(0.5) / b1)
      v50_m <- round(-qnorm(0.5) / b2)
      
    } else if (variable == "age" && lien == "cloglog") {
      kappa <- 0.3665129
      b1 <- coef_mod["age:sexeF"]
      b2 <- coef_mod["age:sexeM"]
      v50_f <- round(-kappa / b1)
      v50_m <- round(-kappa / b2)
    }
    
    point50 <- list(fem = v50_f, male = v50_m)
    point50_inf <- point50_sup <- NA
  }
  
  # Calcul des prédictions pour la courbe ---------------------------------------
  if (modele == "TLO") {
    x_min <- min(donnees_modeles[[variable]], na.rm = TRUE)
    x_max <- max(donnees_modeles[[variable]], na.rm = TRUE)
    donnees_prediction <- data.frame(temp = seq(from = x_min, to = x_max, by = 1))
    names(donnees_prediction) <- variable
  } else {
    form <- as.formula(glue::glue("{variable} ~ sexe"))
    var_minmax <- FSA::Summarize(form, data = donnees_modeles)
    donnees_prediction <- bind_rows(
      data.frame(sexe = "F", temp = seq(from = var_minmax$min[var_minmax$sexe == "F"], to = var_minmax$max[var_minmax$sexe == "F"], by = 1)),
      data.frame(sexe = "M", temp = seq(from = var_minmax$min[var_minmax$sexe == "M"], to = var_minmax$max[var_minmax$sexe == "M"], by = 1))
    )
    names(donnees_prediction)[names(donnees_prediction) == "temp"] <- variable
  }
  
  prediction_model <- predict(modele_glm, donnees_prediction, full = TRUE, type = "link", se.fit = TRUE)
  
  donnees_ogive <- donnees_prediction %>%
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
  # Déduction de l’étiquette principale
  etiquette <- if (variable == "ltm") "L" else "A"
  
  if (modele == "TLO") {
    intercept <- coef(modele_glm)["(Intercept)"]
    pente <- coef(modele_glm)[[variable]]
    
    table_resultats <- data.frame(
      intervalle = glue::glue("[{point50_inf}-{point50_sup}]"),
      b0 = round(intercept, 3),
      b1 = round(pente, 3)
    )
    
    # Ajout dynamique de la colonne "ltm50" ou "age50"
    col_point50 <- paste0(tolower(etiquette), "50")
    table_resultats[[col_point50]] <- point50
    table_resultats <- table_resultats[, c(col_point50, "intervalle", "b0", "b1")]
    
  } else {
    table_resultats <- data.frame(
      b0 = round(b0, 3),
      b1 = round(b1, 3)
    )
    
    col_point50_f <- paste0(tolower(etiquette), "50_f")
    col_point50_m <- paste0(tolower(etiquette), "50_m")
    
    table_resultats[[col_point50_f]] <- v50_f
    table_resultats[[col_point50_m]] <- v50_m
    
    if (modele %in% c("ADD", "INT","COM")) {
      table_resultats[["sexe"]] <- round(b2, 3)
    }
    if (modele == "INT") {
      table_resultats[["interaction"]] <- round(b3, 3)
    }
  }
  
  # Réorganisation des colonnes dans l’ordre voulu
  ordre_cols <- switch(modele,
                       "TLO" = c(col_point50, "intervalle", "b0", "b1"),
                       "ADD" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe"),
                       "COM" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe"),
                       "INT" = c(col_point50_m, col_point50_f, "b0", "b1", "sexe", "interaction")
  )
  
  table_resultats <- table_resultats[, ordre_cols]
  
  
  
  # Création du flextable
  ft <- flextable(table_resultats)
  
  # Définir les labels temporaires
  labels <- list(
    b0 = "b0",
    b1 = "b1"
  )
  
  # Ajout conditionnel des colonnes selon leur présence
  if ("sexe" %in% names(table_resultats)) labels$sexe <- "sexe"
  if ("interaction" %in% names(table_resultats)) labels$interaction <- "interaction"
  if ("intervalle" %in% names(table_resultats)) labels$intervalle <- "IC 95%"
  if ("l50" %in% names(table_resultats)) labels$l50 <- "tmp"
  if ("a50" %in% names(table_resultats)) labels$a50 <- "tmp"
  if ("l50_f" %in% names(table_resultats)) labels$l50_f <- "tmp_f"
  if ("l50_m" %in% names(table_resultats)) labels$l50_m <- "tmp_m"
  if ("a50_f" %in% names(table_resultats)) labels$a50_f <- "tmp_f"
  if ("a50_m" %in% names(table_resultats)) labels$a50_m <- "tmp_m"
  
  # Appliquer les étiquettes temporaires
  ft <- do.call(set_header_labels, c(list(ft), labels))
  
  # Appliquer les indices (b0, b1)
  ft <- ft %>%
    compose(j = "b0", part = "header", value = as_paragraph("b", as_sub("0"))) %>%
    compose(j = "b1", part = "header", value = as_paragraph("b", as_sub("1")))
  
  # Appliquer les étiquettes dynamiques pour point50
  if ("l50" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "l50", part = "header", value = as_paragraph("L", as_sub("50")))
  }
  if ("a50" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "a50", part = "header", value = as_paragraph("A", as_sub("50")))
  }
  if ("l50_f" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "l50_f", part = "header", value = as_paragraph("L", as_sub("50"), " – Femelle"))
  }
  if ("l50_m" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "l50_m", part = "header", value = as_paragraph("L", as_sub("50"), " – Mâle"))
  }
  if ("a50_f" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "a50_f", part = "header", value = as_paragraph("A", as_sub("50"), " – Femelle"))
  }
  if ("a50_m" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "a50_m", part = "header", value = as_paragraph("A", as_sub("50"), " – Mâle"))
  }
  if ("intervalle" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "intervalle", part = "header", value = as_paragraph("IC 95%"))
  }
  if ("sexe" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "sexe", part = "header", value = as_paragraph("sexe"))
  }
  if ("interaction" %in% names(table_resultats)) {
    ft <- ft %>% compose(j = "interaction", part = "header", value = as_paragraph("interaction"))
  }
  
  # Mise en forme finale
  ft <- ft %>%
    autofit() %>%
    align(align = "center", part = "all") %>%
    bold(part = "header")
  
  # Résultat
  table_resultats_flextable <- ft
  # table_resultats_flextable
  
  
  # Graphique -------------------------------------------------------------------
  color_by_sex <- "sexe" %in% names(donnees_ogive)
  graphique <- ggplot(data = donnees_ogive, aes(x = .data[[variable]], y = maturite)) +
    { if (color_by_sex) geom_line(aes(color = sexe)) else geom_line(color = "black") } +
    geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.1, fill = "blue") +
    { if (color_by_sex) scale_color_manual(values = c("F" = "red", "M" = "black")) else NULL } +
    theme_classic() +
    labs(
      x = ifelse(variable == "ltm", "Longueur totale maximale (mm)", "Âge"),
      y = "Proportion reproducteurs actifs",
      title = "Ogive de maturité"
    ) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "none")
  
  if (modele == "TLO") {
    graphique <- graphique +
      annotate("segment", x = point50, xend = point50, y = 0, yend = 0.5, color = "black", lty = 2) +
      annotate("segment", x = x_min, xend = point50, y = 0.5, yend = 0.5, color = "black", lty = 2)
  }
  
  if (modele == "COM") {
    # Pour COM : les prédictions doivent être faites séparément pour chaque sexe
    vmin_m <- min(donnees_modeles %>% filter(sexe == "M") %>% pull(.data[[variable]]))
    vmin_f <- min(donnees_modeles %>% filter(sexe == "F") %>% pull(.data[[variable]]))
    
    graphique <- graphique +
      annotate("segment", x = v50_m, xend = v50_m, y = 0, yend = 0.5, color = "black", lty = 2) +
      annotate("segment", x = vmin_m, xend = v50_m, y = 0.5, yend = 0.5, color = "black", lty = 2) +
      annotate("segment", x = v50_f, xend = v50_f, y = 0, yend = 0.5, color = "red", lty = 2) +
      annotate("segment", x = vmin_f, xend = v50_f, y = 0.5, yend = 0.5, color = "red", lty = 2)
  }
  
  if (modele %in% c("ADD",  "INT")) {
    form <- as.formula(glue::glue("{variable} ~ sexe"))
    var_minmax <- FSA::Summarize(form, data = donnees_modeles)
    vmin_m <- var_minmax %>% filter(sexe == "M") %>% pull(min)
    vmin_f <- var_minmax %>% filter(sexe == "F") %>% pull(min)
    
    graphique <- graphique +
      annotate("segment", x = v50_m, xend = v50_m, y = 0, yend = 0.5, color = "black", lty = 2) +
      annotate("segment", x = vmin_m, xend = v50_m, y = 0.5, yend = 0.5, color = "black", lty = 2) +
      annotate("segment", x = v50_f, xend = v50_f, y = 0, yend = 0.5, color = "red", lty = 2) +
      annotate("segment", x = vmin_f, xend = v50_f, y = 0.5, yend = 0.5, color = "red", lty = 2)
  }
  
  graphique <- graphique +
    geom_point(
      data = donnees_modeles,
      mapping = aes(
        x = .data[[variable]],
        y = as.numeric(maturite) - 1,
        color = if (color_by_sex) sexe else NULL
      ),
      alpha = 0.5
    )
  
  return(list(
    table_resultats = table_resultats,
    table_resultats_flextable = table_resultats_flextable,
    commentaire = commentaire,
    graphique = graphique,
    donnees_ogive = donnees_ogive
  ))
}
