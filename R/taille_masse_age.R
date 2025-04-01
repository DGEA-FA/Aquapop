#' Génère un tableau morphologique (taille, masse, âge) au format brut ou flextable
#'
#' Cette fonction calcule des statistiques descriptives (N, moyenne, écart-type, min, max)
#' pour la longueur totale (LTMax), la masse et l'âge des spécimens, regroupés par sexe et statut reproducteur.
#' Elle retourne soit un `data.frame`, soit un `flextable` mis en page selon l'argument `format`.
#'
#' @param data_specimen_valid Un data.frame contenant les colonnes `ltm`, `masse`, `age`, `sexe` et `maturite`.
#' @param format Format de sortie : `"data.frame"` (par défaut) ou `"flextable"`.
#'
#' @return Un tableau de statistiques morphologiques au format spécifié.
#' @export
taille_masse_age <- function(data_specimen_valid, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
  calculate_stats <- function(data, var, group_var = NULL) {
    if (!is.null(group_var)) {
      data <- data %>% group_by(!!sym(group_var), .drop = FALSE)
    }
    data %>%
      summarise(
        nb  = sum(!is.na(!!sym(var))),
        moy = ifelse(all(is.na(!!sym(var))), NA, mean(!!sym(var), na.rm = TRUE)) %>% round(1),
        e_t = ifelse(all(is.na(!!sym(var))), NA, sd(!!sym(var), na.rm = TRUE)) %>% round(1),
        min = ifelse(all(is.na(!!sym(var))), NA, min(!!sym(var), na.rm = TRUE)) %>% round(1),
        max = ifelse(all(is.na(!!sym(var))), NA, max(!!sym(var), na.rm = TRUE)) %>% round(1),
        .groups = "drop"
      )
  }
  
  # ---- Données LTMax ----
  ltm_df <- bind_rows(
    calculate_stats(data_specimen_valid, "ltm", "sexe"),
    calculate_stats(data_specimen_valid, "ltm") %>% mutate(sexe = NA),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "M"), "ltm") %>% mutate(sexe = "Reprod. actifs mâles"),
    calculate_stats(filter(data_specimen_valid, maturite == "N"), "ltm") %>% mutate(sexe = "Imm. ou reprod. inactifs"),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "F"), "ltm") %>% mutate(sexe = "Reprod. actifs femelles"),
    calculate_stats(filter(data_specimen_valid, maturite == "IND"), "ltm") %>% mutate(sexe = "Statut reprod. inconnu")
  ) %>%
    mutate(
      sexe = as.character(sexe),
      sexe = ifelse(is.na(sexe), "Tous", sexe),
      sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),
      sexe = factor(sexe, levels = c("Tous", "Femelle", "Mâle", "Sexe inconnu",
                                     "Reprod. actifs femelles", "Reprod. actifs mâles",
                                     "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))
    ) %>%
    arrange(sexe)
  
  # ---- Masse ----
  masse_df <- bind_rows(
    calculate_stats(data_specimen_valid, "masse", "sexe"),
    calculate_stats(data_specimen_valid, "masse") %>% mutate(sexe = NA),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "M"), "masse") %>% mutate(sexe = "Reprod. actifs mâles"),
    calculate_stats(filter(data_specimen_valid, maturite == "N"), "masse") %>% mutate(sexe = "Imm. ou reprod. inactifs"),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "F"), "masse") %>% mutate(sexe = "Reprod. actifs femelles"),
    calculate_stats(filter(data_specimen_valid, maturite == "IND"), "masse") %>% mutate(sexe = "Statut reprod. inconnu")
  ) %>%
    mutate(
      sexe = as.character(sexe),
      sexe = ifelse(is.na(sexe), "Tous", sexe),
      sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),
      sexe = factor(sexe, levels = levels(ltm_df$sexe))
    ) %>%
    arrange(sexe)
  
  # ---- Âge ----
  age_df <- bind_rows(
    calculate_stats(data_specimen_valid, "age", "sexe"),
    calculate_stats(data_specimen_valid, "age") %>% mutate(sexe = NA),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "M"), "age") %>% mutate(sexe = "Reprod. actifs mâles"),
    calculate_stats(filter(data_specimen_valid, maturite == "N"), "age") %>% mutate(sexe = "Imm. ou reprod. inactifs"),
    calculate_stats(filter(data_specimen_valid, maturite == "O" & sexe == "F"), "age") %>% mutate(sexe = "Reprod. actifs femelles"),
    calculate_stats(filter(data_specimen_valid, maturite == "IND"), "age") %>% mutate(sexe = "Statut reprod. inconnu")
  ) %>%
    mutate(
      sexe = as.character(sexe),
      sexe = ifelse(is.na(sexe), "Tous", sexe),
      sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),
      sexe = factor(sexe, levels = levels(ltm_df$sexe))
    ) %>%
    arrange(sexe)
  
  # ---- Fusion ----
  ltm_df    <- rename_with(ltm_df, ~ paste0("ltm_", .), -sexe)
  masse_df  <- rename_with(masse_df, ~ paste0("masse_", .), -sexe)
  age_df    <- rename_with(age_df, ~ paste0("age_", .), -sexe)
  
  complet <- ltm_df %>%
    inner_join(masse_df, by = "sexe") %>%
    inner_join(age_df, by = "sexe") %>%
    rename(Sexe = sexe) %>%
    mutate(across(ends_with(c("min", "max", "moy", "e_t")),
                  ~ ifelse(. %in% c("Inf", "-Inf") | is.na(.), "-", .)))
  
  # ---- Optionnel : sortie flextable ----
  if (format == "flextable") {
    normal_border <- officer::fp_border(width = 1)
    header <- tibble::tibble(
      col_keys = names(complet),
      Niveau1 = c("Groupe", rep("LTMax (mm)", 5), rep("Masse (g)", 5), rep("Âge", 5)),
      Niveau2 = c("", rep(c("N", "Moyenne", "ÉT", "Min", "Max"), 3))
    )
    
    complet <- complet %>%
      flextable::flextable() %>%
      flextable::set_header_df(mapping = header, key = "col_keys") %>%
      flextable::merge_h(part = "header") %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::autofit() %>%
      flextable::border(i = 2, border.bottom = normal_border, part = "header") %>%
      flextable::fontsize(size = 10, part = "all")
    
    for (col in c("ltm_max", "masse_max")) {
      complet <- complet %>%
        flextable::border(i = 1:2, j = col, border.right = normal_border, part = "header") %>%
        flextable::border(j = col, border.right = normal_border, part = "body")
    }
  }
  
  return(complet)
}
