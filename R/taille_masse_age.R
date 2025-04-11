#' Génère un tableau morphologique (taille, masse, âge) au format brut et flextable
#'
#' Cette fonction calcule des statistiques descriptives (N, moyenne, écart-type, min, max)
#' pour la longueur totale (LTMax), la masse et l'âge des spécimens, regroupés par sexe et statut reproducteur.
#' Elle retourne à la fois un `data.frame` brut et un tableau `flextable` mis en page.
#'
#' @param data Un data.frame contenant les colonnes `ltm`, `masse`, `age`, `sexe` et `maturite`.
#'
#' @return Une liste avec deux éléments : `data` (data.frame brut) et `flextable` (tableau formaté)
#' @export
taille_masse_age <- function(data) {
  
  stats_morpho <- function(data, var, group_var = NULL) {
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
  
  regrouper_stats_morpho <- function(var) {
    bind_rows(
      stats_morpho(data, var, "sexe"),
      stats_morpho(data, var) %>% mutate(sexe = NA),
      stats_morpho(filter(data, maturite == "O" & sexe == "M"), var) %>% mutate(sexe = "Reprod. actifs mâles"),
      stats_morpho(filter(data, maturite == "N"), var) %>% mutate(sexe = "Imm. ou reprod. inactifs"),
      stats_morpho(filter(data, maturite == "O" & sexe == "F"), var) %>% mutate(sexe = "Reprod. actifs femelles"),
      stats_morpho(filter(data, maturite == "IND"), var) %>% mutate(sexe = "Statut reprod. inconnu")
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
  }
  
  ltm_df   <- regrouper_stats_morpho("ltm")   %>% rename_with(~ paste0("ltm_", .), -sexe)
  masse_df <- regrouper_stats_morpho("masse") %>% rename_with(~ paste0("masse_", .), -sexe)
  age_df   <- regrouper_stats_morpho("age")   %>% rename_with(~ paste0("age_", .), -sexe)
  
  complet_df <- ltm_df %>%
    inner_join(masse_df, by = "sexe") %>%
    inner_join(age_df, by = "sexe") %>%
    rename(Sexe = sexe) %>%
    mutate(across(ends_with(c("min", "max", "moy", "e_t")),
                  ~ ifelse(. %in% c("Inf", "-Inf") | is.na(.), "-", .)))
  
  # Création du flextable
  normal_border <- officer::fp_border(width = 1)
  header <- tibble::tibble(
    col_keys = names(complet_df),
    Niveau1 = c("Groupe", rep("LTMax (mm)", 5), rep("Masse (g)", 5), rep("Âge", 5)),
    Niveau2 = c("", rep(c("N", "Moyenne", "ÉT", "Min", "Max"), 3))
  )
  
  complet_ft <- complet_df %>%
    flextable::flextable() %>%
    flextable::set_header_df(mapping = header, key = "col_keys") %>%
    flextable::merge_h(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit() %>%
    flextable::border(i = 2, border.bottom = normal_border, part = "header") %>%
    flextable::fontsize(size = 10, part = "all")
  
  for (col in c("ltm_max", "masse_max")) {
    complet_ft <- complet_ft %>%
      flextable::border(i = 1:2, j = col, border.right = normal_border, part = "header") %>%
      flextable::border(j = col, border.right = normal_border, part = "body")
  }
  
  return(list(
    data = complet_df,
    flextable = complet_ft
  ))
}
