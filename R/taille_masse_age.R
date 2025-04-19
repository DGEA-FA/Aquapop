#' Génère un tableau morphologique (taille, masse, âge) au format brut et flextable
#'
#' Cette fonction calcule des statistiques descriptives (N, moyenne, écart-type, min, max)
#' pour la longueur totale (ltm), la masse et l'âge des spécimens, regroupés par sexe et statut reproducteur.
#' Elle retourne à la fois un `data.frame` brut et un tableau `flextable` mis en page.
#'
#' @param data Un data.frame contenant les colonnes `ltm`, `masse`, `age`, `sexe` et `maturite`.
#'
#' @return Une liste contenant deux éléments :
#' \describe{
#'   \item{data}{Tableau brut (`data.frame`) des statistiques morphologiques}
#'   \item{flextable}{Version mise en page du tableau avec `flextable`}
#' }
#'
#' @importFrom checkmate assert_data_frame assert_subset assert_numeric assert_character
#' @export
taille_masse_age <- function(data) {
  
  # Validation des données ----
  checkmate::assert_data_frame(data, min.rows = 1)
  
  colonnes_requises <- c("ltm", "masse", "age", "sexe", "maturite")
  checkmate::assert_subset(colonnes_requises, colnames(data))
  
  checkmate::assert_numeric(data$ltm, null.ok = FALSE)
  checkmate::assert_numeric(data$masse, null.ok = FALSE)
  checkmate::assert_numeric(data$age, null.ok = FALSE)
  
  if (all(is.na(data$ltm)) && all(is.na(data$masse)) && all(is.na(data$age))) {
    stop("Aucune donnée disponible pour les variables ltm, masse ou age.")
  }
  
  # Calcul des statistiques morphologiques ----
  table_ltm   <- .regrouper_stats_morpho(data, "ltm")   %>% rename_with(~ paste0("ltm_", .), -sexe)
  table_masse <- .regrouper_stats_morpho(data, "masse") %>% rename_with(~ paste0("masse_", .), -sexe)
  table_age   <- .regrouper_stats_morpho(data, "age")   %>% rename_with(~ paste0("age_", .), -sexe)
  
  # Fusion des tableaux ----
  table_resultats <- table_ltm %>%
    inner_join(table_masse, by = "sexe") %>%
    inner_join(table_age, by = "sexe") %>%
    rename(Sexe = sexe) %>%
    mutate(across(ends_with(c("min", "max", "moy", "e_t")),
                  ~ ifelse(. %in% c("Inf", "-Inf") | is.na(.), "-", .)))
  
  # Création du tableau flextable ----
  bordure_normale <- officer::fp_border(width = 1)
  en_tete <- tibble::tibble(
    col_keys = names(table_resultats),
    Niveau1 = c("Groupe", rep("LTMax (mm)", 5), rep("Masse (g)", 5), rep("Âge", 5)),
    Niveau2 = c("", rep(c("N", "Moyenne", "ÉT", "Min", "Max"), 3))
  )
  
  table_flextable <- table_resultats %>%
    flextable::flextable() %>%
    flextable::set_header_df(mapping = en_tete, key = "col_keys") %>%
    flextable::merge_h(part = "header") %>%
    flextable::border(i = 2, border.bottom = bordure_normale, part = "header") %>%
    style_flextable_aquapop()
  
  
  for (col in c("ltm_max", "masse_max")) {
    table_flextable <- table_flextable %>%
      flextable::border(i = 1:2, j = col, border.right = bordure_normale, part = "header") %>%
      flextable::border(j = col, border.right = bordure_normale, part = "body")
  }
  
  ligne_bloc_repro <- which(table_resultats$Sexe == "Reprod. actifs femelles")
  if (length(ligne_bloc_repro) == 1) {
    table_flextable <- table_flextable %>%
      flextable::border(i = ligne_bloc_repro - 1, border.bottom = bordure_normale, part = "body")
  }
  
  
  # Retour de la liste des objets ----
  return(list(
    data = table_resultats,
    flextable = table_flextable
  ))
}

#' Regrouper les statistiques morphologiques par sexe et statut reproducteur
#'
#' Fonction interne. Applique `.stats_morpho()` à différents sous-groupes définis
#' par les variables `sexe` et `maturite`. Retourne un tableau consolidé prêt pour fusion.
#'
#' @param data Un `data.frame` contenant les données morphologiques.
#' @param var Nom de la variable numérique à résumer (`"ltm"`, `"masse"`, `"age"`).
#'
#' @return Un `data.frame` avec une colonne `sexe` (groupe) et les statistiques associées.
#' @keywords internal
.regrouper_stats_morpho <- function(data, var) {
  
  # --- Calcul des statistiques pour chaque sous-groupe ---
  table_groupes <- bind_rows(
    .stats_morpho(data, var, "sexe"),
    .stats_morpho(data, var) %>% mutate(sexe = NA),
    .stats_morpho(filter(data, maturite == "O" & sexe == "M"), var) %>% mutate(sexe = "Reprod. actifs mâles"),
    .stats_morpho(filter(data, maturite == "N"), var) %>% mutate(sexe = "Imm. ou reprod. inactifs"),
    .stats_morpho(filter(data, maturite == "O" & sexe == "F"), var) %>% mutate(sexe = "Reprod. actifs femelles"),
    .stats_morpho(filter(data, maturite == "IND"), var) %>% mutate(sexe = "Statut reprod. inconnu")
  )
  
  # --- Nettoyage et harmonisation des libellés de groupes ---
  table_groupes <- table_groupes %>%
    mutate(
      sexe = as.character(sexe),
      sexe = ifelse(is.na(sexe), "Tous", sexe),
      sexe = dplyr::recode(sexe,
                           "M"   = "Mâle",
                           "F"   = "Femelle",
                           "IND" = "Sexe inconnu",
                           .default = sexe),
      sexe = factor(sexe, levels = c(
        "Tous", "Femelle", "Mâle", "Sexe inconnu",
        "Reprod. actifs femelles", "Reprod. actifs mâles",
        "Imm. ou reprod. inactifs", "Statut reprod. inconnu"
      ))
    ) %>%
    arrange(sexe)
  
  return(table_groupes)
}

#' Calculer des statistiques descriptives sur une variable morphologique
#'
#' Fonction utilitaire interne. Résume une variable numérique (`ltm`, `masse`, `age`)
#' par groupe (ou globalement), en calculant : N, moyenne, écart-type, minimum, maximum.
#'
#' @param data Un `data.frame` contenant la variable à résumer.
#' @param var Nom de la variable numérique à résumer (chaîne de caractères).
#' @param group_var Nom de la variable de regroupement (optionnel, ex: `"sexe"`).
#'
#' @return Un `data.frame` avec les statistiques résumées, par groupe si applicable.
#' @keywords internal
.stats_morpho <- function(data, var, group_var = NULL) {
  
  # --- Regroupement (si applicable) ---
  if (!is.null(group_var)) {
    data <- data %>% group_by(!!sym(group_var), .drop = FALSE)
  }
  
  # --- Calcul des statistiques descriptives ---
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
