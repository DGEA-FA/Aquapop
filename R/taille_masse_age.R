#' Tableau des statistiques morphologiques (taille, masse, âge)
#'
#' Calcule des statistiques descriptives (effectif, moyenne, écart-type, minimum,
#' maximum) pour la longueur totale (`ltm`), la masse et l'âge des spécimens,
#' selon différents groupes biologiques (sexe et statut reproducteur).
#'
#' Retourne à la fois un tableau brut (`data.frame`) et une version formatée
#' avec `flextable`. Si aucune donnée exploitable n'est disponible, la fonction
#' retourne un objet structuré avec `success = FALSE`, sans générer d'erreur.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes `ltm`, `masse`,
#'   `age`, `sexe` et `maturite`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Indique si le tableau a pu être produit}
#'   \item{data}{Tableau brut (`data.frame`) des statistiques morphologiques par groupe}
#'   \item{flextable}{Tableau mis en forme avec `flextable` pour affichage ou export}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @importFrom tidyselect ends_with
#' @importFrom dplyr inner_join mutate rename rename_with across bind_rows filter recode arrange
#' @importFrom tibble tibble
#' @importFrom flextable flextable merge_h set_header_df border set_caption colformat_double
#' @importFrom officer fp_border
#' @importFrom checkmate assert_data_frame assert_numeric assert_subset
#'
#' @examples
#' data_exemple <- data.frame(
#'   ltm = c(150, 180, 170, 190, NA),
#'   masse = c(80, 90, 85, 95, NA),
#'   age = c(2, 3, 2, 3, NA),
#'   sexe = c("M", "F", "F", "M", "IND"),
#'   maturite = c("O", "O", "N", "N", "IND")
#' )
#'
#' res <- taille_masse_age(data_exemple)
#' res$data
#' if (requireNamespace("flextable", quietly = TRUE)) res$flextable
#'
#' @export
taille_masse_age <- function(data) {
  
  # Validation des données ----
  assert_data_frame(data)
  
  colonnes_requises <- c("ltm", "masse", "age", "sexe", "maturite")
  assert_subset(colonnes_requises, colnames(data))
  
  assert_numeric(data$ltm, null.ok = TRUE)
  assert_numeric(data$masse, null.ok = TRUE)
  assert_numeric(data$age, null.ok = TRUE)
  
  # Cas sans ligne ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun spécimen valide disponible pour produire le tableau de taille, masse et âge.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Cas sans donnée exploitable ----
  if (all(is.na(data$ltm)) && all(is.na(data$masse)) && all(is.na(data$age))) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = paste(
        "Aucune donnée exploitable n'est disponible pour les variables",
        "ltm, masse et age."
      )
    ))
  }
  
  # Calcul des statistiques morphologiques ----
  table_ltm <- regrouper_stats_morpho(data, "ltm") |>
    rename_with(~ paste0("ltm_", .), -"sexe")
  
  table_masse <- regrouper_stats_morpho(data, "masse") |>
    rename_with(~ paste0("masse_", .), -"sexe")
  
  table_age <- regrouper_stats_morpho(data, "age") |>
    rename_with(~ paste0("age_", .), -"sexe")
  
  # Fusion des tableaux ----
  table_resultats <- table_ltm |>
    inner_join(table_masse, by = "sexe") |>
    inner_join(table_age, by = "sexe") |>
    rename(Sexe = "sexe") 
  
  # Création du tableau flextable ----
  bordure_normale <- fp_border(width = 1)
  
  en_tete <- tibble(
    col_keys = names(table_resultats),
    Niveau1 = c("Groupe", rep("LTMax (mm)", 5), rep("Masse (g)", 5), rep("Âge", 5)),
    Niveau2 = c("", rep(c("N", "Moyenne", "ÉT", "Min", "Max"), 3))
  )
  
  table_flextable <- table_resultats |>
    flextable() |>
    set_header_df(mapping = en_tete, key = "col_keys") |>
    merge_h(part = "header") |>
    border(i = 2, border.bottom = bordure_normale, part = "header") |>
    set_caption("Aperçu des données morphologiques") |>
    style_flextable_aquapop() |>
    colformat_double(
      j = grep("moy|e_t|min|max", names(table_resultats), value = TRUE),
      digits = 1,
      decimal.mark = ",",
      big.mark = " ",
      na_str = "-"
    ) |>
    colformat_double(
      j = grep("_nb$", names(table_resultats), value = TRUE),
      digits = 0,
      decimal.mark = ",",
      big.mark = " ",
      na_str = "-"
    )
  
  for (col in c("ltm_max", "masse_max")) {
    table_flextable <- table_flextable |>
      border(i = 1:2, j = col, border.right = bordure_normale, part = "header") |>
      border(j = col, border.right = bordure_normale, part = "body")
  }
  
  ligne_bloc_repro <- which(table_resultats$Sexe == "Reprod. actifs femelles")
  
  if (length(ligne_bloc_repro) == 1) {
    table_flextable <- table_flextable |>
      border(
        i = ligne_bloc_repro - 1,
        border.bottom = bordure_normale,
        part = "body"
      )
  }
  
  # Retour ----
  return(list(
    success = TRUE,
    data = table_resultats,
    flextable = table_flextable,
    message = NULL
  ))
}

#' Regrouper les statistiques morphologiques par groupe biologique
#'
#' Fonction interne. Applique `stats_morpho()` à plusieurs sous-groupes définis
#' par les variables `sexe` et `maturite`, et retourne un tableau consolidé prêt à fusionner.
#'
#' @param data Un `data.frame` contenant les variables morphologiques.
#' @param var Chaîne de caractères correspondant à la variable numérique à résumer (`"ltm"`, `"masse"`, `"age"`).
#'
#' @return Un `data.frame` avec une colonne `sexe` (libellé du groupe) et les statistiques correspondantes.
#'
#' @keywords internal
regrouper_stats_morpho <- function(data, var) {
  
  # --- Calcul des statistiques pour chaque sous-groupe ---
  table_groupes <- bind_rows(
    stats_morpho(data, var, "sexe"),
    stats_morpho(data, var) |> mutate(sexe = NA),
    stats_morpho(filter(data, .data$maturite == "O" & .data$sexe == "M"), var) |> mutate(sexe = "Reprod. actifs mâles"),
    stats_morpho(filter(data, .data$maturite == "N"), var) |> mutate(sexe = "Imm. ou reprod. inactifs"),
    stats_morpho(filter(data, .data$maturite == "O" & .data$sexe == "F"), var) |> mutate(sexe = "Reprod. actifs femelles"),
    stats_morpho(filter(data, .data$maturite == "IND"), var) |> mutate(sexe = "Statut reprod. inconnu")
  )
  
  # --- Nettoyage et harmonisation des libellés de groupes ---
  table_groupes <- table_groupes |>
    mutate(
      sexe = as.character(.data$sexe),
      sexe = ifelse(is.na(.data$sexe), "Tous", .data$sexe),
      sexe = recode(.data$sexe,
                    "M"   = "Mâle",
                    "F"   = "Femelle",
                    "IND" = "Sexe inconnu",
                    .default = .data$sexe),
      sexe = factor(.data$sexe, levels = c(
        "Tous", "Femelle", "Mâle", "Sexe inconnu",
        "Reprod. actifs femelles", "Reprod. actifs mâles",
        "Imm. ou reprod. inactifs", "Statut reprod. inconnu"
      ))
    ) |>
    arrange(.data$sexe)
  
  return(table_groupes)
}

#' Statistiques descriptives sur une variable morphologique
#'
#' Fonction utilitaire interne. Résume une variable numérique (`ltm`, `masse`, `age`)
#' globalement ou par groupe, en calculant le nombre de valeurs non manquantes, la moyenne,
#' l'écart-type, le minimum et le maximum.
#'
#' @param data Un `data.frame` contenant la variable à résumer.
#' @param var Chaîne de caractères : nom de la variable numérique à résumer.
#' @param group_var Chaîne de caractères : nom de la variable de regroupement (optionnelle).
#'
#' @return Un `data.frame` avec les statistiques résumées.
#'
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @importFrom stats sd
#'
#' @keywords internal
stats_morpho <- function(data, var, group_var = NULL) {
  
  # --- Regroupement (si applicable) ---
  if (!is.null(group_var)) {
    data <- data |> group_by(!!sym(group_var), .drop = FALSE)
  }
  
  # --- Calcul des statistiques descriptives ---
  data |>
    summarise(
      nb  = sum(!is.na(!!sym(var))),
      moy = ifelse(all(is.na(!!sym(var))), NA_real_, mean(!!sym(var), na.rm = TRUE)),
      e_t = ifelse(all(is.na(!!sym(var))), NA_real_, sd(!!sym(var), na.rm = TRUE)),
      min = ifelse(all(is.na(!!sym(var))), NA_real_, min(!!sym(var), na.rm = TRUE)),
      max = ifelse(all(is.na(!!sym(var))), NA_real_, max(!!sym(var), na.rm = TRUE)),
      .groups = "drop"
    )
}
