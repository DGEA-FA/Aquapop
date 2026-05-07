#' Créer un tableau d'abondance structuré par groupe biologique
#'
#' Cette fonction calcule les effectifs et proportions de différents groupes biologiques
#' à partir d’un jeu de spécimens filtré (une espèce cible, un lac, une année, etc.).
#' Elle ajoute les colonnes de CPUE et d'intervalles de confiance (IC 95 %) extraites des meilleurs modèles.
#'
#' @param data Un `data.frame` de spécimens filtrés, contenant minimalement les colonnes `sexe` et `maturite`.
#' @param cpue_table_tous Résultats des modèles CPUE pour tous les individus (format = "data.frame").
#' @param cpue_table_femelles Résultats des modèles CPUE pour les femelles reproductrices.
#' @param best_model_tous Nom du meilleur modèle CPUE pour le groupe "Tous".
#' @param best_model_femelles Nom du meilleur modèle CPUE pour le groupe "Repro. actifs femelles".
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{data}{Un `data.frame` résumant les abondances, proportions, CPUE et IC 95 % par groupe.}
#'   \item{flextable}{Un objet `flextable` prêt à l’exportation.}
#' }
#'
#' @importFrom dplyr count filter mutate recode select summarise bind_rows arrange case_when n
#' @importFrom tidyr complete 
#' @importFrom tibble tibble
#' @importFrom flextable flextable set_caption colformat_double hline
#' @importFrom officer fp_border
#'
#' @export
cpue_abondance_table <- function(data,
                                 cpue_table_tous,
                                 cpue_table_femelles,
                                 best_model_tous,
                                 best_model_femelles) {
  # --- Statistiques globales ---
  total_individus <- nrow(data)

  # --- Extraction des CPUE et IC 95 % ---
  best_tous <- cpue_table_tous[cpue_table_tous$methode == best_model_tous, ]
  best_femelles <- cpue_table_femelles[cpue_table_femelles$methode == best_model_femelles, ]

  cpue_tous <- best_tous$cpue
  ic95_tous <- best_tous$ic95

  cpue_femelles <- best_femelles$cpue
  ic95_femelles <- best_femelles$ic95

  # --- Tableaux par groupe biologique ---

  # Groupe : Tous
  table_tous <- tibble(
    groupe = "Tous",
    abondance = total_individus,
    proportion = 100,
    mf_ratio = calculate_mf_ratio(sum(data$sexe == "M", na.rm = TRUE), sum(data$sexe == "F", na.rm = TRUE))
  )

  # Groupe : Sexe
  table_sexe <- data |>
    count(.data$sexe, name = "abondance") |>
    mutate(
      groupe = recode(.data$sexe,
        "F" = "Femelles",
        "M" = "Mâles",
        "IND" = "Sexe inconnu"
      ),
      proportion = .data$abondance / total_individus * 100,      mf_ratio = NA_character_
    ) |>
    select("groupe", "abondance", "proportion", "mf_ratio")

  # Groupe : Reproducteurs actifs
  table_repro <- data |>
    filter(.data$maturite == "O", .data$sexe %in% c("M", "F")) |>
    count(.data$sexe, name = "abondance") |>
    mutate(
      groupe = recode(.data$sexe,
                      "F" = "Repro. actifs femelles",
                      "M" = "Repro. actifs mâles"
      ),
      proportion = .data$abondance / total_individus * 100,
      mf_ratio = NA_character_
    ) |>
    complete(
      groupe = c("Repro. actifs femelles", "Repro. actifs mâles"),
      fill = list(abondance = 0, proportion = 0, mf_ratio = NA_character_)
    )

  # Groupe : Immatures ou inactifs
  table_inactifs <- data |>
    filter(.data$maturite == "N") |>
    summarise(
      groupe = "Immatures ou reprod. inactifs",
      abondance = n(),
      proportion = .data$abondance / total_individus * 100,
      mf_ratio = calculate_mf_ratio(sum(.data$sexe == "M", na.rm = TRUE), sum(.data$sexe == "F", na.rm = TRUE))
    )

  # Groupe : Statut inconnu
  table_inconnu <- data |>
    filter(.data$maturite == "IND") |>
    summarise(
      groupe = "Statut reprod. inconnu",
      abondance = n(),
      proportion = .data$abondance / total_individus * 100,
      mf_ratio = calculate_mf_ratio(sum(.data$sexe == "M", na.rm = TRUE), sum(.data$sexe == "F", na.rm = TRUE))
    )

  # --- Fusion et ajout des CPUE ---
  table_finale <- bind_rows(
    table_tous, table_sexe, table_repro, table_inactifs, table_inconnu
  ) |>
    mutate(
      groupe = factor(.data$groupe, levels = c(
        "Tous", "Femelles", "Mâles", "Sexe inconnu",
        "Repro. actifs femelles", "Repro. actifs mâles",
        "Immatures ou reprod. inactifs", "Statut reprod. inconnu"
      ))
    ) |>
    arrange(.data$groupe) |>
    mutate(
      cpue = case_when(
        .data$groupe == "Tous" ~ cpue_tous,
        .data$groupe == "Repro. actifs femelles" ~ cpue_femelles,
        TRUE ~ NA_real_
      ),
      ic95 = case_when(
        .data$groupe == "Tous" ~ ic95_tous,
        .data$groupe == "Repro. actifs femelles" ~ ic95_femelles,
        TRUE ~ NA_character_
      )
    ) |> select(-"sexe")

  # --- Création flextable ---

  ft <- flextable(table_finale) |>
    set_caption("Tableau d'abondance") |>
    set_header_labels(
      groupe = "Groupe",
      abondance = "Nombre",
      proportion = "Proportion (%)",
      cpue = "CPUE",
      ic95 = "IC 95%",
      mf_ratio = "Ratio M:F"
    ) |>
    style_flextable_aquapop() |>
    hline(i = 3, border = fp_border(color = "black", width = 0.5))  |>
    colformat_double(j = "proportion", digits = 1, decimal.mark = ",", na_str = "-", big.mark = " ") |> 
    colformat_double(j = "cpue", digits = 2, decimal.mark = ",", na_str = "-", big.mark = " ")

  list(
    data = table_finale,
    flextable = ft
  )
}
