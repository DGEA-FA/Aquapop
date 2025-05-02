#' Créer une table d’abondance structurée par groupe biologique
#'
#' Cette fonction calcule les effectifs et proportions de différents groupes (tous spécimens,
#' par sexe, par statut reproducteur) à partir des spécimens d’une espèce cible. Elle ajoute les
#' colonnes de CPUE et les intervalles de confiance (IC à 95 %) en extrayant les valeurs des meilleurs modèles sélectionnés.
#'
#' @param data Un `data.frame` de spécimens filtrés (issu de `load_specimen()`), pour un lac, une année, etc.
#' @param cpue_table_tous Un `data.frame` issu de `cpue_compare_modele(..., group = "tous", format = "data.frame")`.
#' @param cpue_table_femelles Idem, pour les femelles matures (`group = "femelles"`).
#' @param best_model_tous Nom du meilleur modèle pour les CPUE totales (ex. `"nb1"`).
#' @param best_model_femelles Nom du meilleur modèle pour les femelles matures (ex. `"nb2"`).
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` résumant les effectifs, proportions, CPUE et IC 95 % pour chaque groupe biologique.}
#'   \item{`flextable`}{Une version formatée (`flextable`) du tableau pour exportation dans Word, Shiny, etc.}
#' }
#'
#' @importFrom dplyr filter select mutate recode summarise count arrange bind_rows case_when n
#' @importFrom tidyr complete
#' @importFrom tibble tibble
#' @importFrom labelled set_variable_labels
#' @importFrom flextable set_caption flextable
#' 
#' @export
cpue_abondance_table <- function(data,
                            cpue_table_tous,
                            cpue_table_femelles,
                            best_model_tous,
                            best_model_femelles) {
  
  total <- nrow(data)
  
  # Extraire CPUE pour groupe "Tous"
  ligne_tous <- cpue_table_tous[cpue_table_tous$Méthode == best_model_tous, ]
  CPUE_tous <- ligne_tous$CPUE
  CPUEic_tous <- ligne_tous$`IC 95%`
  
  # Extraire CPUE pour groupe "Repro. actifs femelles"
  ligne_femelles <- cpue_table_femelles[cpue_table_femelles$Méthode == best_model_femelles, ]
  CPUE_Fmature <- ligne_femelles$CPUE
  CPUEic_Fmature <- ligne_femelles$`IC 95%`
  
  # Groupes de base
  tous <- tibble(
    group = "Tous",
    abundance = total,
    proportion = 100,
    mf_ratio = calculate_mf_ratio(sum(data$sexe == "M"), sum(data$sexe == "F"))
  )
  
  sexe_group <- data |>
    count(sexe, name = "abundance") |>
    mutate(
      group = recode(sexe,
                            "F" = "Femelle",
                            "M" = "Mâle",
                            "IND" = "Sexe inconnu"
      ),
      proportion = round(abundance / total * 100),
      mf_ratio = NA_character_
    ) |>
    select(group, abundance, proportion, mf_ratio)
  
  repro_group <- data |>
    filter(maturite == "O", sexe %in% c("M", "F")) |>
    count(sexe, name = "abundance") |>
    mutate(
      group = recode(sexe,
                            "F" = "Repro. actifs femelles",
                            "M" = "Repro. actifs mâles"
      ),
      proportion = round(abundance / total * 100),
      mf_ratio = NA_character_
    ) |>
    select(group, abundance, proportion, mf_ratio) |>
    complete(
      group = c("Repro. actifs femelles", "Repro. actifs mâles"),
      fill = list(abundance = 0, proportion = 0, mf_ratio = NA_character_)
    )
  
  inactif_group <- data |>
    filter(maturite == "N") |>
    summarise(
      group = "Immatures ou reprod. inactifs",
      abundance = n(),
      proportion = round(abundance / total * 100),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  inconnu_group <- data |>
    filter(maturite == "IND") |>
    summarise(
      group = "Statut reprod. inconnu",
      abundance = n(),
      proportion = round(abundance / total * 100),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  table <- bind_rows(
    tous, sexe_group, repro_group, inactif_group, inconnu_group
  ) |>
    mutate(
      group = factor(group, levels = c(
        "Tous", "Femelle", "Mâle", "Sexe inconnu",
        "Repro. actifs femelles", "Repro. actifs mâles",
        "Immatures ou reprod. inactifs", "Statut reprod. inconnu"
      ))
    ) |>
    arrange(group) |>
    mutate(
      cpue = case_when(
        group == "Tous" ~ CPUE_tous,
        group == "Repro. actifs femelles" ~ CPUE_Fmature,
        TRUE ~ NA_real_
      ),
      ic95 = case_when(
        group == "Tous" ~ CPUEic_tous,
        group == "Repro. actifs femelles" ~ CPUEic_Fmature,
        TRUE ~ NA_character_
      )
    )
  
  table <- set_variable_labels(
    table,
    group = "Groupe",
    abundance = "Nombre",
    proportion = "Proportion (%)",
    cpue = "CPUE",
    ic95 = "IC 95%",
    mf_ratio = "Ratio M:F"
  )
  
  ft <- flextable(table) |>
    set_caption("Tableau d'abondance") |>
    style_flextable_aquapop()
  
  return(list(
    data = table,
    flextable = ft
  ))
}
