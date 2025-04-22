#' Créer une table d’abondance structurée par groupe biologique
#'
#' Cette fonction calcule les effectifs et proportions de différents groupes (tous, par sexe,
#' par statut reproducteur) à partir des spécimens d’une espèce cible. Elle ajoute les
#' colonnes de CPUE et d’intervalle de confiance (IC 95%) en extrayant les données des meilleurs modèles.
#'
#' @param data Un `data.frame` de spécimens filtrés pour le lac, année, etc.
#' @param cpue_table_tous Un `data.frame` issu de `cpue_compare_modele(...)$data` (tous spécimens)
#' @param cpue_table_femelles Idem pour les femelles matures
#' @param best_model_tous Nom du meilleur modèle (ex: "nb1") pour tous
#' @param best_model_femelles Nom du meilleur modèle (ex: "nb2") pour femelles
#'
#' @return Une liste avec deux éléments : `data` (tableau brut) et `flextable` (tableau formaté).
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
  tous <- tibble::tibble(
    group = "Tous",
    abundance = total,
    proportion = 100,
    mf_ratio = calculate_mf_ratio(sum(data$sexe == "M"), sum(data$sexe == "F"))
  )
  
  sexe_group <- data %>%
    dplyr::count(sexe, name = "abundance") %>%
    dplyr::mutate(
      group = dplyr::recode(sexe,
                            "F" = "Femelle",
                            "M" = "Mâle",
                            "IND" = "Sexe inconnu"
      ),
      proportion = round(abundance / total * 100),
      mf_ratio = NA_character_
    ) %>%
    dplyr::select(group, abundance, proportion, mf_ratio)
  
  repro_group <- data %>%
    dplyr::filter(maturite == "O", sexe %in% c("M", "F")) %>%
    dplyr::count(sexe, name = "abundance") %>%
    dplyr::mutate(
      group = dplyr::recode(sexe,
                            "F" = "Repro. actifs femelles",
                            "M" = "Repro. actifs mâles"
      ),
      proportion = round(abundance / total * 100),
      mf_ratio = NA_character_
    ) %>%
    dplyr::select(group, abundance, proportion, mf_ratio) %>%
    tidyr::complete(
      group = c("Repro. actifs femelles", "Repro. actifs mâles"),
      fill = list(abundance = 0, proportion = 0, mf_ratio = NA_character_)
    )
  
  inactif_group <- data %>%
    dplyr::filter(maturite == "N") %>%
    dplyr::summarise(
      group = "Immatures ou reprod. inactifs",
      abundance = dplyr::n(),
      proportion = round(abundance / total * 100),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  inconnu_group <- data %>%
    dplyr::filter(maturite == "IND") %>%
    dplyr::summarise(
      group = "Statut reprod. inconnu",
      abundance = dplyr::n(),
      proportion = round(abundance / total * 100),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  table <- dplyr::bind_rows(
    tous, sexe_group, repro_group, inactif_group, inconnu_group
  ) %>%
    dplyr::mutate(
      group = factor(group, levels = c(
        "Tous", "Femelle", "Mâle", "Sexe inconnu",
        "Repro. actifs femelles", "Repro. actifs mâles",
        "Immatures ou reprod. inactifs", "Statut reprod. inconnu"
      ))
    ) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(
      cpue = dplyr::case_when(
        group == "Tous" ~ CPUE_tous,
        group == "Repro. actifs femelles" ~ CPUE_Fmature,
        TRUE ~ NA_real_
      ),
      ic95 = dplyr::case_when(
        group == "Tous" ~ CPUEic_tous,
        group == "Repro. actifs femelles" ~ CPUEic_Fmature,
        TRUE ~ NA_character_
      )
    )
  
  table <- labelled::set_variable_labels(
    table,
    group = "Groupe",
    abundance = "Nombre",
    proportion = "Proportion (%)",
    cpue = "CPUE",
    ic95 = "IC 95%",
    mf_ratio = "Ratio M:F"
  )
  
  ft <- flextable::flextable(table) |>
    flextable::set_caption("Tableau d'abondance") |>
    style_flextable_aquapop()
  
  return(list(
    data = table,
    flextable = ft
  ))
}
