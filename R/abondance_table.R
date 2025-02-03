abondance_table <- function(specimen_data, espece) {
  
  # Étape 1 : Filtrage des spécimens pour l'espèce ciblée
  specimen_filtered <- specimen_data %>%
    filter(sp == espece) %>%
    droplevels()
  
  # Étape 2 : Calcul du nombre total de spécimens pour l'espèce ciblée
  total_abundance <- nrow(specimen_filtered)
  
  # Étape 3 : Calcul des différents groupes d'abondance
  
  ## Groupe "Tous"
  all_group <- specimen_filtered %>%
    summarise(
      group = "Tous",
      abundance = total_abundance,
      proportion = round(abundance * 100 / total_abundance, 0),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  ## Groupes par sexe
  sex_group <- specimen_filtered %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      mf_ratio = NA
    ) %>%
    mutate(group = recode_factor(sexe, "F" = "Femelle", "M" = "Mâle", "IND" = "Sexe inconnu")) %>%
    select(-sexe) 
  
  ## Groupes des reproducteurs matures (⚠ Correction : inclure les "Repro. actifs mâles" même si `0`)
  mature_group <- specimen_filtered %>%
    filter(maturite == "O" & sexe %in% c("M", "F")) %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      mf_ratio = NA
    ) %>%
    mutate(group = recode_factor(sexe, "F" = "Repro. actifs femelles", "M" = "Repro. actifs mâles")) %>%
    select(-sexe) %>%
    tidyr::complete(group = c("Repro. actifs femelles", "Repro. actifs mâles"), fill = list(abundance = 0, proportion = 0)) # Ajout des valeurs manquantes
  
  ## Groupe des immatures ou reproducteurs inactifs
  immature_group <- specimen_filtered %>%
    filter(maturite == "N") %>%
    summarise(
      group = "Immatures ou reprod. inactifs",
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  ## Groupe des spécimens avec statut reproducteur inconnu
  unknown_repro_group <- specimen_filtered %>%
    filter(maturite == "IND") %>%
    summarise(
      group = "Statut reprod. inconnu",
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))
    )
  
  # Étape 4 : Combiner tous les groupes
  final_table <- bind_rows(all_group, sex_group, mature_group, immature_group, unknown_repro_group)
  
  # Étape 5 : Reclasser les groupes pour assurer un ordre cohérent
  final_table <- final_table %>%
    mutate(group = factor(group, levels = c(
      "Tous",
      "Femelle",
      "Mâle",
      "Sexe inconnu",
      "Repro. actifs femelles",
      "Repro. actifs mâles",
      "Immatures ou reprod. inactifs",
      "Statut reprod. inconnu"
    ))) %>%
    arrange(group)
  
  # Étape 6 : Ajouter des colonnes pour CPUE et IC95 (initialisées à NA)
  final_table <- final_table %>%
    mutate(cpue = NA, 
           ic95 = NA)
  
  # Étape 7 : Ajouter les labels aux colonnes
  final_table <- final_table %>%
    labelled::set_variable_labels(
      group = "Groupe",
      abundance = "Nombre",
      proportion = "Proportion (%)",
      cpue = "CPUE",
      ic95 = "IC 95%",
      mf_ratio = "Ratio M:F"
    )
  
  return(final_table)
}
