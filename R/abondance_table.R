abondance_table <- function(capture_data, specimen_data, species) {
  
  # Étape 1 : Filtrage des données de capture pour l'espèce ciblée
  capture_filtered <- capture_data %>%
    filter(sp == species, st_hasard == "O", st_valide %in% c("O", NA)) %>%
    select(no_station, nb_capture, nb_pese) %>%
    droplevels()
  
  # Filtrage des données de spécimen pour l'espèce ciblée
  specimen_filtered <- specimen_data %>%
    filter(sp == species, st_hasard == "O", st_valide %in% c("O", NA)) %>%
    droplevels()
  
  # Étape 2 : Fusionner les données de capture et de spécimen
  combined_data <- left_join(specimen_filtered, capture_filtered, by = "no_station")
  
  # Remplacer les valeurs manquantes dans la colonne "sexe" par "IND"
  combined_data <- combined_data %>%
    mutate(sexe = forcats::fct_explicit_na(sexe, na_level = "IND"))
  
  # Calcul du nombre total de spécimens après la fusion des données
  total_abundance <- nrow(combined_data)
  
  # Étape 3 : Calcul des différents groupes
  
  # Groupe "Tous"
  all_group <- combined_data %>%
    summarise(
      group = "Tous",
      abundance = total_abundance,
      proportion = round(abundance * 100 / total_abundance, 0),
      cpue = NA,
      ic95 = NA,
      mf_ratio = NA
    )
  
  # Groupes par sexe : Femelle, Mâle, Sexe inconnu
  sex_group <- combined_data %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      cpue = NA,
      ic95 = NA,
      mf_ratio = NA
    ) %>%
    mutate(group = recode_factor(sexe, "F" = "Femelle", "M" = "Mâle", "IND" = "Sexe inconnu")) %>%
    select(-sexe)
  
  # Groupes des reproducteurs matures : Repro. actifs femelles, Repro. actifs mâles
  mature_group <- combined_data %>%
    filter(maturite == "O" & sexe %in% c("M", "F")) %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      cpue = NA,
      ic95 = NA,
      mf_ratio = NA
    ) %>%
    mutate(group = recode_factor(sexe, "F" = "Repro. actifs femelles", "M" = "Repro. actifs mâles")) %>%
    select(-sexe)
  
  # Groupe des immatures ou reproducteurs inactifs
  immature_group <- combined_data %>%
    filter(maturite == "N") %>%
    summarise(
      group = "Immatures ou reprod. inactifs",
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      cpue = NA,
      ic95 = NA,
      mf_ratio = paste0(sum(sexe == "M"), ":", sum(sexe == "F"))
    )
  
  # Groupe des spécimens avec statut reproducteur inconnu
  unknown_repro_group <- combined_data %>%
    filter(is.na(maturite)) %>%
    summarise(
      group = "Statut reprod. inconnu",
      abundance = n(),
      proportion = round(abundance * 100 / total_abundance, 0),
      cpue = NA,
      ic95 = NA,
      mf_ratio = paste0(sum(sexe == "M"), ":", sum(sexe == "F"))
    )
  
  # Étape 4 : Combiner tous les groupes dans une seule table finale
  final_table <- bind_rows(all_group, sex_group, mature_group, immature_group, unknown_repro_group)
  
  # Reclasser les groupes pour assurer un ordre cohérent dans la table finale
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
  
  # Ajouter les labels aux colonnes pour une meilleure compréhension lors de l'affichage
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
