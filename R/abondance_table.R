abondance_table <- function(specimen_data, espece) {
  
  # Étape 1 : Filtrage des spécimens pour l'espèce ciblée
  # On filtre les données de spécimens pour ne conserver que ceux correspondant à l'espèce ciblée.
  specimen_filtered <- specimen_data %>%
    filter(sp == espece) %>%
    droplevels()
  
  # Étape 2 : Calcul du nombre total de spécimens pour l'espèce ciblée
  # On calcule le nombre total de spécimens pour l'espèce sélectionnée.
  total_abundance <- nrow(specimen_filtered)
  
  # Étape 3 : Calcul des différents groupes d'abondance
  
  ## Groupe "Tous"
  # Ce groupe représente l'ensemble des spécimens filtrés.
  all_group <- specimen_filtered %>%
    summarise(
      group = "Tous",  # Nom du groupe
      abundance = total_abundance,  # Nombre total de spécimens
      proportion = round(abundance * 100 / total_abundance, 0),  # Proportion par rapport au total
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))  # Ratio Mâle/Femelle
    )
  
  ## Groupes par sexe : Femelle, Mâle, Sexe inconnu
  # On regroupe les spécimens par sexe et on calcule les statistiques pour chaque groupe.
  sex_group <- specimen_filtered %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),  # Nombre de spécimens dans chaque groupe de sexe
      proportion = round(abundance * 100 / total_abundance, 0),  # Proportion par rapport au total
      mf_ratio = NA  # Ratio M/F non applicable ici
    ) %>%
    # On transforme les valeurs de sexe en noms de groupe explicites.
    mutate(group = recode_factor(sexe, "F" = "Femelle", "M" = "Mâle", "IND" = "Sexe inconnu")) %>%
    select(-sexe)  # On supprime la colonne 'sexe' après recodage
  
  ## Groupes des reproducteurs matures : Repro. actifs femelles, Repro. actifs mâles
  # On filtre les spécimens matures et on les regroupe par sexe pour calculer les statistiques.
  mature_group <- specimen_filtered %>%
    filter(maturite == "O" & sexe %in% c("M", "F")) %>%
    group_by(sexe) %>%
    summarise(
      abundance = n(),  # Nombre de spécimens matures
      proportion = round(abundance * 100 / total_abundance, 0),  # Proportion par rapport au total
      mf_ratio = NA  # Ratio M/F non applicable ici
    ) %>%
    mutate(group = recode_factor(sexe, "F" = "Repro. actifs femelles", "M" = "Repro. actifs mâles")) %>%
    select(-sexe)  # On supprime la colonne 'sexe' après recodage
  
  ## Groupe des immatures ou reproducteurs inactifs
  # On calcule les statistiques pour les spécimens immatures ou inactifs.
  immature_group <- specimen_filtered %>%
    filter(maturite == "N") %>%
    summarise(
      group = "Immatures ou reprod. inactifs",  # Nom du groupe
      abundance = n(),  # Nombre de spécimens immatures ou inactifs
      proportion = round(abundance * 100 / total_abundance, 0),  # Proportion par rapport au total
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))  # Ratio Mâle/Femelle
    )
  
  ## Groupe des spécimens avec statut reproducteur inconnu
  # On calcule les statistiques pour les spécimens dont le statut reproducteur est inconnu.
  unknown_repro_group <- specimen_filtered %>%
    filter(maturite == "IND") %>%
    summarise(
      group = "Statut reprod. inconnu",  # Nom du groupe
      abundance = n(),  # Nombre de spécimens avec statut reproducteur inconnu
      proportion = round(abundance * 100 / total_abundance, 0),  # Proportion par rapport au total
      mf_ratio = calculate_mf_ratio(sum(sexe == "M"), sum(sexe == "F"))  # Ratio Mâle/Femelle
    )
  
  # Étape 4 : Combiner tous les groupes dans une table finale
  final_table <- bind_rows(all_group, sex_group, mature_group, immature_group, unknown_repro_group)
  
  # Étape 5 : Reclasser les groupes pour assurer un ordre cohérent dans la table finale
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
  
  # Étape 7 : Ajouter les labels aux colonnes pour une meilleure compréhension lors de l'affichage
  final_table <- final_table %>%
    labelled::set_variable_labels(
      group = "Groupe",
      abundance = "Nombre",
      proportion = "Proportion (%)",
      cpue = "CPUE",
      ic95 = "IC 95%",
      mf_ratio = "Ratio M:F"
    )
  
  # Retourner la table finale
  return(final_table)
}
