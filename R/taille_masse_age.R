taille_masse_age <- function(dataspecimen, espece) {
  
  # Filtrer les données pour l'espèce concernée
  dataspecimen_filtered <- dataspecimen %>% filter(sp == espece)
  
  # Fonction pour calculer les statistiques par groupe
  # - `data`: le dataframe filtré à utiliser
  # - `var`: le nom de la variable pour laquelle calculer les statistiques
  # - `group_var`: le nom de la variable de regroupement (par exemple, sexe)
  calculate_stats <- function(data, var, group_var = NULL) {
    if (!is.null(group_var)) {
      data <- data %>% group_by(!!sym(group_var), .drop = FALSE)
    }
    data %>%
      dplyr::summarise(
        nb = sum(!is.na(!!sym(var))),   # Nombre d'observations non manquantes
        moy = ifelse(all(is.na(!!sym(var))), NA, mean(!!sym(var), na.rm = TRUE)) %>% round(digits = 1),  # Moyenne
        e_t = ifelse(all(is.na(!!sym(var))), NA, sd(!!sym(var), na.rm = TRUE)) %>% round(digits = 1),    # Écart type
        min = ifelse(all(is.na(!!sym(var))), NA, min(!!sym(var), na.rm = TRUE)) %>% round(digits = 1),   # Minimum
        max = ifelse(all(is.na(!!sym(var))), NA, max(!!sym(var), na.rm = TRUE)) %>% round(digits = 1),   # Maximum
        .groups = "drop" # Éviter les messages d'avertissement liés au regroupement
      )
  }
  
  # Calcul des statistiques par longueur
  ltm_mf <- calculate_stats(dataspecimen_filtered, "ltm", group_var = "sexe")
  ltm_tous <- calculate_stats(dataspecimen_filtered, "ltm") %>% mutate(sexe = NA)
  ltm_fmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "F"), "ltm") %>% mutate(sexe = "Reprod. actifs ♀")
  ltm_mmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "M"), "ltm") %>% mutate(sexe = "Reprod. actifs ♂")
  ltm_immature <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "N"), "ltm") %>% mutate(sexe = "Imm. ou reprod. inactifs")
  ltm_inconnu <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "IND"), "ltm") %>% mutate(sexe = "Statut reprod. inconnu")
  
  # Combiner les résultats de longueur dans un dataframe unique
  ltm_df <- bind_rows(ltm_mf, ltm_tous, ltm_mmat, ltm_immature, ltm_fmat, ltm_inconnu) %>%
    mutate(sexe = as.character(sexe),
           sexe = ifelse(is.na(sexe), "Tous", sexe),  # Remplacer les valeurs manquantes par "Tous"
           sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),  # Traduction des codes
           sexe = factor(sexe, levels = c("Tous", "Femelle", "Mâle", "Sexe inconnu", "Reprod. actifs ♀", "Reprod. actifs ♂", "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))) %>%
    arrange(sexe)  # Trier par sexe
  
  # Calcul des statistiques par masse
  masse_mf <- calculate_stats(dataspecimen_filtered, "masse", group_var = "sexe")
  masse_tous <- calculate_stats(dataspecimen_filtered, "masse") %>% mutate(sexe = NA)
  masse_fmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "F"), "masse") %>% mutate(sexe = "Reprod. actifs ♀")
  masse_mmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "M"), "masse") %>% mutate(sexe = "Reprod. actifs ♂")
  masse_immature <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "N"), "masse") %>% mutate(sexe = "Imm. ou reprod. inactifs")
  masse_inconnu <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "IND"), "masse") %>% mutate(sexe = "Statut reprod. inconnu")
  
  # Combiner les résultats de masse dans un dataframe unique
  masse_df <- bind_rows(masse_mf, masse_tous, masse_mmat, masse_immature, masse_fmat, masse_inconnu) %>%
    mutate(sexe = as.character(sexe),
           sexe = ifelse(is.na(sexe), "Tous", sexe),
           sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),
           sexe = factor(sexe, levels = c("Tous", "Femelle", "Mâle", "Sexe inconnu", "Reprod. actifs ♀", "Reprod. actifs ♂", "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))) %>%
    arrange(sexe)
  
  # Calcul des statistiques par âge
  age_mf <- calculate_stats(dataspecimen_filtered, "age", group_var = "sexe")
  age_tous <- calculate_stats(dataspecimen_filtered, "age") %>% mutate(sexe = NA)
  age_fmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "F"), "age") %>% mutate(sexe = "Reprod. actifs ♀")
  age_mmat <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "O" & sexe == "M"), "age") %>% mutate(sexe = "Reprod. actifs ♂")
  age_immature <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "N"), "age") %>% mutate(sexe = "Imm. ou reprod. inactifs")
  age_inconnu <- calculate_stats(dataspecimen_filtered %>% filter(maturite == "IND"), "age") %>% mutate(sexe = "Statut reprod. inconnu")
  
  # Combiner les résultats d'âge dans un dataframe unique
  age_df <- bind_rows(age_mf, age_tous, age_mmat, age_immature, age_fmat, age_inconnu) %>%
    mutate(sexe = as.character(sexe),
           sexe = ifelse(is.na(sexe), "Tous", sexe),
           sexe = plyr::mapvalues(sexe, from = c("M", "F", "IND"), to = c("Mâle", "Femelle", "Sexe inconnu")),
           sexe = factor(sexe, levels = c("Tous", "Femelle", "Mâle", "Sexe inconnu", "Reprod. actifs ♀", "Reprod. actifs ♂", "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))) %>%
    arrange(sexe)
  
  # Renommer les colonnes de chaque dataframe avec le préfixe approprié
  ltm_df <- ltm_df %>%
    rename_with(~ paste0("ltm_", .), -sexe)
  
  masse_df <- masse_df %>%
    rename_with(~ paste0("masse_", .), -sexe)
  
  age_df <- age_df %>%
    rename_with(~ paste0("age_", .), -sexe)
  
  # Fusionner les dataframes sur la colonne `sexe`
  complet <- ltm_df %>%
    inner_join(masse_df, by = "sexe") %>%
    inner_join(age_df, by = "sexe") %>%
    rename(Sexe = sexe) %>%
    # Remplacement des valeurs infinies ou NA par "-"
    mutate(across(ends_with(c("min", "max", "moy", "e_t")),
                  ~ ifelse(. %in% c("Inf", "-Inf") | is.na(.), "-", .)))
  
  return(complet)
}
