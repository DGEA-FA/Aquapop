biomasse_table <- function(specimen, sp_pen, data_station) {
  
  # Filtrage des données par l'espèce ciblée
  dataspec <- specimen %>%
    dplyr::filter(sp == sp_pen) %>%
    droplevels()
  
  # Calculer le nombre total de stations échantillonnées (incluant celles sans captures)
  nstation <- nrow(data_station) # Toutes les stations échantillonnées, y compris celles sans captures

  
  # Calculs pour le groupe "Tous" --------------------------------------------------
  # Agréger les données pour obtenir la biomasse totale par station
  temp <- alldata %>% dplyr::group_by(no_station) %>%
    summarise(bpue = sum(masse, na.rm = TRUE), Group = "Tous", .groups = "drop")
  
  # Calculer la biomasse totale pour toutes les stations en kg
  biomassefix <- sum(temp$bpue, na.rm = TRUE) %>% as.numeric() / 1000

  # Modèle poisson pour bpuelac
  mp <- glm(bpue ~ 1, family = poisson, data = temp)
  # The warnings you are seeing occur because the Poisson distribution is intended for modeling count data, 
  # which are typically non -negative integers. Your bpue values, however, are continuous non - integer numbers, 
  #which is why the warning is generated. The Poisson model may not be the most appropriate for your data if 
  #it is continuous and not count -  based. To address this, you have a few options:
  
  # 1. Use a Quasi-Poisson Model
  # The quasi-Poisson model can handle overdispersion and continuous data better than the regular Poisson model.
  # mp <- glm(bpue ~ 1, family = quasipoisson, data = temp)
  #Commentaire de Caro: aucun warning quand on utilise ca ! 
  # 2. Use a Negative Binomial Model
  # Since you are already using a negative binomial model (mnb2), it might be more suitable to focus on this model instead of the Poisson model, as it is more flexible and can handle the variance better for count and continuous data.
  
  # Modèle nb (negative binomial) pour bpuelac
  mnb2 <- MASS::glm.nb(bpue ~ 1, data = temp)
  
  # Compromis entre les deux modèles
  compromis <- model.avg(model.sel(mnb2, mp))
  
  # Prédictions basées sur le modèle compromis
  newdata <- data.frame(moyenne = c("moyenne"))
  predM <- predict(compromis, newdata, full = TRUE, se.fit = TRUE, type = "link")
  
  # Calcul de bpue final et des intervalles de confiance à 95%
  
  # Étape 1 : Calcul des valeurs non arrondies
  bpue_non_arrondi <- exp(predM$fit) / 1000
  lowerlimbpue_non_arrondi <- exp(predM$fit - (1.96 * predM$se.fit)) / 1000
  upperlimbpue_non_arrondi <- exp(predM$fit + (1.96 * predM$se.fit)) / 1000
  
  # Étape 2 : Arrondissement des valeurs
  bpue_arrondi <- round(bpue_non_arrondi, digits = 1)
  lowerlimbpue_arrondi <- round(lowerlimbpue_non_arrondi, digits = 1)
  upperlimbpue_arrondi <- round(upperlimbpue_non_arrondi, digits = 1)
  
  # Étape 3 : Formatage des valeurs arrondies
  bpuefinal <- format(bpue_arrondi, nsmall = 1)
  lowerlimbpue <- format(lowerlimbpue_arrondi, nsmall = 1)
  upperlimbpue <- format(upperlimbpue_arrondi, nsmall = 1)
  
  # Calculer le pourcentage de biomasse pour la catégorie "Tous"
  percent_tous <- round(biomassefix * 100 / biomassefix, digits = 0) # Ce sera toujours 100%
  
  
  # Créer le dataframe pour le groupe "Tous"
  Tous <- as.data.frame(c(
    biomasse =format(round(biomassefix, 1), nsmall = 1),
    percent = percent_tous,
    bpue = bpuefinal,
    ic95 = paste0("(", lowerlimbpue, "-", upperlimbpue, ")")
  ))
  colnames(Tous) <- "Tous"
  
  # Calculs pour les groupes par sexe (m_f_ind) --------------------------------------------------
  temp_sexe <- alldata %>% dplyr::group_by(no_station, sexe) %>%
    summarise(massesum = sum(masse, na.rm = TRUE), .groups = "drop")
  
  # Agréger les données pour chaque sexe et calculer la biomasse 
  temp_sexe <- temp_sexe %>% dplyr::group_by(sexe) %>%
    summarise(biomasse = sum(massesum, na.rm = TRUE) / 1000)
  
  # Calculer bpue pour chaque sexe
  temp_sexe <- temp_sexe %>% mutate(bpue = biomasse / nstation)
  
  # Calculer le pourcentage de biomasse par sexe
  temp_sexe <- temp_sexe %>% mutate(percent = biomasse * 100 / biomassefix,
                                    ic95 = NA)
  
  
  # Renommer les niveaux de sexe
  temp_sexe <- temp_sexe %>% mutate(sexe = plyr::mapvalues(
    sexe,
    from = c("F", "M", "IND"),
    to = c("Femelle", "Mâle", "Sexe inconnu")
  ))
  # Arrondir et formater les valeurs dans le tableau final
  temp_sexe$biomasse <- format(round(as.numeric(temp_sexe$biomasse), 1), nsmall = 1)
  temp_sexe$percent <- format(round(as.numeric(temp_sexe$percent), 0), nsmall = 0)
  temp_sexe$bpue <- format(round(as.numeric(temp_sexe$bpue), 1), nsmall = 1)
  
  
  # Sélectionner les colonnes d'intérêt et transposer les données
  m_f_ind <- temp_sexe %>% dplyr::select(c(sexe, biomasse, percent, bpue, ic95)) %>%
    t() %>% as.data.frame()
  
  # Ajuster les noms de colonnes
  colnames(m_f_ind) <- m_f_ind[1, ]
  m_f_ind <- m_f_ind[-1, ]
  
  
  # Calculs pour les mâles matures (m_mature) --------------------------------------------------
  temp_males <- alldata %>% filter(maturite == "O" & sexe == "M") %>% droplevels()
  
  # Agréger les données pour les mâles matures et calculer la biomasse et bpue
  temp_males <- temp_males %>% dplyr::group_by(no_station) %>%
    summarise(massesum = sum(masse, na.rm = TRUE), .groups = "drop")
  
  # Calculer la biomasse totale des mâles matures
  biomasse_males <- sum(temp_males$massesum, na.rm = TRUE) / 1000

  # Calculer bpue pour les mâles matures
  bpue_males <- round(biomasse_males / nstation, digits = 1)
  
  # Calculer le pourcentage de biomasse pour les mâles matures
  percent_males <- round(biomasse_males * 100 / biomassefix, digits = 0)
  
  # Créer le dataframe pour les mâles matures
  m_mature <- as.data.frame(c(
    biomasse = format(round(biomasse_males, 1), nsmall = 1),
    percent = percent_males,
    bpue = bpue_males,
    ic95 = NA  # Pas de calcul d'intervalle de confiance pour les mâles
  ))
  colnames(m_mature) <- "Repro. actifs mâles"
  
  # Calculs pour les femelles matures (f_mature) --------------------------------------------------
  temp_femelles <- alldata %>% filter(maturite == "O" & sexe == "F") %>% droplevels()
  
  # Agréger les données pour les femelles repro actives et calculer la biomasse et bpue
  temp_femelles <- temp_femelles %>% dplyr::group_by(no_station) %>%
    summarise(massesum = sum(masse, na.rm = TRUE), .groups = "drop")
  
  # Calculer la biomasse totale des femelles repro actives
  biomasse_fem <- sum(temp_femelles$massesum, na.rm = TRUE) / 1000

  # Modèle poisson pour les femelles repro actives
  mp_fem <- glm(massesum ~ 1, family = poisson, data = temp_femelles)
  
  # Modèle nb pour les femelles repro actives
  mnb2_fem <- MASS::glm.nb(massesum ~ 1, data = temp_femelles)
  
  # Compromis entre les deux modèles
  compromis_fem <- model.avg(model.sel(mnb2_fem, mp_fem))
  
  # Prédictions basées sur le modèle compromis
  newdata <- data.frame(moyenne = c("moyenne"))
  predM_fem <- predict(compromis_fem, newdata, full = TRUE, se.fit = TRUE, type = "link")
  
  # Calcul des intervalles de confiance à 95% pour les femelles repro actives
  
  # Étape 1 : Calcul des valeurs non arrondies
  bpue_fem_non_arrondi <- exp(predM_fem$fit) / 1000
  lowerlimbpue_fem_non_arrondi <- exp(predM_fem$fit - (1.96 * predM_fem$se.fit)) / 1000
  upperlimbpue_fem_non_arrondi <- exp(predM_fem$fit + (1.96 * predM_fem$se.fit)) / 1000
  
  # Étape 2 : Arrondissement des valeurs
  bpue_fem_arrondi <- round(bpue_fem_non_arrondi, digits = 1)
  lowerlimbpue_fem_arrondi <- round(lowerlimbpue_fem_non_arrondi, digits = 1)
  upperlimbpue_fem_arrondi <- round(upperlimbpue_fem_non_arrondi, digits = 1)
  
  # Étape 3 : Formatage des valeurs arrondies
  bpuefinal_fem <- format(bpue_fem_arrondi, nsmall = 1)
  lowerlimbpue_fem <- format(lowerlimbpue_fem_arrondi, nsmall = 1)
  upperlimbpue_fem <- format(upperlimbpue_fem_arrondi, nsmall = 1)
  
  # Calcul du pourcentage de biomasse pour les femelles repro actives
  percent_fem <- (biomasse_fem * 100 / biomassefix) %>% round(digits = 0)
  
  # Préparer les résultats bpue pour les femelles repro actives
  f_mature <- as.data.frame(c(
    biomasse = format(round(biomasse_fem, 1), nsmall = 1),
    percent = percent_fem,
    bpue = bpuefinal_fem,
    ic95 = paste0("(", lowerlimbpue_fem, "-", upperlimbpue_fem, ")")
  ))
  colnames(f_mature) <- "Repro. actifs femelles"

  
  
  # Calculs pour les poissons immatures ou reproducteurs inactifs (immature) --------------------------------------------------
  temp_immatures <- alldata %>% filter(maturite == "N") %>% droplevels()
  
  # Agréger les données pour les poissons immatures ou reproducteurs inactifs et calculer la biomasse
  temp_immatures <- temp_immatures %>% dplyr::group_by(no_station) %>%
    summarise(massesum = sum(masse, na.rm = TRUE), .groups = "drop")
  
  # Calculer la biomasse totale des poissons immatures ou reproducteurs inactifs
  biomasse_immatures <- sum(temp_immatures$massesum, na.rm = TRUE) / 1000

  # Calculer bpue pour les poissons immatures ou reproducteurs inactifs
  bpue_immatures <- round(biomasse_immatures / nstation, digits = 1)
  
  # Calculer le pourcentage de biomasse pour les poissons immatures ou reproducteurs inactifs
  percent_immatures <- round(biomasse_immatures * 100 / biomassefix, digits = 0)
  
  # Créer le dataframe pour les poissons immatures ou reproducteurs inactifs
  immature <- as.data.frame(c(
    biomasse = format(round(biomasse_immatures, 1), nsmall = 1),
    percent = percent_immatures,
    bpue = bpue_immatures,
    ic95 = NA  # Pas de calcul d'intervalle de confiance pour cette catégorie
  ))
  colnames(immature) <- "Imm. ou reprod. inactifs"
  
  
  
  

  # Calculs pour les poissons avec statut reproducteur inconnu (inconnu) --------------------------------------------------
  temp_inconnu <- alldata %>% filter(maturite =="IND") %>% droplevels()
  
  # Agréger les données pour les poissons avec statut reproducteur inconnu et calculer la biomasse
  temp_inconnu <- temp_inconnu %>% dplyr::group_by(no_station) %>%
    summarise(massesum = sum(masse, na.rm = TRUE), .groups = "drop")
  
  # Calculer la biomasse totale des poissons avec statut reproducteur inconnu
  biomasse_inconnu <- sum(temp_inconnu$massesum, na.rm = TRUE) / 1000

  # Calculer bpue pour les poissons avec statut reproducteur inconnu
  bpue_inconnu <- round(biomasse_inconnu / nstation, digits = 1)
  
  # Calculer le pourcentage de biomasse pour les poissons avec statut reproducteur inconnu
  percent_inconnu <- round(biomasse_inconnu * 100 / biomassefix, digits = 0)
  
  # Créer le dataframe pour les poissons avec statut reproducteur inconnu
  inconnu <- as.data.frame(c(
    biomasse = format(round(biomasse_inconnu, 1), nsmall = 1),
    percent = percent_inconnu,
    bpue = bpue_inconnu,
    ic95 = NA  # Pas de calcul d'intervalle de confiance pour cette catégorie
  ))
  colnames(inconnu) <- "Statut reprod. inconnu"
  
  
  # Combiner toutes les catégories dans une table finale final_table --------------------------------------------------
  final_table <- cbind(Tous, m_f_ind, m_mature, f_mature, immature, inconnu) %>%
    dplyr::select(
      c(
        "Tous",
        "Femelle",
        "Mâle",
        "Sexe inconnu",
        "Repro. actifs femelles",
        "Repro. actifs mâles",
        "Imm. ou reprod. inactifs",
        "Statut reprod. inconnu"
      )
    ) %>%
    t() %>% as.data.frame()
  
  # Ajouter une colonne pour le groupe correspondant et réorganiser les colonnes
  final_table <- final_table %>% mutate(
    groupe = c(
      "Tous",
      "Femelle",
      "Mâle",
      "Sexe inconnu",
      "Repro. actifs femelles",
      "Repro. actifs mâles",
      "Imm. ou reprod. inactifs",
      "Statut reprod. inconnu"
    )
  ) %>%
    dplyr::select(c(groupe, everything()))
  
  # Supprimer les rownames
  rownames(final_table) <- NULL
  
  
  # Convertir les colonnes numériques
  final_table$biomasse <- as.numeric(final_table$biomasse)
  final_table$percent <- as.numeric(final_table$percent)
  final_table$bpue <- as.numeric(final_table$bpue)
  
  # Formater les colonnes numériques avec 1 ou 2 décimales
  final_table$biomasse <- format(round(final_table$biomasse, 1), nsmall = 1)
  final_table$percent <- format(round(final_table$percent, 0), nsmall = 0)
  final_table$bpue <- format(round(final_table$bpue, 1), nsmall = 1)
  
  # Gérer les valeurs NA dans la colonne ic95
  final_table$ic95 <- ifelse(is.na(final_table$ic95), "", final_table$ic95)
  
  # Appliquer des labels aux colonnes de final_table
  final_table <- final_table %>%
    labelled::set_variable_labels(
      groupe = "Groupe",
      biomasse = "Biomasse totale (kg)",
      percent = "Proportion (%)",
      bpue = "BPUE (kg/station)",
      ic95 = "IC 95%"
    )

  
  return(final_table)
}
