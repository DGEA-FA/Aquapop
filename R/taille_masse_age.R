taille_masse_age <- function(dataspecimen, espece) {
  # longueur ----------------------------------------------------------------
  ltm_MF <-
    dataspecimen %>%  filter(sp == espece) %>%  filter(sexe == c("M", "F", "IND")) %>%
    dplyr::select(ltm, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    ))
  
  ltm_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = NA)
  
  ltm_Fmature <-
    dataspecimen %>%  filter(sp == espece, maturite == "O" , sexe == "F")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♀")
  
  ltm_Mmature <-
    dataspecimen %>%  filter(sp == espece &
                               maturite == "O" & sexe == "M")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♂")
  
  ltm_immature <-
    dataspecimen %>%  filter(sp == espece & maturite == "N")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Imm. ou reprod. inactifs")
  
  ltm_inconnu <-
    dataspecimen %>%  filter(sp == espece & is.na(maturite))  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Statut reprod. inconnu")
  
  
  completltm <-
    rbind(ltm_MF,
          ltm_tous,
          ltm_Mmature,
          ltm_immature,
          ltm_Fmature,
          ltm_inconnu)
  completltm$sexe <- as.character(completltm$sexe)
  completltm$sexe[is.na(completltm$sexe)] <- "Tous"
  completltm <-
    completltm %>% mutate(sexe = plyr::mapvalues(
      sexe,
      from = c("M", "F", "IND"),
      to = c("Mâle", "Femelle", "Sexe inconnu")
    ))
  completltm$sexe <- as.factor(completltm$sexe)
  
  completltm$sexe <-
    factor(
      completltm$sexe,
      levels = c(
        "Tous",
        "Femelle",
        "Mâle",
        "Sexe inconnu",
        "Reprod. actifs ♀",
        "Reprod. actifs ♂",
        "Imm. ou reprod. inactifs",
        "Statut reprod. inconnu"
      )
    )
  completltm <- completltm %>% arrange(sexe)
  completltm <-
    completltm %>% dplyr::select(c(sexe, ltm_N, ltm_Moyenne, ltm_ET, ltm_Minimum, ltm_Maximum))
  
  # masse -------------------------------------------------------------------
  masse_MF <-
    dataspecimen %>%  filter(sp == espece) %>%  filter(sexe == c("M", "F", "IND")) %>%
    dplyr::select(masse, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    ))
  
  masse_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(masse) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = NA)
  
  masse_Fmature <-
    dataspecimen %>%  filter(sp == espece, maturite == "O" , sexe == "F")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♀")
  
  masse_Mmature <-
    dataspecimen %>%  filter(sp == espece &
                               maturite == "O" & sexe == "M")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♂")
  
  masse_immature <-
    dataspecimen %>%  filter(sp == espece & maturite == "N")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Imm. ou reprod. inactifs")
  
  masse_inconnu <-
    dataspecimen %>%  filter(sp == espece & is.na(maturite))  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Statut reprod. inconnu")
  
  
  completmasse <-
    rbind(masse_MF,
          masse_tous,
          masse_Mmature,
          masse_immature,
          masse_Fmature,
          masse_inconnu)
  completmasse$sexe <- as.character(completmasse$sexe)
  completmasse$sexe[is.na(completmasse$sexe)] <- "Tous"
  completmasse <-
    completmasse %>% mutate(sexe = plyr::mapvalues(
      sexe,
      from = c("M", "F", "IND"),
      to = c("Mâle", "Femelle", "Sexe inconnu")
    ))
  completmasse$sexe <- as.factor(completmasse$sexe)
  
  completmasse$sexe <-
    factor(
      completmasse$sexe,
      levels = c(
        "Tous",
        "Femelle",
        "Mâle",
        "Sexe inconnu",
        "Reprod. actifs ♀",
        "Reprod. actifs ♂",
        "Imm. ou reprod. inactifs",
        "Statut reprod. inconnu"
      )
    )
  completmasse <- completmasse %>% arrange(sexe)
  completmasse <- completmasse %>% dplyr::select(c(
    sexe,
    masse_N,
    masse_Moyenne,
    masse_ET,
    masse_Minimum,
    masse_Maximum
  ))
  
  
  # age ---------------------------------------------------------------------
  age_MF <-
    dataspecimen %>%  filter(sp == espece) %>%  filter(sexe == c("M", "F", "IND")) %>%
    dplyr::select(age, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    ))
  
  age_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(age) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = NA)
  
  age_Fmature <-
    dataspecimen %>%  filter(sp == espece , maturite == "O" , sexe == "F")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♀")
  
  
  age_Mmature <-
    dataspecimen %>%  filter(sp == espece &
                               maturite == "O" & sexe == "M")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Reprod. actifs ♂")
  
  age_immature <-
    dataspecimen %>%  filter(sp == espece & maturite == "N")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Imm. ou reprod. inactifs")
  
  age_inconnu <-
    dataspecimen %>%  filter(sp == espece & is.na(maturite))  %>%
    dplyr::select(age) %>%
    dplyr::summarise(across(
      where(is.numeric),
      .fns = list(
        N =  ~ length(.x[!is.na(.x)]),
        Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
        ET = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
        Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
        Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
      )
    )) %>% mutate(sexe = "Statut reprod. inconnu")
  
  completage <-
    rbind(age_MF,
          age_tous,
          age_Mmature,
          age_immature,
          age_Fmature,
          age_inconnu)
  completage$sexe <- as.character(completage$sexe)
  completage$sexe[is.na(completage$sexe)] <- "Tous"
  completage <-
    completage %>% mutate(sexe = plyr::mapvalues(
      sexe,
      from = c("M", "F", "IND"),
      to = c("Mâle", "Femelle", "Sexe inconnu")
    ))
  completage$sexe <- as.factor(completage$sexe)
  
  completage$sexe <-
    factor(
      completage$sexe,
      levels = c(
        "Tous",
        "Femelle",
        "Mâle",
        "Sexe inconnu",
        "Reprod. actifs ♀",
        "Reprod. actifs ♂",
        "Imm. ou reprod. inactifs",
        "Statut reprod. inconnu"
      )
    )
  completage <- completage %>% arrange(sexe)
  completage <-
    completage %>% dplyr::select(c(sexe,
                                   age_N,
                                   age_Moyenne,
                                   age_ET,
                                   age_Minimum,
                                   age_Maximum))
  completage <- completage %>% dplyr::rename(Sexe = "sexe")
  
  complet <- cbind(completltm, completmasse, completage)
  complet <-
    complet %>% dplyr::select(
      c(
        Sexe,
        ltm_N,
        ltm_Moyenne,
        ltm_ET,
        ltm_Minimum,
        ltm_Maximum,
        masse_N,
        masse_Moyenne,
        masse_ET,
        masse_Minimum,
        masse_Maximum,
        age_N,
        age_Moyenne,
        age_ET,
        age_Minimum,
        age_Maximum
      )
    )
  
  complet <-
    complet %>% mutate(ltm_Minimum = plyr::mapvalues(
      ltm_Minimum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  complet <-
    complet %>% mutate(ltm_Maximum = plyr::mapvalues(
      ltm_Maximum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  
  complet <-
    complet %>% mutate(age_Minimum = plyr::mapvalues(
      age_Minimum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  complet <-
    complet %>% mutate(age_Maximum = plyr::mapvalues(
      age_Maximum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  
  complet <-
    complet %>% mutate(masse_Minimum = plyr::mapvalues(
      masse_Minimum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  complet <-
    complet %>% mutate(masse_Maximum = plyr::mapvalues(
      masse_Maximum,
      from = c("Inf", "-Inf"),
      to = c("-", "-")
    ))
  
  complet
}