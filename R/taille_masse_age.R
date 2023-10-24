taille_masse_age <- function(dataspecimen, espece) {
  
  ltm_MF <- dataspecimen %>%  filter(sp==espece) %>%  filter(sexe==c("M", "F", "IND")) %>%
    dplyr::select(ltm, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("ltm_", "", .x, fixed = TRUE))

  ltm_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("ltm_", "", .x, fixed = TRUE)) %>% mutate(sexe=NA)

  ltm_Fmature <- dataspecimen %>%  filter(sp == espece, maturite=="O" , sexe=="F")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("ltm_", "", .x, fixed = TRUE)) %>% mutate(sexe ="♀ mature")



  ltm_Mmature <- dataspecimen %>%  filter(sp == espece & maturite=="O" & sexe=="M")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("ltm_", "", .x, fixed = TRUE)) %>% mutate(sexe="♂ mature")

  ltm_immature <- dataspecimen %>%  filter(sp == espece & maturite=="N")  %>%
    dplyr::select(ltm) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("ltm_", "", .x, fixed = TRUE)) %>% mutate(sexe="Immature")

   completltm <- rbind(ltm_MF, ltm_tous, ltm_Mmature, ltm_immature, ltm_Fmature )
   completltm$sexe <- as.character(completltm$sexe)
   completltm$sexe[is.na(completltm$sexe)] <- "Tous"
   completltm <- completltm %>% mutate(sexe=plyr::mapvalues(sexe, from=c("M","F", "IND"), to=c("Mâle","Femelle", "Sexe inconnu")))
   completltm$sexe <- as.factor(completltm$sexe)
   
   completltm$sexe <- factor(completltm$sexe, levels=c("Tous",  "Femelle","Mâle","♀ mature","♂ mature","Immature", "Sexe inconnu"))
   completltm <- completltm %>% arrange(sexe)
   completltm <- completltm %>% mutate(Morphologie= "LTmax (mm)")
   completltm <- completltm %>% dplyr::select(c("Morphologie", "sexe", "n", "Moyenne", "Écart-type", "Minimum", "Maximum"))
  
   
   
  #masse
  masse_MF <- dataspecimen %>%  filter(sp==espece) %>%  filter(sexe==c("M", "F", "IND")) %>%
    dplyr::select(masse, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("masse_", "", .x, fixed = TRUE))

  masse_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(masse) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("masse_", "", .x, fixed = TRUE)) %>% mutate(sexe = NA)

  masse_Fmature <- dataspecimen %>%  filter(sp == espece, maturite=="O" , sexe=="F")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("masse_", "", .x, fixed = TRUE)) %>% mutate(sexe ="♀ mature")



  masse_Mmature <- dataspecimen %>%  filter(sp == espece & maturite=="O" & sexe=="M")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("masse_", "", .x, fixed = TRUE)) %>% mutate(sexe="♂ mature")

  masse_immature <- dataspecimen %>%  filter(sp == espece & maturite=="N")  %>%
    dplyr::select(masse) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("masse_", "", .x, fixed = TRUE)) %>% mutate(sexe="Immature")

  completmasse <- rbind(masse_MF,masse_tous, masse_Mmature, masse_immature, masse_Fmature )
  completmasse$sexe <- as.character(completmasse$sexe)
  completmasse$sexe[is.na(completmasse$sexe)] <- "Tous"
  completmasse <- completmasse %>% mutate(sexe=plyr::mapvalues(sexe, from=c("M","F", "IND"), to=c("Mâle","Femelle", "Sexe inconnu")))
  completmasse$sexe <- as.factor(completmasse$sexe)

  completmasse$sexe <- factor(completmasse$sexe, levels=c("Tous",  "Femelle","Mâle","♀ mature","♂ mature","Immature", "Sexe inconnu"))
  completmasse <- completmasse %>% arrange(sexe)
  completmasse <- completmasse %>% mutate(Morphologie= "Masse (g)")
  completmasse <- completmasse %>% dplyr::select(c("Morphologie", "sexe", "n", "Moyenne", "Écart-type", "Minimum", "Maximum"))


  #age
  age_MF <- dataspecimen %>%  filter(sp==espece) %>%  filter(sexe==c("M", "F", "IND")) %>%
    dplyr::select(age, sexe) %>%
    group_by(sexe) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("age_", "", .x, fixed = TRUE))

  age_tous <- dataspecimen %>%  filter(sp == espece) %>%
    dplyr::select(age) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )
    ) %>%
    rename_with(~ gsub("age_", "", .x, fixed = TRUE)) %>% mutate(sexe=NA)

  age_Fmature <- dataspecimen %>%  filter(sp == espece , maturite=="O" , sexe=="F")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("age_", "", .x, fixed = TRUE)) %>% mutate(sexe ="♀ mature")



  age_Mmature <- dataspecimen %>%  filter(sp == espece & maturite=="O" & sexe=="M")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("age_", "", .x, fixed = TRUE)) %>% mutate(sexe="♂ mature")

  age_immature <- dataspecimen %>%  filter(sp == espece & maturite=="N")  %>%
    dplyr::select(age) %>%
    dplyr::summarise(
      across(
        where(is.numeric),
        .fns = list(
          n =  ~ n(),
          Moyenne = ~ mean(.x, na.rm = TRUE) %>% round(digits = 1),
          "Écart-type" = ~ sd(.x, na.rm = TRUE) %>% round(digits = 1),
          Minimum = ~ min(.x, na.rm = TRUE) %>% round(digits = 1),
          Maximum = ~ max(.x, na.rm = TRUE) %>% round(digits = 1)
        )
      )  ) %>%
    rename_with(~ gsub("age_", "", .x, fixed = TRUE)) %>% mutate(sexe="Immature")

  completage <- rbind(age_MF,age_tous, age_Mmature, age_immature, age_Fmature )
  completage$sexe <- as.character(completage$sexe)
  completage$sexe[is.na(completage$sexe)] <- "Tous"
  completage <- completage %>% mutate(sexe=plyr::mapvalues(sexe, from=c("M","F", "IND"), to=c("Mâle","Femelle", "Sexe inconnu")))
  completage$sexe <- as.factor(completage$sexe)

  completage$sexe <- factor(completage$sexe, levels=c("Tous",  "Femelle","Mâle","♀ mature","♂ mature","Immature", "Sexe inconnu"))
  completage <- completage %>% arrange(sexe)
  completage <- completage %>% mutate(Morphologie= "Âge")
  completage <- completage %>% dplyr::select(c("Morphologie", "sexe", "n", "Moyenne", "Écart-type", "Minimum", "Maximum"))


  complet <- rbind(completltm,completmasse, completage )
  complet <- complet %>% dplyr::rename("Sexe" = "sexe",
                                       "N" = "n")
  complet
}