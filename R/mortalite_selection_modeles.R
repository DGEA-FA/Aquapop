mortalite_selection_modeles <- function(df_EXT) {
  
  
  calcul.A.IClow <- function(model) {IClow.Z <- abs(confint(model )[2,2]) 
  IClow.A <- 1 - (exp(-IClow.Z)) 
  return(IClow.A)}
  
  calcul.A.ICup <- function(model) {ICup.Z <- abs(confint(model)[2,1]) 
  ICup.A <- 1 - (exp(-ICup.Z))  
  return(ICup.A)
  }
  
  # IClow.A <- calcul.A.IClow(model)
  # IClow.A <- calcul.A.ICup(model)
  
  
  
  
  # Poisson -----------------------------------------------------------------
  m.df_EXT.p <- glm(number ~ age, family = poisson, data = df_EXT)
  resume.p <- summary(m.df_EXT.p)
  SE.p <- resume.p[["coefficients"]][2, 2]
  #nous allons introduire ici un script provenant de l’Annexe 1 de Mainguy et Moral (2021)* permettant de réaliser 100 simulations hnp (au lieu d’une seule), tel que vu
  #aussi à la section 6.1, utilisant ensuite la moyenne obtenue pour le pourcentage de résidus de Pearson retrouvé en dehors de l’enveloppe simulée pour catégoriser l’ajustement du modèle (les résultats seront
  #enregistrés dans hnp_p). Comme nous souhaitons obtenir l’estimation de Z la moins biaisée possible,
  #nous allons cette fois-ci vérifier le modèle obtenu à partir des zéros qui ont été ajouté artificiellement
  
  set.seed(2021)
  hnp_p.p <- list()
  for (i in 1:2) {
    #remettre 100
    hnp_p.p[[i]] <- hnp::hnp(
      m.df_EXT.p,
      resid.type = "pearson",
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }
  summary_hnp.p <- sapply(hnp_p.p, function(x)
    x$out / x$total * 100)
  model_adequacy.p <- mean(summary_hnp.p)
  
  IClowA.p <- calcul.A.IClow(m.df_EXT.p)
  ICupA.p <- calcul.A.ICup(m.df_EXT.p)
  
  
  
  
  # nb1 ---------------------------------------------------------------------
  m.df_EXT.NB1 <-
    glmmTMB::glmmTMB(number ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_EXT)
  resume.NB1 <- summary(m.df_EXT.NB1)
  SE.NB1 <- resume.NB1[["coefficients"]]$cond[2, 2]
  
  dfun <- function(obj) {
    residuals(obj, type = "pearson")
  }
  sfun <- function(n, obj) {
    simulate(obj)[[1]]
  }
  
  ffun_nb1 <- function(response) {
    fit <- try(glmmTMB(response ~ age, family = nbinom1, data = df_EXT),
               silent = TRUE)
    while (class(fit) == "try-error") {
      response2 <- sfun(1, m.df_EXT.NB1)
      fit <-
        try(glmmTMB(response2 ~ age, family = nbinom1, data = df_EXT),
            silent = TRUE)
    }
    return(fit)
  }
  
  set.seed(2021)
  hnp_nb1 <- list()
  for (i in 1:2) {
    #remettre 100
    hnp_nb1[[i]] <- hnp::hnp(
      m.df_EXT.NB1,
      newclass = TRUE,
      diagfun = dfun,
      simfun = sfun,
      fitfun = ffun_nb1,
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }

  summary_hnp_NB1 <- sapply(hnp_nb1, function(x)
    x$out / x$total * 100)
  model_adequacy.NB1 <- mean(summary_hnp_NB1)
  
  
  IClowA.nb1 <- calcul.A.IClow(m.df_EXT.NB1)
  ICupA.nb1 <- calcul.A.ICup(m.df_EXT.NB1)
  
  
  # nb2 ---------------------------------------------------------------------
  m.df_EXT.NB2 <- glm.nb(number ~ age, data = df_EXT)
  resume.NB2 <- summary(m.df_EXT.NB2)
  SE.NB2 <- resume.NB2[["coefficients"]][2, 2]
  
  set.seed(2021)
  hnp_p.NB2 <- list()
  for (i in 1:2) {
    #remettre 100
    hnp_p.NB2[[i]] <- hnp::hnp(
      m.df_EXT.NB2,
      resid.type = "pearson",
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }
  summary_hnp.NB2 <-
    sapply(hnp_p.NB2, function(x)
      x$out / x$total * 100)
  model_adequacy.NB2 <- mean(summary_hnp.NB2)
  
  IClowA.nb2 <- calcul.A.IClow(m.df_EXT.NB2)
  ICupA.nb2 <- calcul.A.ICup(m.df_EXT.NB2)
  
  
  
  # CMPa --------------------------------------------------------------------
  m.df_EXT.CMPa <-
    glmmTMB::glmmTMB(number ~ age,
                     family = glmmTMB::compois(link = "log"),
                     data = df_EXT)
  resume.CMPa <- summary(m.df_EXT.CMPa)
  SE.CMPa <- resume.CMPa[["coefficients"]]$cond[2, 2]
  
  ffun_CMPa <- function(response) {
    fit <- try(glmmTMB(response ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_EXT),
               silent = TRUE)
    while (class(fit) == "try-error") {
      response2 <- sfun(1, m.df_EXT.CMPa)
      fit <-
        try(glmmTMB(response2 ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_EXT),
            silent = TRUE)
    }
    return(fit)
  }
  #
  set.seed(2021)
  hnp_CMPa <- list()
  for (i in 1:2) {
    #remettre 100 for(i in 1:100)
    hnp_CMPa[[i]] <- hnp::hnp(
      m.df_EXT.CMPa,
      newclass = TRUE,
      diagfun = dfun,
      simfun = sfun,
      fitfun = ffun_CMPa,
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }
  summary_hnp_CMPa <- sapply(hnp_CMPa, function(x)
    x$out / x$total * 100)
  model_adequacy.CMPa <- mean(summary_hnp_CMPa)
  
  
  IClowA.CMPa <- calcul.A.IClow(m.df_EXT.CMPa)
  ICupA.CMPa <- calcul.A.ICup(m.df_EXT.CMPa)
  
  # GP ----------------------------------------------------------------------
  m.df_EXT.GP <-
    glmmTMB::glmmTMB(number ~ age,
                     family = glmmTMB::genpois(link = "log"),
                     data = df_EXT)
  resume.GP <- summary(m.df_EXT.GP)
  SE.GP <- resume.GP[["coefficients"]]$cond[2, 2]
  
  ffun_GP <- function(response) {
    fit <- try(glmmTMB(response ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_EXT),
               silent = TRUE)
    while (class(fit) == "try-error") {
      response2 <- sfun(1, m.df_EXT.GP)
      fit <-
        try(glmmTMB(response2 ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_EXT),
            silent = TRUE)
    }
    return(fit)
  }
  
  set.seed(2021)
  hnp_GP <- list()
  for (i in 1:2) {
    #remettre 100
    hnp_GP[[i]] <- hnp::hnp(
      m.df_EXT.GP,
      newclass = TRUE,
      diagfun = dfun,
      simfun = sfun,
      fitfun = ffun_GP,
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }
  summary_hnp_GP <- sapply(hnp_GP, function(x)
    x$out / x$total * 100)
  model_adequacy.GP <- mean(summary_hnp_GP)
  
  IClowA.GP <- calcul.A.IClow(m.df_EXT.GP)
  ICupA.GP <- calcul.A.ICup(m.df_EXT.GP)
  

# CLEAN -------------------------------------------------------------------

  #La comparaison de modèles ayant recours à différentes extensions de la
  #distribution de Poisson permettra d’identifier laquelle parmi celles testées offre la meilleure capacité à
  #estimer non seulement Z, mais aussi et surtout la variance associée (SE).
  
  CLEAN <- MuMIn::model.sel(m.df_EXT.p,
                     m.df_EXT.NB1,
                     m.df_EXT.NB2,
                     m.df_EXT.CMPa,
                     m.df_EXT.GP) %>% as.data.frame()
  
  CLEAN <-
    CLEAN %>% dplyr::select(-c(
      "(Intercept)",
      "cond((Int))",
      "disp((Int))",
      "family",
      "class"
    ))
  CLEAN <- CLEAN %>% rename(condage = `cond(age)`)
  CLEAN <- CLEAN %>% mutate(Z = ifelse(is.na(age), condage, age))
  CLEAN <- CLEAN %>% dplyr::select(-c("age", "condage"))
  CLEAN <-
    cbind(GLM = rownames(CLEAN), CLEAN) #add the rownames as a proper column
  rownames(CLEAN) <- NULL # remove the original rownames
  CLEAN <-
    CLEAN %>% mutate(GLM = plyr::mapvalues(
      CLEAN$GLM,
      from = c(
        "m.df_EXT.NB2",
        "m.df_EXT.CMPa",
        "m.df_EXT.GP",
        "m.df_EXT.NB1",
        "m.df_EXT.p"
      ),
      to = c("NB2", "CMP", "GP", "NB1", "Poisson")
    ))
  
  CLEAN$GLM <- factor(CLEAN$GLM, levels = c("Poisson",
                                            "NB1", "NB2", "CMP", "GP"))
  CLEAN <- CLEAN %>% arrange(by = GLM)
  
  CLEAN2 <- cbind(
    CLEAN,
    SE = c(SE.p, SE.NB1, SE.NB2, SE.CMPa, SE.GP),
    
    lowerA = c(IClowA.p, IClowA.nb1, IClowA.nb2, IClowA.CMPa, IClowA.GP),
    
    Ahigh = c(ICupA.p, ICupA.nb1, ICupA.nb2, ICupA.CMPa, ICupA.GP),
    Modeladequacy = c(
      model_adequacy.p,
      model_adequacy.NB1,
      model_adequacy.NB2,
      model_adequacy.CMPa,
      model_adequacy.GP
    )
  )
  
  
  CLEAN2 <- CLEAN2 %>%
    mutate(commentaires = ifelse(Modeladequacy < 10, "Le modèle s'ajuste bien à vos données.",
                                   "Le modèle ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."))
  
  
  CLEAN2$delta <- round(CLEAN2$delta, digits = 2)
  CLEAN2$AICc <- round(CLEAN2$AICc, digits = 1)
  CLEAN2$weight <- round(CLEAN2$weight, digits = 3)
  CLEAN2$Z <- abs(CLEAN2$Z)
  CLEAN2$Z <- round(CLEAN2$Z, digits = 4)
  CLEAN2$SE <- round(CLEAN2$SE, digits = 4)
  CLEAN2$Modeladequacy <- round(CLEAN2$Modeladequacy, digits = 2)

  CLEAN2 <- CLEAN2 %>% dplyr::arrange(AICc)
  
  
  CLEAN2 <-
    CLEAN2 %>% mutate("A" = round((1 - exp(-Z)) * 100, digits = 1))
  CLEAN2 <- CLEAN2 %>% mutate("lowerZ" = Z - SE,
                              "highZ" = Z + SE)
  # CLEAN2 <-
  #   CLEAN2 %>% mutate("lowerA" = round((1 - exp(-lowerZ)) * 100, digits = 1),
  #                     "Ahigh" = round((1 - exp(-highZ)) * 100, digits = 1))
  
  CLEAN2 <-
    CLEAN2 %>% mutate("lowerA" = round(A - lowerA, digits = 1),
                      "Ahigh" = round(A + Ahigh, digits = 1))
  
  CLEAN2 <- CLEAN2 %>% mutate("IC 95%" = glue("[{lowerA}-{Ahigh}]"))
  CLEAN2 <-
    CLEAN2 %>% dplyr::select(-c("lowerA", "Ahigh", "lowerZ", "highZ"))
  
  
  CLEAN2 <-
    CLEAN2 %>% dplyr::select(GLM, Modeladequacy, df, AICc, delta, weight, Z, SE,A,"IC 95%",commentaires)
  
 
  
  CLEAN2 <-
    CLEAN2 %>% rename("Ajustement (résultat du test HNP)" = Modeladequacy)
  CLEAN2 <- CLEAN2 %>% rename("Poids d'Akaike" = weight)
  CLEAN2 <- CLEAN2 %>% rename("Commentaires" = commentaires)
  
  CLEAN2 <- CLEAN2 %>% rename("Δ AICc" = delta)
  CLEAN2 <- CLEAN2 %>% rename("Méthode" = GLM)
  
    CLEAN2

  

  
}
