courbe_croissance_comparaison <- function(data) {
  # https://rdrr.io/cran/fishmethods/man/growthlrt.html  pas assez clair pour reproduire, besoin de plus d'infos
  #A la place je me suis inspiree de Mainguy pour definir les modeles, puis de Ogle (Chapitre 12) Dans Quist and Isermann (2017) pour comparer les modeles
  
  #calculs selon Julien Mainguy
  # library(FSA)
  #library(nlstools)
  
  
  init <- data
  pi <- vbStarts(ltm ~ age, data = init) #Les pi pour Linf, K et t0 peuvent être obtenus grâce à la fonction vbStarts() du package FSA.
  
  #chercher growth dans help si necessaire
  result <- fishmethods::growth(
    intype = 1,
    unit = 1,
    size = init$ltm,
    age = init$age,
    calctype = 1,
    wgtby = 1,
    error = 1,
    Sinf = pi$Linf,
    K = pi$K,
    t0 = pi$t0,
    graph = FALSE,
    control = list(
      maxiter = 10000,
      minFactor = 1 / 1024,
      tol = 1e-5
    )
  )
  

  tableresult <- data.frame(
    Methode = c("Von Bertalanffy",
                "Gompertz" ,
                "Logistique"),
    Linf = c(
      environment(result[["vout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
      environment(result[["gout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
      environment(result[["lout"]][["m"]][["deviance"]])[["env"]][["Sinf"]]
    ),
    K = c(
      environment(result[["vout"]][["m"]][["deviance"]])[["env"]][["K"]],
      environment(result[["gout"]][["m"]][["deviance"]])[["env"]][["K"]],
      environment(result[["lout"]][["m"]][["deviance"]])[["env"]][["K"]]
    ),
    t0 = c(
      environment(result[["vout"]][["m"]][["deviance"]])[["env"]][["t0"]],
      environment(result[["gout"]][["m"]][["deviance"]])[["env"]][["t0"]],
      environment(result[["lout"]][["m"]][["deviance"]])[["env"]][["t0"]]
    ),
    LCI_linf = c(
      stats::confint(result[["vout"]], level = 0.95)[1, 1],
      stats::confint(result[["gout"]], level = 0.95)[1, 1],
      stats::confint(result[["lout"]], level = 0.95)[1, 1]
    ),
    UCI_linf = c(
      stats::confint(result[["vout"]], level = 0.95)[1, 2],
      stats::confint(result[["gout"]], level = 0.95)[1, 2],
      stats::confint(result[["lout"]], level = 0.95)[1, 2]
    ),
    LCI_K = c(
      stats::confint(result[["vout"]], level = 0.95)[2, 1],
      stats::confint(result[["gout"]], level = 0.95)[2, 1],
      stats::confint(result[["lout"]], level = 0.95)[2, 1]
    ),
    UCI_K = c(
      stats::confint(result[["vout"]], level = 0.95)[2, 2],
      stats::confint(result[["gout"]], level = 0.95)[2, 2],
      stats::confint(result[["lout"]], level = 0.95)[2, 2]
    ),
    LCI_t0 = c(
      stats::confint(result[["vout"]], level = 0.95)[3, 1],
      stats::confint(result[["gout"]], level = 0.95)[3, 1],
      stats::confint(result[["lout"]], level = 0.95)[3, 1]
    ),
    UCI_t0 = c(
      stats::confint(result[["vout"]], level = 0.95)[3, 2],
      stats::confint(result[["gout"]], level = 0.95)[3, 2],
      stats::confint(result[["lout"]], level = 0.95)[3, 2]
    ),
    
    converged = c(result[["vout"]][["convInfo"]][["stopMessage"]],
                  result[["gout"]][["convInfo"]][["stopMessage"]],
                  result[["lout"]][["convInfo"]][["stopMessage"]])
  )
  

  #Pour obtenir les AIC
  resultAIC <- AICcmodavg::aictab(list(result[["vout"]],
                                       result[["gout"]],
                                       result[["lout"]]),
                                  c("Von Bertalanffy",
                                    "Gompertz" ,
                                    "Logistique")) %>% as.data.frame()
  
  #Mise en page
  resultAIC <- resultAIC %>% rename(Methode = Modnames)
  resultAIC <- resultAIC %>% dplyr::select(-c("K"))
  CLEAN  <- merge(tableresult, resultAIC, by = "Methode")
  
  CLEAN <- CLEAN %>% dplyr::select(-c("LL", "Cum.Wt", "ModelLik"))
  
  CLEAN[CLEAN == "converged"] <- "convergé"
  
  
  
  CLEAN$Linf <- round(CLEAN$Linf, digits = 0)
  CLEAN$K <- round(CLEAN$K, digits = 3)
  CLEAN$t0 <- round(CLEAN$t0, digits = 3)
  CLEAN$AICc <- round(CLEAN$AICc, digits = 2)
  CLEAN$Delta_AICc <- round(CLEAN$Delta_AICc, digits = 2)
  CLEAN$AICcWt <- round(CLEAN$AICcWt, digits = 2)
  
  CLEAN$UCI_linf <- round(CLEAN$UCI_linf, digits = 0)
  CLEAN$LCI_linf <- round(CLEAN$LCI_linf, digits = 0)
  CLEAN <-
    CLEAN %>% mutate(LinfIC = paste0("[", LCI_linf, "-", UCI_linf, "]"))
  CLEAN <- CLEAN %>% dplyr::select(-c("LCI_linf", "UCI_linf"))
  
  CLEAN$UCI_K <- round(CLEAN$UCI_K, digits = 3)
  CLEAN$LCI_K <- round(CLEAN$LCI_K, digits = 3)
  CLEAN <- CLEAN %>% mutate(KIC = paste0("[", LCI_K, "-", UCI_K, "]"))
  CLEAN <- CLEAN %>% dplyr::select(-c("LCI_K", "UCI_K"))
  
  CLEAN$UCI_t0 <- round(CLEAN$UCI_t0, digits = 3)
  CLEAN$LCI_t0 <- round(CLEAN$LCI_t0, digits = 3)
  CLEAN <- CLEAN %>% mutate(t0IC = paste0("[", LCI_t0, "-", UCI_t0, "]"))
  CLEAN <- CLEAN %>% dplyr::select(-c("LCI_t0", "UCI_t0"))
  
  
  
  CLEAN <-
    CLEAN %>% dplyr::select(
      c(
        "Methode",
        "Linf",
        "LinfIC",
        "K",
        "KIC",
        "t0",
        "t0IC",
        "AICc",
        "Delta_AICc",
        "AICcWt",
        "converged"
      )
    )
  CLEAN <- CLEAN %>% dplyr::arrange(AICc)
  
  colnames(CLEAN)[1] <- "Modèles"
  colnames(CLEAN)[2] <- "L∞"
  colnames(CLEAN)[3] <- "L∞ IC95%"
  colnames(CLEAN)[5] <- "K IC95%"
  colnames(CLEAN)[7] <- "t0 IC95%"
  colnames(CLEAN)[9] <- "Δ AICc"
  colnames(CLEAN)[10] <- "Poids d’Akaike"
  colnames(CLEAN)[11] <- "Convergence"
  
  CLEAN

 }

