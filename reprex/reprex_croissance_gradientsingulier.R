# tt <- paste(init$age, sep = ",")
# enumeration <- paste(tt, collapse = ", ")
# cat(enumeration)


# Create the dataframe 'init'

init <- data.frame(
  ltm = c(155, 175, 173, 212, 177, 150, 230, 184, 191, 172, 168, 158, 150, 171, 183, 185, 149, 164, 180, 167, 188, 205, 129, 128, 181, 172, 143, 195, 194, 175, 158, 177, 140, 193, 120, 160, 150, 178, 172, 158, 220, 220, 153, 160, 128, 194, 135, 130, 179, 153, 145, 155, 145, 171, 171, 193, 186, 204, 217, 173, 181, 122, 124, 156, 168, 185, 117, 129, 225, 211, 164, 202, 180, 160, 180, 172, 168, 129, 165, 178, 208, 200, 155, 157, 118, 181, 183, 200, 280, 189, 168, 155, 173, 169, 162, 164, 168, 197, 163, 168),
  age = c(2, 3, 3, 3, 3, 2, 5, 4, 3, 2, 2, 3, 1, 2, 2, 3, 1, 2, 3, 3, 2, 3, 1, 1, 3, 3, 1, 4, 3, 2, 2, 3, 2, 3, 1, 2, 1, 3, 3, 2, 4, 4, 2, 2, 1, 4, 1, 1, 3, 2, 2, 3, 3, 3, 2, 3, 2, 3, 4, 3, 2, 1, 1, 2, 2, 2, 1, 1, 3, 3, 3, 3, 3, 2, 2, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 2, 2, 3, 4, 3, 2, 1, 2, 2, 2, 2, 4, 3, 2, 2)
)


pi <- FSA::vbStarts(ltm ~ age, data = init) #Les pi pour Linf, K et t0 peuvent être obtenus grâce à la fonction vbStarts() du package FSA.

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

# Define a function to handle error cases
handle_error <- function(e) {
  return(conditionMessage(e))
}

# Creating the data frame with tryCatch
tableresult <- data.frame(
  Methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
  Linf = c(
    environment(result[["vout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
    environment(result[["gout"]][["m"]][["deviance"]])[["env"]][["Sinf"]],
    environment(result[["lout"]][["m"]][["deviance"]])[["env"]][["Sinf"]]
  ),
  K = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 1], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 1], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 1], error = handle_error)
  ),
  t0 = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 1], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 1], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 1], error = handle_error)
  ),
  LCI_linf = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[1, 1], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[1, 1], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[1, 1], error = handle_error)
  ),
  UCI_linf = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[1, 2], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[1, 2], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[1, 2], error = handle_error)
  ),
  LCI_K = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 1], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 1], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 1], error = handle_error)
  ),
  UCI_K = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[2, 2], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[2, 2], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[2, 2], error = handle_error)
  ),
  LCI_t0 = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 1], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 1], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 1], error = handle_error)
  ),
  UCI_t0 = c(
    tryCatch(stats::confint(result[["vout"]], level = 0.95)[3, 2], error = handle_error),
    tryCatch(stats::confint(result[["gout"]], level = 0.95)[3, 2], error = handle_error),
    tryCatch(stats::confint(result[["lout"]], level = 0.95)[3, 2], error = handle_error)
  ),
  converged = c(
    result[["vout"]][["convInfo"]][["stopMessage"]],
    result[["gout"]][["convInfo"]][["stopMessage"]],
    result[["lout"]][["convInfo"]][["stopMessage"]]
  )
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

if(is.numeric(CLEAN$K)) {
  CLEAN$K <- round(CLEAN$K, digits = 3)
}

if(is.numeric(CLEAN$t0)) {
  CLEAN$t0 <- round(CLEAN$t0, digits = 3)
}

CLEAN$AICc <- round(CLEAN$AICc, digits = 2)
CLEAN$Delta_AICc <- round(CLEAN$Delta_AICc, digits = 2)
CLEAN$AICcWt <- round(CLEAN$AICcWt, digits = 2)

if(is.numeric(CLEAN$UCI_linf)) {
  CLEAN$UCI_linf <- round(CLEAN$UCI_linf, digits = 0)
}


if(is.numeric(CLEAN$LCI_linf)) {
  CLEAN$LCI_linf <- round(CLEAN$LCI_linf, digits = 0)
}

if(is.numeric(CLEAN$LCI_linf) & is.numeric(CLEAN$UCI_linf)) {
  CLEAN <- CLEAN %>% mutate(LinfIC = paste0("[", LCI_linf, "-", UCI_linf, "]"))
} else {
  CLEAN <- CLEAN %>% mutate(LinfIC = "")
}


CLEAN <- CLEAN %>% dplyr::select(-c("LCI_linf", "UCI_linf"))

if(is.numeric(CLEAN$UCI_K)) {
  CLEAN$UCI_K <- round(CLEAN$UCI_K, digits = 3)
}

if(is.numeric(CLEAN$LCI_K)) {
  CLEAN$LCI_K <- round(CLEAN$LCI_K, digits = 3)
}


if(is.numeric(CLEAN$LCI_K) & is.numeric(CLEAN$UCI_K)) {
  CLEAN <- CLEAN %>% mutate(KIC = paste0("[", LCI_K, "-", UCI_K, "]"))
}else {
  CLEAN <- CLEAN %>% mutate(KIC = "")
}

CLEAN <- CLEAN %>% dplyr::select(-c("LCI_K", "UCI_K"))


if(is.numeric(CLEAN$UCI_t0)) {
  CLEAN$UCI_t0 <- round(CLEAN$UCI_t0, digits = 3)
}


if(is.numeric(CLEAN$LCI_t0)) {
  CLEAN$LCI_t0 <- round(CLEAN$LCI_t0, digits = 3)
}


if(is.numeric(CLEAN$LCI_t0) & is.numeric(CLEAN$UCI_t0)) {
  CLEAN <- CLEAN %>% mutate(t0IC = paste0("[", LCI_t0, "-", UCI_t0, "]"))
} else {
  CLEAN <- CLEAN %>% mutate(t0IC = "")
}

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
