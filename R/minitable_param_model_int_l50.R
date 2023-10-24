minitable_param_model_int_l50 <- function(model, df) {
  
a <- coef(model)[["(Intercept)"]]
b <- coef(model)[["ltm"]]
sexe <-coef(model)[["sexeM"]]
interaction <-coef(model)[["ltm:sexeM"]]

LINKK <- model$family$link

#L50
dfM <- df %>% dplyr::filter(sexe == "M") %>% droplevels()
modelM <- glm((as.numeric(maturite)-1)~ltm, data = dfM, family = binomial(link = LINKK))
l50arrayM <- as.array(dose.p(modelM, cf = 1:2, p = 0.5))
L50M <- format(round(as.numeric(l50arrayM ), 2), nsmall = 2) %>% as.numeric() #arrondir a 2 decimales  
sel50M <- format(round(as.numeric(attr(l50arrayM, "SE")), 2), nsmall = 2) %>% as.numeric()
Delta_CI_M <- (L50M + 1.96 * c(-1, 1) * sel50M)
LI_M <- round(Delta_CI_M[1], 0)
LS_M <- round(Delta_CI_M[2], 0) 
L50_M <- round(L50M, digits = 0)

dfF <- df %>% dplyr::filter(sexe == "F") %>% droplevels()
modelF <- glm((as.numeric(maturite)-1)~ltm, data = dfF , family = binomial(link = LINKK))
l50arrayF <- as.array(dose.p(modelF, cf = 1:2, p = 0.5))
L50F <- format(round(as.numeric(l50arrayF), 2), nsmall = 2) %>% as.numeric() #arrondir a 2 decimales  
sel50F <- format(round(as.numeric(attr(l50arrayF, "SE")), 2), nsmall = 2) %>% as.numeric()
Delta_CI_F <- (L50F + 1.96 * c(-1, 1) * sel50F)
LI_F <- round(Delta_CI_F[1], 0)
LS_F <- round(Delta_CI_F[2], 0) 
L50_F <- round(L50F, digits = 0)

library(glue)
minitable <- rbind(c("L~50~ Mâle (mm)", L50_M, glue("[{LI_M}-{LS_M}]")),
                   c("L~50~ Femelle (mm)", L50_F, glue("[{LI_F}-{LS_F}]")),
                   c("a",round(a, digits = 3), NA),
                   c("b",round(b, digits = 3), NA),
                   c("sexe", round(sexe, digits = 3), NA),
                   c("interaction", round(interaction, digits = 3), NA)) %>% as.data.frame()

colnames(minitable)[1] <- "Paramètre"
colnames(minitable)[2] <- "Valeur"
colnames(minitable)[3] <- "[Min-Max]"

minitable
}

