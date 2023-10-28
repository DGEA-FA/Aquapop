minitable_param_model_int_a50 <- function(model, df) {
  
a <- coef(model)[["(Intercept)"]]
b <- coef(model)[["age"]]
sexe <-coef(model)[["sexeM"]]
interaction <-coef(model)[["age:sexeM"]]

LINKK <- model$family$link

#A50
dfM <- df %>% dplyr::filter(sexe == "M") %>% droplevels()
modelM <- glm((as.numeric(maturite)-1)~age, data = dfM, family = binomial(link = LINKK))
a50arrayM <- as.array(dose.p(modelM, cf = 1:2, p = 0.5))
A50M <- format(round(as.numeric(a50arrayM ), 2), nsmall = 2) %>% as.numeric() #arrondir a 2 decimales  
sea50M <- format(round(as.numeric(attr(a50arrayM, "SE")), 2), nsmall = 2) %>% as.numeric()
Delta_CI_M <- (A50M + 1.96 * c(-1, 1) * sea50M)
LI_M <- round(Delta_CI_M[1], 0)
LS_M <- round(Delta_CI_M[2], 0) 
A50_M <- round(A50M, digits = 0)

dfF <- df %>% dplyr::filter(sexe == "F") %>% droplevels()
modelF <- glm((as.numeric(maturite)-1)~age, data = dfF , family = binomial(link = LINKK))
a50arrayF <- as.array(dose.p(modelF, cf = 1:2, p = 0.5))
A50F <- format(round(as.numeric(a50arrayF), 2), nsmall = 2) %>% as.numeric() #arrondir a 2 decimales  
sea50F <- format(round(as.numeric(attr(a50arrayF, "SE")), 2), nsmall = 2) %>% as.numeric()
Delta_CI_F <- (A50F + 1.96 * c(-1, 1) * sea50F)
LI_F <- round(Delta_CI_F[1], 0)
LS_F <- round(Delta_CI_F[2], 0) 
A50_F <- round(A50F, digits = 0)

library(glue)
minitable <- rbind(c("A~50~ Mâle (mm)", A50_M, glue("[{LI_M}-{LS_M}]")),
                   c("A~50~ Femelle (mm)", A50_F, glue("[{LI_F}-{LS_F}]")),
                   c("a",round(a, digits = 3), NA),
                   c("b",round(b, digits = 3), NA),
                   c("sexe", round(sexe, digits = 3), NA),
                   c("interaction", round(interaction, digits = 3), NA)) %>% as.data.frame()

colnames(minitable)[1] <- "Paramètre"
colnames(minitable)[2] <- "Valeur"
colnames(minitable)[3] <- "IC 95%"

minitable
}

