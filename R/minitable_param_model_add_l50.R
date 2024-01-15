minitable_param_model_add_l50 <- function(model) {
  a <- coef(model)[["(Intercept)"]]
  b <- coef(model)[["ltm"]]
  sexe <- coef(model)[["sexeM"]]
  
  L50_F <- (-a / b) %>% round(digits = 0)
  
  L50_M <- ((-a - sexe) / b) %>% round(digits = 0)
  
  #library(car)
  LI_F <-
    deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
  LS_F <-
    deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`97.5 %`  %>% round(digits = 0)
  LI_M <-
    deltaMethod(coef(model), "-(Intercept+sexeM)/(ltm)", vcov. = vcov(model))$`2.5 %`  %>% round(digits = 0)
  LS_M <-
    deltaMethod(coef(model), "-(Intercept+sexeM)/(ltm)", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
  
  #library(glue)
  minitable <-
    rbind(
      c("L~50~ Mâle (mm)", L50_M, glue("[{LI_M }-{LS_M }]")),
      c("L~50~ Femelle (mm)", L50_F, glue("[{LI_F }-{LS_F }]")),
      c("a", round(a , digits = 3), NA),
      c("b", round(b , digits = 3), NA),
      c("sexe", round(sexe , digits = 3), NA)
    ) %>% as.data.frame()
  
  colnames(minitable)[1] <- "Paramètre"
  colnames(minitable)[2] <- "Valeur"
  colnames(minitable)[3] <- "IC 95%"
  
  minitable
}
