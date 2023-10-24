minitable_param_model_l_l50 <- function(model) {
  
a <- coef(model)[["(Intercept)"]]
b <- coef(model)[["ltm"]]
  
  #Delta_SE <- sqrt(
  #  deltavar(
  #    -a/b,
  #    meanval=c(a=a,
  #              b=b),                      
  #    Sigma=vcov(model , complete = TRUE)))
  #str(Delta_SE)

L50 <- (-a/b) %>% round(digits = 0)
  
#library(car)
LI <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov.=vcov(model))$`2.5 %` %>% round(digits = 0)
LS <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov.=vcov(model))$`97.5 %` %>% round(digits = 0)

#Delta_CI<-(L50+1.96*c(-1,1)*Delta_SE) 

#library(glue)
minitable  <- rbind(c("L~50~ (mm)",L50, glue::glue("[{LI }-{LS }]")),
                              c("a",round(a ,digits = 3), NA),
                              c("b",round(b ,digits = 3), NA)) %>% as.data.frame()

colnames(minitable)[1] <- "Paramètre"
colnames(minitable)[2] <- "Valeur"
colnames(minitable)[3] <- "[Min-Max]"

minitable
}

    

