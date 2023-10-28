minitable_param_model_l_a50 <- function(model) {
  
a <- coef(model)[["(Intercept)"]]
b <- coef(model)[["age"]]
  
  #Delta_SE <- sqrt(
  #  deltavar(
  #    -a/b,
  #    meanval=c(a=a,
  #              b=b),                      
  #    Sigma=vcov(model , complete = TRUE)))
  #str(Delta_SE)

A50 <- (-a/b) %>% round(digits = 0)
  
#library(car)
LI <- deltaMethod(coef(model), "-(Intercept)/age", vcov.=vcov(model))$`2.5 %` %>% round(digits = 0)
LS <- deltaMethod(coef(model), "-(Intercept)/age", vcov.=vcov(model))$`97.5 %` %>% round(digits = 0)

#Delta_CI<-(A50+1.96*c(-1,1)*Delta_SE) 

#library(glue)
minitable  <- rbind(c("A~50~ (mm)",A50, glue::glue("[{LI }-{LS }]")),
                              c("a",round(a ,digits = 3), NA),
                              c("b",round(b ,digits = 3), NA)) %>% as.data.frame()

colnames(minitable)[1] <- "Paramètre"
colnames(minitable)[2] <- "Valeur"
colnames(minitable)[3] <- "IC 95%"

minitable
}

    

