tabRMS <- function(superficie_ha, profil) {

  data0.5m <- profil %>% dplyr::filter(prof<=5) %>% dplyr::select(prof, conductivite ) 
  data0.5m <- data0.5m %>% mutate(std = 0.666*conductivite)
  std <- mean(data0.5m$std) %>% round(digits = 2) 

  Frms <- 0.054 + 0.028 * log10(superficie_ha) - 0.063 * log10(std) + 0.038 * log10(superficie_ha) * log10(std)
  Mrms <- 0.22 * std^0.1061 / superficie_ha^0.06658
  Zrms <- Frms + Mrms
  
  
 # Resultats
  TabRMS <- data.frame(Paramètre = c("F~rms~", "M~rms~", "Z~rms~"),
                       Valeur = c(Frms, Mrms, Zrms))
  
  TabRMS[,2] <- round(TabRMS[,2], 3) 
  TabRMS
  
}


#methode <- "glm (Mainguy et Morales)"
#z <- abs(coef( m.df_EXT.p)[2])
#A <- 1 - exp(-z) 
#A <-A * 100
#A <- round(A, digits = 1)

#lowerZ <- abs(coef(m.df_EXT.p)[2] - confint(m.df_EXT.p)[2,1])

#highZ <- abs(coef(m.df_EXT.p)[2] + confint(m.df_EXT.p)[2,2])

#Alow <- 1 - exp(-lowerZ)
#Alow <- Alow * 100
#Alow <- round(Alow, digits = 1)

#Ahigh <- 1 - exp(- highZ)
#Ahigh <- Ahigh * 100
#Ahigh <- round(Ahigh, digits = 1)

#newdata <- data.frame(moyenne = c("Atotal"))
#resultmort <- cbind(newdata, A,  Alow,   Ahigh)

#resultmort <- resultmort %>% mutate(IC95 = glue("({  Alow}-{Ahigh})"))


    

