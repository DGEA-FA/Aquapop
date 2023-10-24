#TableWeightRef
t <- FSA::wsVal("Lake Trout") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
a <- FSA::wsVal("Brook Trout") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
b <- FSA::wsVal("Walleye") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
TableWeightRef <- rbind(t,a,b)

rm(list= c( "t", "a", "b"))
library(dplyr)
TableWeightRef <- TableWeightRef %>% mutate(sp= c("SANA", "SAFO", "SAVI"))
