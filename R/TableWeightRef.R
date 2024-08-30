#TableWeightRef
refSANA <- FSA::wsVal("Lake Trout", units = "metric", ref = 75) # valeurs de référence pour Lake Trout
refSAFO <- FSA::wsVal("Brook Trout", units = "metric", ref = 75) # valeurs de référence pour Brook Trout
refSAVI <- FSA::wsVal("Walleye", units = "metric", ref = 75) # valeurs de référence pour Walleye
TableWeightRef <- rbind(refSANA, refSAFO, refSAVI)

library(dplyr)
TableWeightRef <- TableWeightRef %>% mutate(sp = c("SANA", "SAFO", "SAVI"))
