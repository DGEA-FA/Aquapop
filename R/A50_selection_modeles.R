A50_selection_modeles <- function(df, modlogit.l, modprobit.l, modcloglog.l, modlogit.add, modprobit.add, modcloglog.add, modlogit.int, modprobit.int, modcloglog.int) {

library(MuMIn)
library(pROC)
library(DescTools) #selon JM, on aime McFaddenAdj et McKelveyZavoina 
library(emdbook)
library(glue)

model.sel.TABLE  <-model.sel(modlogit.l, modprobit.l, modcloglog.l, modlogit.add, modprobit.add, modcloglog.add, modlogit.int, modprobit.int, modcloglog.int)
      
model.sel.TABLE  <- cbind(rownames(model.sel.TABLE), data.frame(model.sel.TABLE, row.names = NULL))
colnames(model.sel.TABLE)[1] <-  "model.sel"
      
colnames(model.sel.TABLE)[2] <- "(Intercept)"

model.sel.TABLE[grep("logit.l", model.sel.TABLE$model.sel),1 ] <- "logit.L"
model.sel.TABLE[grep("logit.add", model.sel.TABLE$model.sel),1 ] <- "logit.ADD"
model.sel.TABLE[grep("logit.int", model.sel.TABLE$model.sel),1 ] <- "logit.INT"

model.sel.TABLE[grep("probit.l", model.sel.TABLE$model.sel),1 ] <- "probit.L"
model.sel.TABLE[grep("probit.add", model.sel.TABLE$model.sel),1 ] <- "probit.ADD"
model.sel.TABLE[grep("probit.int", model.sel.TABLE$model.sel),1 ] <- "probit.INT"

model.sel.TABLE[grep("cloglog.l", model.sel.TABLE$model.sel),1 ] <- "cloglog.L"
model.sel.TABLE[grep("cloglog.add", model.sel.TABLE$model.sel),1 ] <- "cloglog.ADD"
model.sel.TABLE[grep("cloglog.int", model.sel.TABLE$model.sel),1 ] <- "cloglog.INT"

#model.sel.TABLE[grep(".INT", model.sel.TABLE$model.sel),1 ] <- "logit.INT"
#model.sel.TABLE[grep(".ADD", model.sel.TABLE$model.sel),1 ] <- "logit.ADD"
#model.sel.TABLE[grep(".L", model.sel.TABLE$model.sel),1 ] <- "logit.L"
      
#model.sel.TABLE <-  model.sel.TABLE %>% arrange(match(model.sel, c("logit.L", "probit.L", "cloglog.L", 
#                                                                   "logit.ADD", "probit.ADD", "cloglog.ADD", 
#                                                                   "logit.INT", "probit.INT", "cloglog.INT")))
    

#Mise en page des tableaux de sélections de modeles
model.sel.TABLE <- model.sel.TABLE %>% dplyr::select(model.sel, family, AICc, delta, weight)
      
model.sel.TABLE[model.sel.TABLE == "logit.L"] <- "Âge (lien logit)"
model.sel.TABLE[model.sel.TABLE == "logit.ADD"] <- "Âge + Sexe (lien logit)"
model.sel.TABLE[model.sel.TABLE == "logit.INT"] <- "Âge + Sexe + Âge:Sexe (lien logit)"

model.sel.TABLE[model.sel.TABLE == "probit.L"] <- "Âge (lien probit)"
model.sel.TABLE[model.sel.TABLE == "probit.ADD"] <- "Âge + Sexe (lien probit)"
model.sel.TABLE[model.sel.TABLE == "probit.INT"] <- "Âge + Sexe + Âge:Sexe (lien probit)"

model.sel.TABLE[model.sel.TABLE == "cloglog.L"] <- "Âge (lien cloglog)"
model.sel.TABLE[model.sel.TABLE == "cloglog.ADD"] <- "Âge + Sexe (lien cloglog)"
model.sel.TABLE[model.sel.TABLE == "cloglog.INT"] <- "Âge + Sexe + Âge:Sexe (lien cloglog)"
      
model.sel.TABLE$AICc <-model.sel.TABLE$AICc %>% round(digits = 2)
model.sel.TABLE$delta <-model.sel.TABLE$delta %>% round(digits = 2)
model.sel.TABLE$weight <-model.sel.TABLE$weight %>% round(digits = 2)
colnames(model.sel.TABLE)[1] <- "Modèles"
colnames(model.sel.TABLE)[2] <- "Lien"
colnames(model.sel.TABLE)[4] <- "Δ AICc"
colnames(model.sel.TABLE)[5] <- "Poids d’Akaike"

#BICmodelsel <- model.sel(modlogit.l ,modlogit.add,modlogit.int,rank=BIC)
#BICmodelsel <- cbind(rownames(BICmodelsel), data.frame(BICmodelsel, row.names=NULL))
#colnames(BICmodelsel)[1] <- "model.sel" 

#BICmodelsel[grep(".INT", BICmodelsel$model.sel),1 ] <- "logit.INT"
#BICmodelsel[grep(".ADD", BICmodelsel$model.sel),1 ] <- "logit.ADD"
#BICmodelsel[grep(".L", BICmodelsel$model.sel),1 ] <- "logit.L"

#BICmodelsel <-  BICmodelsel %>% arrange(match(model.sel, c("logit.L","logit.ADD", "logit.INT")))

#BICmodelsel <- BICmodelsel %>% dplyr::select(model.sel,BIC, delta, weight)
#BICmodelsel[BICmodelsel == "logit.L"] <- "Âge"
#BICmodelsel[BICmodelsel == "logit.ADD"] <- "Âge + Sexe"
#BICmodelsel[BICmodelsel == "logit.INT"] <- "Âge + Sexe + Âge:Sexe"
#BICmodelsel$BIC <-BICmodelsel$BIC %>% round(digits = 2)
#BICmodelsel$delta <-BICmodelsel$delta %>% round(digits = 2)
#BICmodelsel$weight <-BICmodelsel$weight %>% round(digits = 2)
#colnames(BICmodelsel)[1] <- "Modèles"
#colnames(BICmodelsel)[3] <- "Δ BIC"
#colnames(BICmodelsel)[4] <- "Poids"
      
model.sel.TABLE
}

    

