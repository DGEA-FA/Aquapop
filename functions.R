# Définir les fonctions pour chaque modèle de croissance
vBert <- function(age, Linf, K, t0) { Linf * (1 - exp(-K * (age - t0)))}
Gompt <- function(age, Linf, K, t0) {Linf * exp(-exp(-K * (age - t0)))}
Logis <- function(age, Linf, K, t0) {Linf / (1 + exp(-K * (age - t0)))}
