# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(MASS)  # Pour le modèle binomial négatif
library(MuMIn) # Pour comparer les modèles
set.seed(123)  # Pour la reproductibilité

# ---- Génération d'un jeu de données simulé ----
n_stations <- 30  # Nombre de stations
bpue_data <- data.frame(
  no_station = factor(1:n_stations),
  bpue = rgamma(n_stations, shape = 2, scale = 5) # Simulation de données continues
)

# ---- Ajustement des modèles ----
mod_poisson <- glm(bpue ~ 1, family = poisson, data = bpue_data)
mod_quasi_poisson <- glm(bpue ~ 1, family = quasipoisson, data = bpue_data)
mod_gamma <- glm(bpue ~ 1, family = Gamma(link = "log"), data = bpue_data)
mod_nb <- glm.nb(bpue ~ 1, data = bpue_data)

# ---- Comparaison des modèles ----
summary(mod_poisson) # Poisson (problème : bpue est continue)
summary(mod_quasi_poisson) # Quasi-Poisson (meilleure si variance > moyenne)
summary(mod_gamma) # Gamma (meilleure pour variables continues positives)
summary(mod_nb) # Binomial négatif (gère surdispersion)

# ---- Affichage des distributions des résidus ----
par(mfrow = c(2, 2))
hist(residuals(mod_poisson), main = "Poisson", col = "lightblue", breaks = 10)
hist(residuals(mod_quasi_poisson), main = "Quasi-Poisson", col = "lightcoral", breaks = 10)
hist(residuals(mod_gamma), main = "Gamma", col = "lightgreen", breaks = 10)
hist(residuals(mod_nb), main = "Binomial Négatif", col = "lightyellow", breaks = 10)

# ---- Visualisation de l'ajustement des modèles ----
pred_data <- data.frame(
  Model = c("Poisson", "Quasi-Poisson", "Gamma", "Binomial Négatif"),
  Prediction = c(
    exp(coef(mod_poisson)), 
    exp(coef(mod_quasi_poisson)), 
    exp(coef(mod_gamma)), 
    exp(coef(mod_nb))
  )
)

# ---- Calcul des prédictions et des IC95% ----
calc_ic95 <- function(model) {
  pred <- predict(model, newdata = data.frame(1), se.fit = TRUE, type = "link")
  bpue_est <- exp(pred$fit)
  ic_lower <- exp(pred$fit - 1.96 * pred$se.fit)
  ic_upper <- exp(pred$fit + 1.96 * pred$se.fit)
  
  return(data.frame(
    Model = deparse(substitute(model)),
    BPUE_Estimée = round(bpue_est, 2),
    IC95_Lower = round(ic_lower, 2),
    IC95_Upper = round(ic_upper, 2)
  ))
}

# Compilation des résultats
results <- bind_rows(
  calc_ic95(mod_poisson),
  calc_ic95(mod_quasi_poisson),
  calc_ic95(mod_gamma),
  calc_ic95(mod_nb)
)

# ---- Affichage des résultats ----
print(results)

# ---- Visualisation des IC95% ----
ggplot(results, aes(x = Model, y = BPUE_Estimée, ymin = IC95_Lower, ymax = IC95_Upper)) +
  geom_pointrange(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 3) +
  labs(title = "Comparaison des modèles : BPUE et IC95%", y = "BPUE estimée") +
  theme_minimal() +
  coord_flip()

