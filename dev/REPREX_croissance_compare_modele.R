
# REPREX — paramètres estimés mais IC non calculables
#
# Dans certains cas, les paramètres d’un modèle de croissance peuvent être estimés
# correctement, mais les intervalles de confiance ne peuvent pas être calculés en
# raison de limitations numériques de l’ajustement non linéaire (`nls`).
#
# Le REPREX ci-dessous illustre cette situation avec la fonction `growth()` du
# package fishmethods.

# Packages ------------------------------------------------------------------
library(FSA)
library(fishmethods)

# Fonctions utilitaires -----------------------------------------------------

# Extraire l’intervalle de confiance d’un paramètre
extract_param_ic <- function(res, index) {
  tryCatch(
    confint(res, level = 0.95)[index, , drop = TRUE],
    error = function(e) NA
  )
}

# Extraire la valeur estimée d’un paramètre
extract_coef <- function(mod, param_name = "Sinf") {
  tryCatch(
    coef(mod)[[param_name]],
    error = function(e) NA_real_
  )
}

# Données -------------------------------------------------------------------
#Correspond à :
# path     <- "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx"
# typ_pech <- "PENOF"
# no_lac   <- "05907"
# annee    <- 2014
df <- data.frame(
  ltm = c(
    157, 191, 247, 231, 259, 235, 232, 245, 254, 286, 180, 166, 239, 270,
    145, 130, 129, 280, 252, 249, 231, 229, 247, 255, 222, 184, 178, 219,
    188, 195, 181, 178, 180, 177, 172, 142, 282, 269, 240, 233, 235, 242,
    238, 236, 217, 248, 236, 140, 175, 278, 182, 249, 222, 208, 145, 158,
    235, 173, 172, 137, 243, 128, 167, 127, 139, 120, 255, 215, 181, 218,
    291, 164, 152, 157, 138, 149, 169, 145, 196, 154, 167, 148, 164, 144,
    175, 158, 145, 150, 182, 166, 190, 188, 164, 149, 162, 144, 162, 165,
    277, 307, 239, 186
  ),
  age = c(
    2, 2, 2, 3, 3, 2, 3, 4, 3, 4, 2, 2, 3, 4, 1, 1, 1, 3, 3, 4, 2, 3, 3, 3,
    4, 2, 2, 3, 2, 2, 2, 2, 2, 3, 3, 1, 4, 3, 3, 3, 2, 3, 3, 2, 2, 3, 3, 1,
    2, 3, 2, 3, 3, 2, 1, 1, 2, 2, 2, 1, 4, 1, 2, 1, 1, 1, 3, 2, 1, 2, 4, 2,
    1, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 3, 3, 2, 2, 2, 1, 1,
    1, 1, 3, 4, 2, 2
  ),
  no_specimen = factor(
    c(
      1, 10, 101, 102, 104, 105, 106, 107, 109, 11, 110, 111, 112, 113,
      114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
      128, 129, 13, 130, 131, 132, 133, 135, 136, 137, 138, 139, 14, 140,
      141, 142, 143, 145, 146, 147, 148, 149, 15, 150, 151, 152, 153, 154,
      155, 157, 158, 159, 16, 160, 161, 162, 163, 164, 17, 18, 19, 20,
      21, 22, 23, 25, 26, 27, 28, 29, 3, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 4, 40, 41, 42, 43, 44, 45, 46, 47,
      5, 6, 8, 9
    )
  )
)

# Valeurs initiales ---------------------------------------------------------
pi <- vbStarts(ltm ~ age, data = df)

# Ajustement des modèles ----------------------------------------------------
result <- growth(
  intype = 1,
  unit = 1,
  size = df$ltm,
  age = df$age,
  calctype = 1,
  wgtby = 1,
  error = 1,
  Sinf = pi$Linf,
  K = pi$K,
  t0 = pi$t0,
  graph = FALSE,
  control = list(
    maxiter = 10000,
    minFactor = 1 / 1024,
    tol = 1e-5
  )
)

tableresult <- data.frame(
  methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
  l_inf = c(
    extract_coef(result$vout, "Sinf"),
    extract_coef(result$gout, "Sinf"),
    extract_coef(result$lout, "Sinf")
  ),
  k = c(
    extract_coef(result$vout, "K"),
    extract_coef(result$gout, "K"),
    extract_coef(result$lout, "K")
  ),
  t0 = c(
    extract_coef(result$vout, "t0"),
    extract_coef(result$gout, "t0"),
    extract_coef(result$lout, "t0")
  ),
  l_inf_ic = mapply(function(res) {
    ic <- extract_param_ic(res, 1)
    if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
      paste0("[", round(ic[1]), "-", round(ic[2]), "]")
    } else {
      "IC non calculable"
    }
  }, list(result$vout, result$gout, result$lout)),
  k_ic = mapply(function(res) {
    ic <- extract_param_ic(res, 2)
    if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
      paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]")
    } else {
      "IC non calculable"
    }
  }, list(result$vout, result$gout, result$lout)),
  t0_ic = mapply(function(res) {
    ic <- extract_param_ic(res, 3)
    if (is.numeric(ic) && length(ic) == 2 && !any(is.na(ic))) {
      paste0("[", round(ic[1], 3), "-", round(ic[2], 3), "]")
    } else {
      "IC non calculable"
    }
  }, list(result$vout, result$gout, result$lout)),
  converged = c(
    result$vout$convInfo$stopMessage,
    result$gout$convInfo$stopMessage,
    result$lout$convInfo$stopMessage
  )
)

# Paramètres estimés --------------------------------------------------------
coef(result$vout)
coef(result$gout)
coef(result$lout)

# Vérification de la convergence -------------------------------------------
result$vout$convInfo$stopMessage
result$gout$convInfo$stopMessage
result$lout$convInfo$stopMessage

# Tentative de calcul des intervalles de confiance --------------------------
confint(result$vout)
confint(result$gout)
confint(result$lout)

# Extraction robuste des IC -------------------------------------------------
extract_param_ic(result$vout, 1)
extract_param_ic(result$gout, 1)
extract_param_ic(result$lout, 1)
