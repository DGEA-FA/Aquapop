# zzz.R — Initialisation générale pour le package aquapop
# Ce fichier est chargé en dernier lors de la construction du package.
#' @importFrom utils globalVariables 
# ------------------------------------------------------------------------------
# Déclaration des variables non visibles utilisées dans les pipes (dplyr, ggplot)
# Cela évite les fausses alertes lors du check : "no visible binding for..."
# ------------------------------------------------------------------------------

globalVariables(c(
  "ltm", "masse", "sexe", "wr", "moyenne", "classe_brute",
  "classe", "intervalle", "fit", "lwr", "upr", "groupe", "ic95",
  "age", "methode","t0_ic","k_ic", "categorie", 'sp', "number",
  "no_station", "gcat", "aicc", "modele_id", "convergence", "commentaire",
  "maturite", "lim_inf", "lim_sup", "h_pose", "date_leve", "h_leve"
))

# ------------------------------------------------------------------------------
# Optionnel : code exécuté automatiquement quand le package est chargé
# Ici, rien n'est exécuté, mais tu peux ajouter du code dans .onLoad() ou .onAttach()
# ------------------------------------------------------------------------------

# .onLoad <- function(libname, pkgname) {
#   # Par exemple : définir une option par défaut
#   options(aquapop.verbose = TRUE)
# }

# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("aquapop chargé. Utilisez help(package = 'aquapop') pour en savoir plus.")
# }
