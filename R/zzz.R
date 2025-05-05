# zzz.R — Initialisation générale pour le package aquapop
# Ce fichier est chargé en dernier lors de la construction du package.
#' @importFrom utils globalVariables 
# ------------------------------------------------------------------------------
# Déclaration des variables non visibles utilisées dans les pipes (dplyr, ggplot)
# Cela évite les fausses alertes lors du check : "no visible binding for..."
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "ltm", "masse", "sexe", "wr", "moyenne", "classe_brute",
  "classe", "intervalle", "fit", "lwr", "upr", "groupe", "ic95",
  "age", "methode", "t0_ic", "k_ic", "categorie", "sp", "number",
  "no_station", "gcat", "aicc", "modele_id", "convergence", "commentaire",
  "biomasse", "mf_ratio", "delta_aicc", "fill", "maturite", "lim_inf", "lim_sup",
  "h_pose", "date_leve", "h_leve", "no_lac", "typ_pech", "nom_lac",
  "comments", "annee", "nb_capture", "nb_pese", "marquage", "comments_specimen",
  "lat_dd.dec", "long_dd.dec", "prof_deb", "prof_fin", "min_pose", "heure_pose",
  "min_leve", "heure_leve", "date_pose", "leve", "pose", "count",
  "st_valide", "superficie_ha", "t0", "st_hasard", "l_inf_ic", "l_inf", "ltm_interval", "ajustement_hnp", "no_specimen",
  "k", "cpue_moyenne", "bpue", "percent","ic_95", "freq"
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
