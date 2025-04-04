# ============================================================================
# Fichier : load_packages.R
# Rôle    : Vérifie la version de R, installe et charge les dépendances
# ============================================================================

# Vérification de la version minimale de R -----------------------------------
version_minimale_R <- "4.4.2"

if (getRversion() < version_minimale_R) {
  stop(paste0("❌ Version de R trop ancienne. ",
              "Veuillez utiliser R >= ", version_minimale_R, "."))
}

# Déclaration des packages requis --------------------------------------------
packages_requis <- c(
  # Interface utilisateur
  "shiny", "shinyBS", "shinycssloaders", "DT", "reactable", "reactlog",
  "htmltools", "markdown",
  
  # Manipulation de données
  "readxl", "writexl", "dplyr", "tidyr", "stringr", "purrr", "forcats", "lubridate", "glue", "labelled",
  
  # Graphiques
  "ggplot2", "scales", "patchwork", "gghighlight", "gt",
  
  # Exportation de résultats
  "flextable", "officer",
  
  # Analyses statistiques et modélisation
  "FSA", "fishmethods", "nlstools", "hnp", "glmmTMB", "MASS", "MuMIn",
  "pROC", "DescTools", "emdbook", "AICcmodavg", "investr", "car", "AER",
  
  # Pour les méthodes Monte Carlo
  "mvtnorm"   # pour rmvnorm()
)

# Installation automatique des packages manquants ----------------------------
packages_manquants <- setdiff(packages_requis, rownames(installed.packages()))

if (length(packages_manquants) > 0) {
  message("📦 Installation des packages manquants : ", paste(packages_manquants, collapse = ", "))
  install.packages(packages_manquants)
}

# Chargement des packages ----------------------------------------------------
invisible(lapply(packages_requis, library, character.only = TRUE))

# Vérification des versions minimales recommandées (facultatif) --------------
versions_recommandees <- list(
  "readxl"     = "1.4.2",
  "writexl"    = "1.4.2",
  "dplyr"      = "1.1.4",
  "tidyr"      = "1.3.0",
  "stringr"    = "1.5.1",
  "ggplot2"    = "3.5.0",
  "flextable"  = "0.9.3",
  "officer"    = "0.6.2"
)

for (pkg in names(versions_recommandees)) {
  installed_version <- as.character(packageVersion(pkg))
  required_version <- versions_recommandees[[pkg]]
  
  if (installed_version < required_version) {
    message(paste0("⚠️ ", pkg, " est à la version ", installed_version,
                   " (recommandé : ≥ ", required_version, ")."))
  }
}

message("\n==========================")
message("✅ Tous les packages sont prêts.")
message("==========================\n")