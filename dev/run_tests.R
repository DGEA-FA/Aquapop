# dev/run_tests.R

# Charger les fonctions du package en développement
devtools::load_all()

# Exécuter tous les tests unitaires
devtools::test()

fs::dir_tree(path = ".", depth = 2)

devtools::document()

# Copier le projet AquaPop vers un dossier local non synchronisé
source_dir <- "C:/Users/bruca03/OneDrive - Ministère de l'Environnement et la Lutte contre les changements climatiques/Documents/aquapop"
target_dir <- "C:/Users/bruca03/aqua_local/aquapop"

# Crée le dossier cible s'il n'existe pas
dir.create(dirname(target_dir), recursive = TRUE, showWarnings = FALSE)

# Copie tous les fichiers
fs::dir_copy(source_dir, target_dir, overwrite = TRUE)

cat("✅ Projet copié vers :", target_dir, "\n")
