# dev/run_tests.R

# Charger les fonctions du package en développement
devtools::document()

devtools::load_all()

# Exécuter tous les tests unitaires
devtools::test()

fs::dir_tree(path = ".", depth = 2)


# usethis::use_package("tidyselect", type = "Imports")



