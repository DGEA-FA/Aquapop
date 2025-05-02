# dev/run_tests.R

# Charger les fonctions du package en développement
devtools::load_all()

# Exécuter tous les tests unitaires
devtools::test()

fs::dir_tree(path = ".", depth = 2)
