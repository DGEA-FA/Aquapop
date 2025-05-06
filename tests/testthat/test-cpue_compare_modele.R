test_that("cpue_compare_modele() retourne une liste bien formée", {
  skip_if_not_installed("flextable")
  
  # Génère un jeu de données valide
  set.seed(123)
  df <- data.frame(no_station = 1:30, CPUE = rpois(30, lambda = 5))
  
  result <- suppressMessages(cpue_compare_modele(df))
  
  # Vérifie que le résultat est une liste avec deux éléments
  expect_type(result, "list")
  expect_named(result, c("data", "flextable"))
  
  # Vérifie la structure du tableau de données
  expect_s3_class(result$data, "data.frame")
  expect_true(nrow(result$data) >= 5)  # un par modèle
  expect_true(all(c(
    "Méthode",
    "Ajustement (résultat du test HNP)",
    "AICc",
    "Delta_AICc",
    "CPUE",
    "IC 95%",
    "Commentaires",
    "Convergence"
  ) %in% colnames(result$data)))
  
  # Vérifie le type flextable
  expect_s3_class(result$flextable, "flextable")
})
