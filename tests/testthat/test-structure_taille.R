specimen_test <- tibble::tibble(
  sp = rep("SANA", 10),
  ltm = c(300, 310, 320, 340, 350, 370, 380, 390, 400, 410),
  sexe = rep(c("F", "M"), each = 5),
  maturite = rep(c("O", "N"), times = 5),
  marquage = rep("NMA", 10)
)

test_that("structure_taille() fonctionne avec un jeu de données fictif structuré", {
  skip_if_not(exists("pen_constants"), "Le tableau `pen_constants` doit être défini.")
  
  # Données fictives conformes à la structure attendue
  data <- specimen_test
  
  expect_true(all(c("sp", "ltm", "sexe", "maturite", "marquage") %in% names(data)))
  expect_equal(unique(data$sp), "SANA")
  
  # Appel de la fonction
  res <- structure_taille(data, groupement = "sexe")
  
  expect_type(res, "list")
  expect_named(res, c("plot", "data", "flextable"))
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
  expect_s3_class(res$flextable, "flextable")
  
  expect_gt(nrow(res$data), 0)
})
