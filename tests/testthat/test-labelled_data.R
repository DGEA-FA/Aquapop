test_that("labelled_data applique les labels seulement si tous sont valides", {
  df <- data.frame(
    age = c(25, 30),
    poids = c(60, 72)
  )
  labelled::var_label(df) <- list(age = "Âge", poids = "Poids (kg)")
  
  res <- labelled_data(df)
  expect_named(res, c("Âge", "Poids (kg)"))
})

test_that("labelled_data ne modifie pas les noms si un seul label est vide ou NULL", {
  df <- data.frame(
    age = c(25, 30),
    poids = c(60, 72),
    taille = c(160, 170)
  )
  labelled::var_label(df) <- list(age = "Âge", poids = "Poids (kg)", taille = "")
  
  res <- labelled_data(df)
  expect_named(res, c("age", "poids", "taille"))
})

test_that("labelled_data ne modifie pas les noms si un seul label est NA", {
  df <- data.frame(a = 1:2, b = 3:4)
  labelled::var_label(df) <- list(a = "Label A", b = NA)
  
  res <- labelled_data(df)
  expect_named(res, c("a", "b"))
})
