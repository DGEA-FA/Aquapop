test_that("maturite_prepare retourne un data.frame filtré proprement", {
  df <- data.frame(
    ltm = c(150, 160, NA, 170),
    maturite = c("O", "N", "O", "IND"),
    sexe = c("F", "M", "IND", "F")
  )
  
  res <- maturite_prepare(df, variable = "ltm")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)  # les lignes avec IND ou NA sont exclues
  expect_equal(levels(res$maturite), c("N", "O"))
  expect_equal(levels(res$sexe), c("F", "M"))
  expect_s3_class(res$maturite, "ordered")
})

test_that("déclenche une erreur si colonne manquante", {
  df <- data.frame(sexe = c("F", "M"), maturite = c("N", "O"))
  expect_error(maturite_prepare(df, variable = "ltm"), "doit contenir les colonnes")
})

test_that("déclenche une erreur si specimen_data n'est pas un data.frame", {
  expect_error(maturite_prepare("pas un df"), "Must be of type 'data.frame'")
})

test_that("déclenche une erreur si variable invalide", {
  df <- data.frame(ltm = 1:10, sexe = "F", maturite = "O")
  expect_error(maturite_prepare(df, variable = "poids"), regexp = "ltm|age")
})
