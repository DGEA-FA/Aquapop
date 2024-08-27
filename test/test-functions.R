# /test/test-functions.R

library(testthat)
source("R/utils.R")

# Tester verifier_dataframes
test_that("verifier_dataframes détecte les dataframes vides", {
  empty_df <- data.frame()
  result <- verifier_dataframes(empty_df, "Test")
  expect_equal(result, "Test est vide.")
  
  non_empty_df <- data.frame(x = 1:3)
  result <- verifier_dataframes(non_empty_df, "Test")
  expect_equal(result, NULL)
})

# Tester verifier_doublons
test_that("verifier_doublons détecte les doublons", {
  df_with_doublons <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  result <- verifier_doublons(df_with_doublons, "Test")
  expect_equal(result, "Doublons trouvés dans Test")
  
  df_without_doublons <- data.frame(a = c(1, 2), b = c("x", "y"))
  result <- verifier_doublons(df_without_doublons, "Test")
  expect_equal(result, NULL)
})
