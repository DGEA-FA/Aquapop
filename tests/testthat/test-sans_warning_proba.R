test_that("sans_warning_proba ne filtre pas les autres warnings", {
  expect_warning(
    sans_warning_proba({
      warning("autre type de warning")
      42
    }),
    regexp = "autre type de warning"
  )
})
