library(testthat)
library(aquapop)

testthat::skip_if_not_installed("curl")
test_check("aquapop")
