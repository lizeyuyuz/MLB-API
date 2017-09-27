Sys.setenv("R_TESTS" = "") # https://github.com/hadley/testthat/issues/86
library(testthat)
library(BaseballAPI)

test_check("BaseballAPI")
