context("regsim")
library(stringr)
library(rmutil)
library(ggplot2)
library(gridExtra)
library(reshape2)


test_that("check output class",{
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1), "list")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t"), "list")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1), "list")
})

test_that("check for different beta values",{
    expect_silent(regsim(reps = 100, n = 100, beta = -1.2, eps.dist = "uniform", min = -1, max = 1))
    expect_silent(regsim(reps = 100, n = 100, beta = 1.2*100, eps.dist = "t"))
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1), "list")
})


test_that("check list element output class",{
    # check beta hat
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1)$beta_hat, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t")$beta_hat, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1)$beta_hat, "data.frame")

    # check r-sqaured
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1)$rsq, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t")$rsq, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1)$rsq, "data.frame")

    # check power
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1)$power, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t")$power, "data.frame")
    expect_is(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1)$power, "data.frame")
})

test_that("check outpute names",{
    # check beta hat
    expect_named(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1),
                 c("beta_hat", "power", "rsq"))
    expect_named(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t"),
              c("beta_hat", "power", "rsq"))

    expect_named(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t")$rsq,
                 c("sim", "norm"))
    expect_named(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "laplace", m = 0, s = 1)$beta_hat,
              c("sim", "norm"))
    expect_named(regsim(reps = 100, n = 100, beta = 1.2, eps.dist = "t")$power,
                 c("sim", "norm"))
})
