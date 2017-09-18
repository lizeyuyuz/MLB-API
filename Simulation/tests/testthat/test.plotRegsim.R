context("plotRegsim")
library(stringr)
library(rmutil)
library(ggplot2)
library(gridExtra)
library(reshape2)

test_that("check error message",{
    # r2 plots
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 500),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "laplace",
                             m = 0,
                             s= 1,
                             which.plot = "r2"))
    expect_silent(plotRegsim(reps = 100,
                             n = 50,
                             beta = c(1, 1.5),
                             eps.dist = "t",
                             which.plot = "r2"))
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 500),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "uniform",
                             min = -1,
                             max = 1,
                             which.plot = "r2"))
    # power plots
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 10, 200),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "t",
                             which.plot = "power"))
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 300),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "uniform",
                             min = -1,
                             max = 1,
                             which.plot = "power"))
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 200),
                             beta = 4,
                             eps.dist = "laplace",
                             m = 0,
                             s= 1,
                             which.plot = "power"))
    # beta hat plots
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 500),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "laplace",
                             m = 0,
                             s= 1,
                             which.plot = "beta hat"))
    expect_silent(plotRegsim(reps = 500,
                             n = c(50, 75),
                             beta = c(1, 1.5),
                             eps.dist = "t",
                             which.plot = "beta hat"))
    expect_silent(plotRegsim(reps = 100,
                             n = c(50, 100, 500),
                             beta = c(.4, .8, 1, 1.5),
                             eps.dist = "uniform",
                             min = -1,
                             max = 1,
                             which.plot = "beta hat"))
})

