context("getPlayers")
library(testthat)
library(BaseballAPI)

test_that("check output class",{
    expect_is(class(getPlayers(team = "Baltimore Orioles")), "character")
    expect_is(class(getPlayers(team = "DET")), "character")
    expect_is(class(getPlayers(team = "red sox")), "character")
})

test_that("check output dimension",{
    expect_true(!is.null(length(getPlayers("San Francisco Giants"))))
    expect_true(!is.null(length(getPlayers("Seattle Mariners"))))
    expect_true(is.null(dim(getPlayers("Pittsburgh Pirates"))))
    expect_true(is.null(dim(getPlayers("Minnesota Twins"))))
})
