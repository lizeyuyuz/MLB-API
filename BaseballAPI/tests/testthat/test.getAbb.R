context("getAbb")
library(stringr)
library(stringdist)
library(XML)

test_that("check that output is character",{
    expect_is(getAbb("New York White Sox"), "character")
    expect_is(getAbb("white sox"), "character")
})

test_that("check error messages",{
    expect_silent(getAbb("Boston Red Sox"))
    expect_silent(getAbb("torn"))
    expect_error(getAbb(c("Red Sox", "White Sox")))
})

test_that("check abbreivations are recognized", {
    expect_equal(getAbb("TBR"), "TBR")
    expect_equal(getAbb("bos"), "BOS")
    expect_equal(getAbb("mil"), "MIL")
})

test_that("check fuzzy search team names",{
    expect_equal(getAbb(team = "red soz"), "BOS")
    expect_equal(getAbb(team = "ny mets"), "NYM")
    expect_equal(getAbb(team = "ny yankes"), "NYY")
    expect_equal(getAbb(team = "tor jays"), "TOR")
    expect_equal(getAbb(team = "tampa bay"), "TBR")
    expect_equal(getAbb(team = "bay"), "TBR")
})
