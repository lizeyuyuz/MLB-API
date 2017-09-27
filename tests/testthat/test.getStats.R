context("getStats")

test_that("check output class", {
    # batting
    expect_is(getStats(player = "Dylan Bundy",
                    team = "Baltimore Orioles",
                    table = "batting"), "data.frame")
})
test_that("check output class", {
    # pitching
    expect_is(getStats(player = "Alec Asher",
                             team = "Baltimore Orioles",
                             table = "pitching"), "data.frame")
})
test_that("check output class", {
    # fielding
    expect_is(getStats(player = "Darren O'Day",
                       team = "Baltimore Orioles",
                       table = "fielding"), "data.frame")
})

test_that("check player name is character", {
    # pitching
    expect_is(getStats(player = "Darren O'Day",
                       team = "Baltimore Orioles",
                       table = "pitching")$Name, "character")
    # batting
    expect_is(getStats(player = "Zach Britton",
                       team = "Baltimore Orioles",
                       table = "batting")$Name, "character")
})

test_that("check player statistics are numeric",{
    # fielding
    expect_true(
        sum(sapply(getStats(player = "Dylan Bundy",
                       team = "Baltimore Orioles",
                       table = "fielding")[2:(length(getStats(player = "Dylan Bundy",
                                                             team = "Baltimore Orioles",
                                                             table = "fielding")) - 1)],
               function(e) class(e) != "numeric")) == 0)
    # batting
    expect_true(
        sum(sapply(getStats(player = "Hunter Pence",
                       team = "San Francisco Giants",
                       table = "batting")[2:length(getStats(player = "Hunter Pence",
                                                             team = "San Francisco Giants",
                                                             table = "batting"))],
               function(e) class(e) != "numeric")) == 0)
    # pitching
    expect_true(
        sum(sapply(getStats(player = "Rick Porcello",
                       team = "Boston Red Sox",
                       table = "pitching")[2:length(getStats(player = "Rick Porcello",
                                                             team = "Boston Red Sox",
                                                             table = "pitching"))],
               function(e) class(e) != "numeric")) == 0)
})

test_that("check fielding statistics names",{
    expect_named(getStats(player = "Jay Bruce",
                          team = "New York Mets",
                          table = "fielding"), c("Player",     "Age",        "G",          "GS",         "CG",
                                                 "Inn",        "Ch",         "PO",         "A"        ,  "E",
                                                 "DP",         "Fld%",       "Rtot",       "Rtot/yr"  ,  "Rdrs",
                                                 "Rdrs/yr",    "RF/9",       "RF/G",       "PB",         "WP",
                                                 "SB",         "CS",         "CS%",        "lgCS%",      "Pickoffs",
                                                 "Pos Summary"))
})

test_that("check batting statistics names",{
    expect_named(getStats(player = "Aaron Hill",
                          team = "San Francisco Giants",
                          table = "batting"), c("Name","Pos", "Rk",  "Age", "G",   "PA",  "AB",  "R",   "H",    "2B",
                                                "3B",  "HR",  "RBI", "SB",  "CS",  "BB",  "SO",  "BA",  "OBP", "SLG",
                                                "OPS", "OPS+","TB",  "GDP", "HBP", "SH",  "SF",  "IBB" ))
})

test_that("check pitching statistics names",{
    expect_named(getStats(player = "Chih-Wei Hu",
                          team = "Tampa Bay Rays",
                          table = "pitching"), c( "Name", "Pos"  ,"Rk"   ,"Age" , "W"   , "L"   , "W-L%" ,"ERA" , "G"    ,"GS",
                                                   "GF" ,  "CG"  , "SHO" , "SV" ,  "IP" ,  "H"  ,  "R"   , "ER" ,  "HR"  , "BB",
                                                   "IBB",  "SO"  , "HBP" , "BK" ,  "WP" ,  "BF" ,  "ERA+", "FIP",  "WHIP", "H9",
                                                   "HR9",  "BB9" , "SO9" , "SO/W")
                 )
})

