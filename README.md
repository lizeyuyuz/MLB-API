# Description of MLBscrapeR package

This package scrapes 2017 statistics of active MLB baseball teams and of their players from "http://www.baseball-reference.com/". The package provides two functions: the `getPlayers` function scrapes a list of players on a given baseball team; the `getStats` function scrapes an individual player's statistics on a given baseball team in a specified area. The package contains a lookup table of active baseball teams' names.

# Downloading and installing the R package

``` r
# install.packages('devtools')
library(devtools)

devtools::install_github(repo = "lizeyuyuz/MLBscrapeR")

library(MLBscrapeR)
```

# Examples use of functions 

``` r
# to get list of players on an active team
getPlayers(team = "Baltimore Orioles")

# to get fielding statistics of a specific player
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "fielding")

# to get batting statistics of a specific player
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "batting")

# to get pitching statistics of a specific player
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "pitching")

```
