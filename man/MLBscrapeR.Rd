\name{MLBscrapeR}
\alias{MLBscrapeR}
\title{MLB web scraping R package}

\usage{
getPlayers(team)
getStats(player, team, table)
}

\arguments{
\item{team}{the team to be scraped}
\item{player}{the player to be scraped}
\item{table}{Statistics to be scraped. This must be one of \code{'fielding'}, \code{'batting'},\code{'pitching'}}
}
\description{
These functions scrape baseball team and player statsitics. \code{getPlayers} scrapes a list of players on a given baseball team. \code{getStats} scrapes an individual player's statistics on a given baseball.
}
\examples{
getPlayers(team = "Baltimore Orioles")
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "fielding")
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "batting")
getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "pitching")
}
