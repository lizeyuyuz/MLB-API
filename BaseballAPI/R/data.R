#' Names of 30 active baseball teams in 2017
#'
#' A dataset listing abbreviations, full names, locations and partial names of baseball teams.
#'
#' @docType data
#'
#' @usage data(team_table)
#'
#' @format A data frame with 30 rows and 4 variables:
#' \describe{
#'   \item{abb}{3-letter abbreviation of a baseball team}
#'   \item{full}{complete full name of a baseball team}
#'   \item{loc}{location contained in the full name of a baseball team}
#'   \item{name}{partial name contained in the full name of a baseball team}
#' }
#'
#' @keywords datasets
#'
#' @source \url{http://www.baseball-reference.com/teams/}
#'
#' @examples
#' data(team_table)
#' abb <- team_table$abb
#' full <- team_table$full
#' loc <- team_table$loc
#' name <- team_table$name
"team_table"
