#' A helper function for getting 3-letter abbreviation of a baseball team
#'
#' @param team
#'
#' @return a 3-letter abbreviation of the given baseball team
#'
#' @import stringdist
#' @import stringr
#'
#' @examples
#' getAbb("Baltimore Orioles")
getAbb <- function(team = "Baltimore Orioles"){

    if(length(team) != 1){
       stop("Input has to be of length 1", call. = FALSE)
    }else{

        data(team_table)

        team_numb <- nrow(team_table)
        team <- stringr::str_replace_all(tolower(team), "[[:punct:]]", "")
        team <- stringr::str_replace_all(team, "[[:blank:]]", "")

        team_tab <- team_table[,-1]
        team_tab <- apply(team_tab, 2, tolower)
        team_tab <- apply(team_tab, 2, stringr::str_replace_all, pattern = "[[:punct:]]", "")
        team_tab <- apply(team_tab, 2, stringr::str_replace_all, pattern = "[[:blank:]]", "")
        team_tab <- as.data.frame(team_tab, stringsAsFactors = FALSE)
        # check for exact match
        ind_abb <- stringr::str_which(tolower(team_table$abb), team)
        ind_full <- stringr::str_which(team_tab$full, team)
        ind_loc <- stringr::str_which(team_tab$loc, team)
        ind_name <- stringr::str_which(team_tab$name, team)
        # if not exact match, us 'jw' with penalty = 0.1 to fuzzy match
        if(sum(ind_abb) != 0){
            team_abb <- team_table$abb[ind_abb]
        }else{
            if(sum(ind_full) != 0){
                team_abb <- team_table$abb[ind_full]
            }else{
                if(sum(ind_loc) != 0){
                    team_abb <- team_table$abb[ind_loc]
                }else{
                    diss_loc <- stringdist::stringdist(team, team_tab$loc, method = "jw", p=0.1)
                    ind_loc = which(diss_loc == min(diss_loc))
                    if(sum(ind_name) != 0){
                        team_abb <- team_table$abb[ind_name]
                    }else{
                        diss_name <- stringdist::stringdist(team, team_tab$name, method = "jw", p=0.1)
                        ind_name = which(diss_loc == min(diss_name))
                    }
                }
                diss_full <- stringdist::stringdist(team, team_tab$full, method = "jw", p=0.1)
                ind_full = which(diss_full == min(diss_full))

                diss_vec <- c(diss_full, diss_loc, diss_name)
                ind <- which(diss_vec == min(diss_vec)) %% team_numb
                team_abb <- team_table$abb[ind]
            }
        }

        if(length(team_abb) != 1){
            stop("invalid entry")
        }else{
            return(team_abb)
        }
     }
}


#' Get a list of team players
#'
#' @param team
#'
#' @return a character vector of players in a given baseball team
#'
#' @import stringr
#' @import rvest
#' @import xml2
#'
#' @examples
#' getPlayers("Boston Red Sox")
#'
#' @export
#'
getPlayers <- function(team){
    team <- getAbb(team)
    url <- stringr::str_c("http://www.baseball-reference.com/teams/",
                             team, "/2017-roster.shtml")
    data <- rvest::html_table(rvest::html_nodes(xml2::read_html(url), "table")[2])[[1]]
    player_names <- data[-nrow(data),]$Name

    return(player_names)
}


#' Get individual player statistics
#'
#' @param player
#' @param team
#' @param table
#'
#' @import stringr
#' @import rvest
#' @import xml2
#'
#' @return a data frame of player's statistics as requested by table parameter
#' @export
#'
#' @examples
#' getStats(player = "Dylan Bundy", team = "Baltimore Orioles", table = "fielding")
#'
getStats <- function(player = "Dylan Bundy", team = "Baltimore Orioles", table = "fielding"){

    team = getAbb(team)
    url <- stringr::str_c("http://www.baseball-reference.com/teams/",
                 team, "/2017-", table, ".shtml")

    if(table == "batting" | table == "pitching"){
        data <- rvest::html_nodes(xml2::read_html(url), "table")
        stats <- rvest::html_table(data[[1]])

        stats$Name <- stringr::str_replace_all(stats$Name, "\\([[:print:]]*\\)", "")
        stats$Name <- stringr::str_replace_all(stats$Name, "\\*", "")
        stats$Name <- stringr::str_replace_all(stats$Name, "\\#", "")
        stats$Name <- stringr::str_replace_all(stats$Name, "[[:blank:]]*$", "")
        stats$Name <- tolower(stats$Name)
        ind <- tolower(stats$Name) == tolower(player)

        if(sum(ind) ==0){
            return("Player stats not found")
        }else{
            stats <- stats[ind, ]
            dat <- stats[-c(2,3)]
            dat <- sapply(dat, as.character)
            dat <- sapply(dat, function(e) ifelse(e == "", as.numeric(NA), as.numeric(e)))
            dat <- data.frame(dat)
            dat <- rbind(Name = player, Pos = as.character(stats$Pos), dat)
            names(dat) <- NULL
            dat <- as.data.frame(t(dat), stringsAsFactors = FALSE)
            # suppress NA coersion warning message
            suppressWarnings(dat[2:ncol(dat)] <- as.numeric(dat[2:ncol(dat)]))

            return(dat)
        }
    }

    if(table == "fielding"){
        fielding <- xml2::read_html(url)
        dat <- rvest::html_nodes(fielding, "body")
        dat <- rvest::html_nodes(dat, xpath = "//div[@id='content']")
        dat <- rvest::html_nodes(dat, xpath = "//div[@id='all_standard_fielding']")
        dat <- rvest::html_nodes(dat, xpath = "//comment()")
        dat <- rvest::html_text(dat[15])[1]

        dat = unlist(strsplit(dat, '\\n'))
        start <- stringr::str_which(dat, "<tbody>") + 1
        end <- stringr::str_which(dat, "<tfoot>") - 3
        dat <- dat[start:end]
        ind <- stringr::str_which(dat, player)

        if(sum(ind) == 0){
            return("Player stats not found")
        }else{
            dat <- dat[ind]
            dat <- stringr::str_replace_all(dat, "^[[:print:]]*shtml\">", "")
            dat <- stringr::str_replace_all(dat, '\\</a\\>\\</strong\\>\\</th\\>\\<td class=\"right \" ', "")
            dat <- stringr::str_replace_all(dat, '\\</td\\>\\<td class=\"right \" ', "")
            dat <- stringr::str_replace_all(dat, '\\</td\\>\\<td class=\"left \" ', "")
            dat <- stringr::str_replace_all(dat, '\\</td\\>\\</tr\\>', "")
            dat <- stringr::str_replace_all(dat, '_def', "")
            dat <- unlist(stringr::str_split(dat, 'data-stat=\"'))
            dat <- stringr::str_replace_all(dat, ' csk=\"[[:print:]]+\"', "")
            dat <- stringr::str_replace_all(dat, '\\%', "")
            dat <- stringr::str_replace_all(dat, '_perc', "\\%")
            dat <- stringr::str_replace_all(dat, '_per_season', "/yr")
            dat <- stringr::str_replace_all(dat, '_per_game', "/G")
            dat <- stringr::str_replace_all(dat, '_per_nine', "/9")
            dat <- stringr::str_replace_all(dat, 'age', "Age")
            dat <- stringr::str_replace_all(dat, 'chances', "Ch")
            dat <- stringr::str_replace_all(dat, 'fielding', "Fld")
            dat <- stringr::str_replace_all(dat, 'tz_runs_total', "Rtot")
            dat <- stringr::str_replace_all(dat, 'bis_runs_total', "Rdrs")
            dat <- stringr::str_replace_all(dat, 'range_factor', "RF")
            dat <- stringr::str_replace_all(dat, 'caught_stealing%_lg', "lgCS%")
            dat <- stringr::str_replace_all(dat, 'caught_stealing', "CS")
            dat <- stringr::str_replace_all(dat, 'pickoffs', "Pickoffs")
            dat <- stringr::str_replace_all(dat, 'pos_summary', "Pos Summary")
            dat <- stringr::str_split(dat, '\" >')

            dat_names <- sapply(dat[-1], "[", 1)
            dat_stats <- sapply(dat[-1], "[", 2)
            dat_stats <- ifelse(dat_stats == "", NA, dat_stats)
            Pos_sum <- dat_stats[length(dat_stats)]
            dat_stats <- rbind(dat[1],
                               as.data.frame(as.numeric(dat_stats[-length(dat_stats)])),
                               Pos_sum)
            names(dat_stats) <- NULL
            rownames(dat_stats) <- c("Player", dat_names)
            dat_stats <- as.data.frame(t(dat_stats), stringsAsFactors = FALSE)
            end <- ncol(dat_stats) - 1
            dat_stats[2:end] <- as.numeric(dat_stats[2:end])

            return(dat_stats)
        }
    }
}
















