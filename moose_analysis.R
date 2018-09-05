library(tidyverse)
library(googlesheets)
library(plyr)
library(dplyr)
library(plotly)

# read in google sheets
mt <- gs_title("Secret Moose - Game Tracker.xlsx")
tracker <- gs_read(ss=mt, ws = "MOOSE TRACKER", range = "B8:AB205")
game_wrs <- gs_read(ss=mt, ws = "Win Rates by Player Total", range = "A1:C7")
rm(mt)

# format moose tracker
tracker[, 5:6] <- NULL
colnames(tracker)[2:4] <- c("Plyr_no", "bResult", "rResult")
tracker$bResult[is.na(tracker$bResult)] <- "L"
tracker$rResult[is.na(tracker$rResult)] <- "L"
tracker[is.na(tracker)] <- 0

# format game win rates tracker and create total blue win rate variable
game_wrs$bWR <- game_wrs$bWins / (game_wrs$bWins + game_wrs$bLosses)
tot_bWR <- sum(game_wrs$bWins) / (sum(game_wrs$bWins) + sum(game_wrs$bLosses))

# people suck ass at filling out the sheet, so create a check system to ensure the number of red/blue players is accurate per game size
no_of_games <- max(tracker$Game[tracker$Plyr_no > 0])
totPlayers <- ncol(tracker) - 5
# column name clean-up
colnames(tracker) <- make.names(colnames(tracker), unique=TRUE)
# filter table for only games played
tracker <- tracker %>% filter(Game <=no_of_games)
tracker$winners <- rowSums(tracker == 'W') - 1

# create a column for how many winning players there SHOULD be for each game
tracker$winChk <- ifelse(tracker$bResult=='W', round(.6 * tracker$Plyr_no), round(.4 * tracker$Plyr_no))

# make sure that there are the right number of winners/losers and that the player check number matches the count of players in the game
tracker$winEval <- tracker$winChk - tracker$winners
moose_results <- tracker %>% filter(winEval == 0, Player.Check == Plyr_no)
rm(tracker)
# Recount max number of games now that we've removed some illegal ones
no_of_games <- nrow(moose_results)
# Resequence GameIDs
moose_results$Game <- NULL
moose_results <- tibble::rowid_to_column(moose_results, "Game")

# create base Elo rating df for all players in the tracker
elo_ratings <- data.frame("players" = colnames(moose_results[, 1:totPlayers+4]),
                          "dynER" = 1000,
                          "statER" = 1000,
                          stringsAsFactors = FALSE)

# create new dataframe tracking only game outcomes (no player info)  
game_results <- moose_results[,1:4]

# for every game played, identify who played and on which team. then add a blue team/red team column
teamsFunc <- function(x) {
  blue <- ifelse(length(names(which(x != 'L' & x!=0))) > 
                   length(names(which(x != 'W' & x!=0))),
           paste(unlist(as.list(names(which(x != 'L' & x!=0)))), collapse = ' '),
           paste(unlist(as.list(names(which(x != 'W' & x!=0)))), collapse = ' '))
  red <- ifelse(length(names(which(x != 'L' & x!=0))) > 
                  length(names(which(x != 'W' & x!=0))),
                paste(unlist(as.list(names(which(x != 'W' & x!=0)))), collapse = ' '),
                paste(unlist(as.list(names(which(x != 'L' & x!=0)))), collapse = ' '))
  c(blue, red)
}

game_teams <- apply(moose_results[1:totPlayers+4], 1, teamsFunc)
game_results$bTeam <- t(game_teams)[,1]
game_results$rTeam <- t(game_teams)[,2]

# f(x) to calculate elo rtg blue advantage modifier
bAdvCalc <- function (win_prob) {
  
  numerator <- -((10^2.5)*win_prob)
  denominator <- -(1-win_prob)
  xRtg <- numerator/denominator
  adv <- log(xRtg) / log(10)
  adv <- adv - (5/2)
  adv <- adv*400
  
  return(adv)
}

# calculate bAdv for every game size
game_wrs$bAdv <- bAdvCalc(game_wrs$bWR)
# create blank dataframe to track each player's dynamic elo rating over time
dynER_tracker <- data.frame("game_no" = as.integer(),
                            "player" = as.character(),
                            "dynER" = as.integer())

# for every game, calculate bRtg and rRtg & Elo Adjustment
# dynER is an elo rating calculated with a blue team probability adjustment that CONSIDERS GAME SIZE
# statER is an elo rating calculated with a blue team probability that is static (and equals TOTAL blue WR, regardless of game size)
eloLoop <- function(elo_ratings, game_results, no_of_games, tot_bAdv, game_wrs, dynER_tracker){
  for (i in 1:(no_of_games)) {
    bPlayers <- data.frame("players" = matrix(unlist(as.list(str_split(game_results$bTeam[i], " "))), 
                                              nrow=round(game_results$Plyr_no[game_results$Game == i] * .6), 
                                              byrow=T),
                           stringsAsFactors = FALSE)
    bPlayers <- dplyr::inner_join(bPlayers, elo_ratings, by = c("players" = "players"))
    bRatingsDyn <- sum(bPlayers$dynER) / round(game_results$Plyr_no[game_results$Game == i] * .6)
    bRatingsStat <- sum(bPlayers$statER) / round(game_results$Plyr_no[game_results$Game == i] * .6)
    
    rPlayers <- data.frame("players" = matrix(unlist(as.list(str_split(game_results$rTeam[i], " "))), 
                                              nrow=round(game_results$Plyr_no[game_results$Game == i] * .4), 
                                              byrow=T),
                           stringsAsFactors = FALSE)
    rPlayers <- dplyr::inner_join(rPlayers, elo_ratings, by = c("players" = "players"))
    rRatingsDyn <- sum(rPlayers$dynER) / round(game_results$Plyr_no[game_results$Game == i] * .4)
    rRatingsStat <- sum(rPlayers$statER) / round(game_results$Plyr_no[game_results$Game == i] * .4)
    
    game_results$bRatingDyn[i] <- bRatingsDyn
    game_results$rRatingDyn[i] <- rRatingsDyn
    
    game_results$bRatingStat[i] <- bRatingsStat
    game_results$rRatingStat[i] <- rRatingsStat

    ## bAdv calculated & applied based on dynamic win rates of each game size
    bAdv <- game_wrs$bAdv[game_wrs$`# of Players`==game_results$Plyr_no[i]]
    
    ## DYNAMIC WIN RATES --------------------------------------------------------------
    b <- 10 ^ ((bRatingsDyn + bAdv)/400)
    r <- 10 ^ (rRatingsDyn/400)
    
    bProb <- b / (b+r)
    rProb <- r / (r+b)
    
    bResult <- if_else(game_results$bResult[i]=='W', 1, 0)
    rResult <- if_else(bResult == 1, 0, 1)
    
    bEloAdj <- 25*(bResult - bProb)
    rEloAdj <- 25*(rResult- rProb)
    
    bPlayers$dynER <- round(bPlayers$dynER + bEloAdj)
    rPlayers$dynER <- round(rPlayers$dynER + rEloAdj)
    gmPlayers <- rbind(bPlayers, rPlayers)
    ## --------------------------------------------------------------------------------
    
    ## STATIC WIN RATE  ---------------------------------------------------------------
    b <- 10 ^ ((bRatingsStat + tot_bAdv)/400)
    r <- 10 ^ (rRatingsStat/400)
    
    bProb <- b / (b+r)
    rProb <- r / (r+b)
    
    bEloAdj <- 25*(bResult - bProb)
    rEloAdj <- 25*(rResult- rProb)
    
    bPlayers$statER <- round(bPlayers$statER + bEloAdj)
    rPlayers$statER <- round(rPlayers$statER + rEloAdj)
    gmPlayers <- rbind(bPlayers, rPlayers)
    
    # left join elo ratings to updated rankings per game played and delete duplicate column (but keep existing ratings for ppl who didn't play)
    elo_ratings <- elo_ratings %>% left_join(gmPlayers, by=c("players" = "players"))
    elo_ratings$dynER <- if_else(is.na(elo_ratings$dynER.y), elo_ratings$dynER.x, elo_ratings$dynER.y)
    elo_ratings$statER <- if_else(is.na(elo_ratings$statER.y), elo_ratings$statER.x, elo_ratings$statER.y)
    elo_ratings <- elo_ratings %>%  select(-dynER.x, -dynER.y, -statER.x, -statER.y)
    ## --------------------------------------------------------------------------------
    gmPlayers$game_no <- i
    dynER_tracker <- rbind(dynER_tracker, gmPlayers[, c(4, 1, 2)])
  }
  return(list("total"=elo_ratings, "cumulative"=dynER_tracker))
}

# modifier for the likelihood that Blue will win any game
## tot_bAdv calculated based on TOTAL WIN RATE OF ALL GAME SIZES probability only
tot_bAdv <- bAdvCalc(tot_bWR) 
ratings_vector <- eloLoop(elo_ratings, game_results, no_of_games, tot_bAdv, game_wrs, dynER_tracker)
elo_ratings <- as.data.frame(ratings_vector$total)
dynER_tracker <- as.data.frame(ratings_vector$cumulative)

# calculate games played for every player
gp.df <- data.frame("player" = colnames(moose_results[, 1:totPlayers+4]),
                    "gp" = 0,
                    stringsAsFactors = FALSE)
gp.df$gp <- apply(moose_results[1:totPlayers+4], 2, function(x) sum(x!=0))

# calculate basic win rates & red selection % for every player
wr.df <- data.frame("player" = colnames(moose_results[, 1:totPlayers+4]),
                    "bGames" = 0,
                    "bWins" = 0,
                    "rWins" = 0,
                    stringsAsFactors = FALSE)

# function to calculate the total number of blue games played, blue wins and red wins for every player
winRateFunc <- function(player) {
  
  bGames <- game_results %>% filter(grepl(player, bTeam)==TRUE) %>% nrow(.)
  bWins  <- game_results %>% filter(grepl(player, bTeam)==TRUE, bResult == "W") %>% nrow(.)
  rWins <- game_results %>% filter(grepl(player, rTeam)==TRUE, rResult == "W") %>% nrow(.)
  
  return(c(bGames, bWins, rWins))
}

# stitch returned list of vectors from winRateFunc into a dataframe & insert into wr.df
wr.df[, 2:4] <- plyr::ldply(lapply(as.list(wr.df$player), winRateFunc))[, 1:3]

wr.df <- wr.df %>% inner_join(gp.df) %>% mutate("rGames" = gp - bGames,
                                                "r%" = paste0(round((rGames / gp)*100, 0), "%"), # how often a player is Red
                                                "bWR" = paste0(round((bWins / bGames)*100, 0), "%"), # actual blue win rate by player
                                                "rWR" = paste0(round((rWins / rGames)*100, 0), "%"), # actual red win rate by player
                                                "tWR" = paste0(round(((rWins + bWins) / gp)*100, 0), "%"), # actual total win rate by player
                                                "xBW" = round(bGames * tot_bWR, 2), # number of blue wins an average blue player would have (static blue WR)
                                                "xRW" = round(rGames* (1-tot_bWR), 2), # number of red wins an average red player would have (static red WR)
                                                "xW_basic" = xBW + xRW, # sum of expected blue and red wins, if player was average. accounts for % team selection
                                                "xWR_basic" = paste0(round((xW_basic / gp)*100, 0), "%"), # players basic expected win rate, using above calcs
                                                "WABE" = round((bWins+rWins) - xW_basic, 2), # Wins Above Basic Expectation. Actual Wins - expected Wins
                                                "WABE/g" = round(WABE/gp, 2), # WABE per game played
                                                "WABE/100g" = round(`WABE/g`*100, 2) # number of wins above expectation assuming everyone played 100 games
                                                ) %>% 
  select(-rGames, -bGames, -bWins, -rWins, -xBW, -xRW)
  
elo_ratings <- elo_ratings %>% inner_join(gp.df, by=c("players"="player"))
elo_ratings <- elo_ratings[order(-elo_ratings$dynER),]
elo_ratings <- elo_ratings[c('players', 'gp', 'statER', 'dynER')]

### OPTIONAL RANKS BASED ON dynER & GAMES PLAYED THRESHOLD
elo_ratings_rnk <- elo_ratings %>% filter(gp >= 20)
elo_ratings_rnk <- tibble::rowid_to_column(elo_ratings_rnk, "rank")
elo_ratings_rnk <- elo_ratings_rnk[c('rank','players', 'gp', 'statER', 'dynER')]

### FORMAT THE CUMULATIVE DYNAMIC ELO RATING TRACKER & GRAPH OUTPUTS
dynER_tracker <- dynER_tracker[order(dynER_tracker$players), ]
dynER_tracker$gms_played <- ave(dynER_tracker$players, dynER_tracker$players, FUN=seq_along)
dynER_tracker$players <- as.factor(dynER_tracker$players)
dynER_tracker$gms_played <- as.integer(dynER_tracker$gms_played)

elo_graph <- plot_ly(dynER_tracker, x = ~gms_played, y = ~dynER, type = 'scatter', mode = 'lines', split = ~players) %>% 
  layout(xaxis = list(title='Games Played'), yaxis = list(title='Dynamic Elo Rating'))
elo_graph  

rm(gp.df, game_teams, no_of_games, tot_bAdv, tot_bWR, totPlayers)
