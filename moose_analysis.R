library(tidyverse)
library(googlesheets)
library(dplyr)

# read in google sheets
mt <- gs_title("Secret Moose - Game Tracker.xlsx")
tracker <- gs_read(ss=mt, ws = "MOOSE TRACKER", range = "B8:AB205")
game_wrs <- gs_read(ss=mt, ws = "Win Rates by Player Total", range = "A1:C7")

# format moose tracker
tracker$X5 <- NULL
tracker$X6 <- NULL
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
colnames(tracker)[23] <- "DannyHanson"
colnames(tracker)[24] <- "JakeYunker"
# filter table for only games played
tracker <- tracker %>% filter(Game <=no_of_games)
tracker$winners <- rowSums(tracker == 'W') - 1

for (i in 1:no_of_games){
  if(tracker$bResult[i]=='W') {
    tracker$winChk[i] <- round(.6 * tracker$Plyr_no[i])
  } else {
    tracker$winChk[i] <- round(.4 * tracker$Plyr_no[i])
  }
}
# make sure that there are the right number of winners/losers and that the player check number matches the count of players in the game
tracker$winEval <- tracker$winChk - tracker$winners
moose_results <- tracker %>% filter(winEval == 0, `Player Check` == Plyr_no)
# Recount max number of games now that we've removed some illegal ones
no_of_games <- nrow(moose_results)
# Resequence GameIDs
moose_results$Game <- NULL
moose_results <- tibble::rowid_to_column(moose_results, "Game")

# read in moose tracker csv
# moose_results <- read.csv('C:/Users/z076829/Desktop/moose_results.csv', header=TRUE, stringsAsFactors = FALSE)

# moose_results[is.na(moose_results)] <- 0
# moose_results$X.1 <- NULL
# moose_results$X <- NULL

# create list of all players in the moose tracker
players <- vector("list", totPlayers)
for (p in 5:(totPlayers+4)) {
  pPos <- sum(sapply(players, is.null))
  players[[pPos]] <- colnames(moose_results[p])
}

# create base Elo rating df for all players
elo_ratings <- data.frame("players" = matrix(unlist(players), nrow=p-4, byrow=T),
                          "dynER" = 1000,
                          "statER" = 1000,
                          stringsAsFactors = FALSE)

# create new dataframe tracking only results (no player info)  
game_results <- moose_results[,1:4]

# for every game played, identify who played and on which team. then add a blue team/red team column
for (j in 1:no_of_games){
  rTeam <- vector("list", round(moose_results$Plyr_no[j] * .4))
  bTeam <- vector("list", moose_results$Plyr_no[j] - length(rTeam))
  for (i in 5:(totPlayers+4)) {
    if (moose_results$bResult[j] == moose_results[j, i]){
      bListPos <- sum(sapply(bTeam, is.null))
      bTeam[[bListPos]] <- colnames(moose_results[i])
    }
    else if (moose_results$rResult[j] == moose_results[j, i]){
      rListPos <- sum(sapply(rTeam, is.null))
      rTeam[[rListPos]] <- colnames(moose_results[i])
    }
  }
  game_results$bTeam[j] <- paste(bTeam,collapse=" ") 
  game_results$rTeam[j] <- paste(rTeam,collapse=" ")
}

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
for (x in 1:6){
  game_wrs$bAdv[x] <- bAdvCalc(game_wrs$bWR[x]) 
}

# for every game, calculate bRtg and rRtg & Elo Adjustment
# dynER is an elo rating calculated with a blue team probability adjustment that CONSIDERS GAME SIZE
# statER is an elo rating calculated with a blue team probability that is static (and equals TOTAL blue WR, regardless of game size)
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
  # modifier for the likelihood that Blue will win any game
  ## tot_bAdv calculated based on TOTAL WIN RATE OF ALL GAME SIZES probability only
  tot_bAdv <- bAdvCalc(tot_bWR) 
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
  
}

# calculate games played for every player
gp.df <- data.frame("player" = matrix(unlist(players), nrow=totPlayers, byrow=T),
                    "gp" = 0,
                    stringsAsFactors = FALSE)
for (p in 5:(totPlayers+4)){

  gp.df$gp[gp.df$player == colnames(moose_results)[p]] <- nrow(moose_results) - sum(moose_results[, p]==0, na.rm=TRUE)
  
}

# calculate basic win rates & red selection % for every player
wr.df <- data.frame("player" = matrix(unlist(players), nrow=totPlayers, byrow=T),
                    "bGames" = 0,
                    "bWins" = 0,
                    "rWins" = 0,
                    stringsAsFactors = FALSE)

for (p in 1:totPlayers){
  for (g in 1:no_of_games){
    if (grepl(wr.df$player[p], game_results$bTeam[g])){
      wr.df$bGames[p] <- wr.df$bGames[p]+1
      if(game_results$bResult[g] == 'W'){
        wr.df$bWins[p] <- wr.df$bWins[p] + 1
      }
    } else if(grepl(wr.df$player[p], game_results$rTeam[g])){
      if(game_results$rResult[g] == 'W'){
        wr.df$rWins[p] <- wr.df$rWins[p] + 1
      }
    }
    }
}

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
# elo_ratings <- elo_ratings %>% filter(gp > 10)
elo_ratings <- elo_ratings[order(-elo_ratings$dynER),]
elo_ratings <- elo_ratings[c('players', 'gp', 'statER', 'dynER')]

### OPTIONAL RANKS BASED ON dynER & GAMES PLAYED THRESHOLD
elo_ratings_rnk <- elo_ratings %>% filter(gp >= 20)
elo_ratings_rnk <- tibble::rowid_to_column(elo_ratings_rnk, "rank")
elo_ratings_rnk <- elo_ratings_rnk[c('rank','players', 'gp', 'statER', 'dynER')]

rm(bPlayers, gmPlayers, gp.df, rPlayers, tracker, b, bAdv, bEloAdj, bListPos, bProb, bRatings, bRatingsDyn, bRatingsStat, bResult,
   bTeam, g, i, j, mt, no_of_games, players, pPos, r, rEloAdj, rListPos, rProb, rRatings, rRatingsDyn, rRatingsStat, rResult, rTeam,
   tot_bAdv, tot_bWR, totPlayers, x)
