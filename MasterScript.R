## Automatically upload data from the previous-night's games
rm(list = ls())
dir_prefix <- 'C:/Users/matth/Documents/AbsolutelyStackedSupplementaryFiles'
setwd(dir_prefix)
prep_data_only <- T
update_matchups <- F
source(paste0(dir_prefix, '/PredictAndLoadNBA_2024.R'))

## Construct features from the raw data, prepare in a format suitable for model fitting
rm(list = ls())
dir_prefix <- 'C:/Users/matth/Documents/AbsolutelyStackedSupplementaryFiles'
source(paste0(dir_prefix, '/PrepDataForNBA.R'))

## Fit base learners, stacked models, and obtain posteriors for variance components
rm(list = ls())
dir_prefix <- 'C:/Users/matth/Documents/AbsolutelyStackedSupplementaryFiles'
source(paste0(dir_prefix, '/FitStackedModels.R'))

## Make predictions and decisions on bets
rm(list = ls())
dir_prefix <- 'C:/Users/matth/Documents/AbsolutelyStackedSupplementaryFiles'
setwd(dir_prefix)
prep_data_only <- F
update_matchups <- T
source(paste0(dir_prefix, '/PredictAndLoadNBA_2024.R'))

## Process the raw output of decision rules into a more user-friendly format
library(foreach)
dir_prefix <- 'C:/Users/matth/Documents/AbsolutelyStackedSupplementaryFiles'
dat_spread <- read.csv(paste0(dir_prefix, '/SpreadBets.csv'))
dat_ou <- read.csv(paste0(dir_prefix, '/OverUnderBets.csv'))
newspread <- foreach(team = unique(dat_spread$AwayTeam),
                     .combine = 'rbind') %do% {
                       
                       BetHomeSpreadUnder <-  
                         max(dat_spread$VegasSpread[
                           dat_spread$BetAwayOrHomeTeamOnSpread. == 
                             'favor home team' & dat_spread$AwayTeam == team])
                       BetAwaySpreadOver <- 
                         min(dat_spread$VegasSpread[
                           dat_spread$BetAwayOrHomeTeamOnSpread. == 
                             'favor away team' & dat_spread$AwayTeam == team])
                       cbind(dat_spread[which(dat_spread$AwayTeam == team)[1],
                                        c(1,2)],
                             BetHomeSpreadUnder,
                             BetAwaySpreadOver)
                     }
newou <- foreach(team = unique(dat_ou$AwayTeam),
                 .combine = 'rbind') %do% {
                   
                   BetOverIfVegasUnder <- 
                     max(dat_ou$VegasOverUnder[dat_ou$BetOverOrUnder. == 
                                                 'bet over' &
                                                 dat_ou$AwayTeam == team])
                   BetUnderIfVegasOver <- 
                     min(dat_ou$VegasOverUnder[dat_ou$BetOverOrUnder.== 
                                                 'bet under' &
                                                 dat_ou$AwayTeam == team])
                   PointEstimate <- my_preds_total[which(unique(dat_ou$AwayTeam) 
                                                         == team)]
                   cbind(dat_ou[which(dat_ou$AwayTeam == team)[1],
                                c(1,2)],
                         BetUnderIfVegasOver,
                         BetOverIfVegasUnder,
                         PointEstimate)
                 }
write.csv(newspread, 
          file = 
            paste0(dir_prefix, '/NewSpreadBets.csv'),
          row.names = F)
write.csv(newou, file = 
            paste0(dir_prefix, '/NewOverUnderBets.csv'),
          row.names = F)


################################################################################

## Email my decision rules
library(emayili)
library(dplyr)

email <- envelope() %>%
  from("<>") %>%
  to("<>") %>%
  subject("Today's Recommended Bets") %>%
  text("Hello. I've attached the bets recommended by my program below. 
       As a reminder, the program cannot take into account off-court injuries, 
       recent trades, injuries, or drama. Bet at your own discretion.") %>%
  attachment("Today's Recommended Bets are Included Here.",
             path = paste0(dir_prefix, "/MoneyLineBets.csv"),
             name = "MoneyLineBets.csv") %>% 
  attachment("Today's Recommended Bets are Included Here.",
             path = paste0(dir_prefix, "/NewSpreadBets.csv"),
             name = "SpreadBets.csv") %>% 
  attachment("Today's Recommended Bets are Included Here.",
             path = paste0(dir_prefix, "/NewOverUnderBets.csv"),
             name = "OverUnderBets.csv")
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "<>",
               password = "<>")
smtp(email, verbose = F)

################################################################################

## Optional step for extracting vegas lines and saving bet history
library(httr)
library(jsonlite)

## Set your api key from the-odds-api.com
api_key <- "<>"

## Team name mapping (API uses full names, your data uses abbreviations)
team_abbrev <- c(
  "Atlanta Hawks" = "ATL",
  "Boston Celtics" = "BOS",
  "Brooklyn Nets" = "BKN",
  "Charlotte Hornets" = "CHA",
  "Chicago Bulls" = "CHI",
  "Cleveland Cavaliers" = "CLE",
  "Dallas Mavericks" = "DAL",
  "Denver Nuggets" = "DEN",
  "Detroit Pistons" = "DET",
  "Golden State Warriors" = "GSW",
  "Houston Rockets" = "HOU",
  "Indiana Pacers" = "IND",
  "Los Angeles Clippers" = "LAC",
  "Los Angeles Lakers" = "LAL",
  "Memphis Grizzlies" = "MEM",
  "Miami Heat" = "MIA",
  "Milwaukee Bucks" = "MIL",
  "Minnesota Timberwolves" = "MIN",
  "New Orleans Pelicans" = "NOP",
  "New York Knicks" = "NYK",
  "Oklahoma City Thunder" = "OKC",
  "Orlando Magic" = "ORL",
  "Philadelphia 76ers" = "PHI",
  "Phoenix Suns" = "PHX",
  "Portland Trail Blazers" = "POR",
  "Sacramento Kings" = "SAC",
  "San Antonio Spurs" = "SAS",
  "Toronto Raptors" = "TOR",
  "Utah Jazz" = "UTA",
  "Washington Wizards" = "WAS"
)

## Function to get current NBA odds
get_nba_odds <- function(api_key) {
  
  url <- "https://api.the-odds-api.com/v4/sports/basketball_nba/odds/"
  
  cat("Fetching NBA odds from API...\n")
  
  response <- GET(url,
                  query = list(
                    apiKey = api_key,
                    regions = "us",
                    markets = "spreads,totals,h2h",
                    oddsFormat = "american"
                  ))
  
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response))
  }
  
  games <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(games)
}

## Helper function to convert American odds to implied probability
american_to_prob <- function(odds) {
  if (is.na(odds)) return(NA)
  if (odds > 0) {
    return(100 / (odds + 100))
  } else {
    return(abs(odds) / (abs(odds) + 100))
  }
}

## Function to extract spread and O/U for each game
extract_betting_lines <- function(games_data, team_abbrev) {
  
  results <- list()
  
  if (length(games_data) == 0) {
    cat("No games found\n")
    return(data.frame())
  }
  
  for (i in 1:length(games_data$id)) {
    
    away_team_full <- games_data$away_team[i]
    home_team_full <- games_data$home_team[i]
    commence_time <- games_data$commence_time[i]
    
    # Convert to abbreviations
    away_team <- team_abbrev[away_team_full]
    home_team <- team_abbrev[home_team_full]
    
    if (is.na(away_team) || is.na(home_team)) {
      cat("Warning: Could not find abbreviation for", 
          away_team_full, "or", 
          home_team_full, "\n")
      next
    }
    
    # Extract from first bookmaker
    bookmakers <- games_data$bookmakers[[i]]
    if (is.null(bookmakers) || nrow(bookmakers) == 0) {
      cat("No bookmaker data for", away_team, "@", home_team, "\n")
      next
    }
    markets <- bookmakers$markets[[1]]
    
    # Initialize
    home_spread <- NA
    total_points <- NA
    vegas_prob_home <- NA
    vegas_prob_away <- NA
    
    # Get spreads
    spread_idx <- which(markets$key == "spreads")
    if (length(spread_idx) > 0) {
      outcomes <- markets$outcomes[[spread_idx]]
      home_outcome <- outcomes[outcomes$name == home_team_full, ]
      away_outcome <- outcomes[outcomes$name == away_team_full, ]
      if (nrow(home_outcome) > 0) {
        home_spread <- home_outcome$point[1]
        # Get moneyline odds from h2h market for implied probability
      }
    }
    
    # Get moneyline (h2h) for implied probabilities
    h2h_idx <- which(markets$key == "h2h")
    if (length(h2h_idx) > 0) {
      outcomes <- markets$outcomes[[h2h_idx]]
      home_outcome <- outcomes[outcomes$name == home_team_full, ]
      away_outcome <- outcomes[outcomes$name == away_team_full, ]
      if (nrow(home_outcome) > 0 && nrow(away_outcome) > 0) {
        vegas_prob_home <- american_to_prob(home_outcome$price[1])
        vegas_prob_away <- american_to_prob(away_outcome$price[1])
      }
    }
    
    # Get totals (over/under)
    totals_idx <- which(markets$key == "totals")
    if (length(totals_idx) > 0) {
      total_outcome <- markets$outcomes[[totals_idx]]
      if (nrow(total_outcome) > 0) {
        total_points <- total_outcome$point[1]
      }
    }
    
    results[[i]] <- data.frame(
      AwayTeam = away_team,
      HomeTeam = home_team,
      DateTime = commence_time,
      VegasSpread = home_spread,
      VegasOverUnder = total_points,
      VegasImpliedProbHome = vegas_prob_home,
      VegasImpliedProbAway = vegas_prob_away,
      stringsAsFactors = FALSE
    )
  }
  
  result_df <- do.call(rbind, results)
  
  cat("Found", nrow(result_df), "games with odds\n")
  
  return(result_df)
}

## Main function to pull odds and match to predictions
update_history <- function(api_key, dir_prefix, team_abbrev) {
  
  ## Pull current Vegas lines
  games <- get_nba_odds(api_key)
  vegas_lines <- extract_betting_lines(games, team_abbrev)
  
  if (nrow(vegas_lines) == 0) {
    cat("No games to process\n")
    return(data.frame())
  }
  
  ## Load prediction datasets
  spread_file <- paste0(dir_prefix, '/SpreadBets.csv')
  ou_file <- paste0(dir_prefix, '/OverUnderBets.csv')
  ml_file <- paste0(dir_prefix, '/MoneyLineBets.csv')
  
  if (!file.exists(spread_file) || !file.exists(ou_file)) {
    return(data.frame())
  }
  
  dat_spread <- read.csv(spread_file)
  dat_ou <- read.csv(ou_file)
  
  ## Load MoneyLine data if it exists
  has_moneyline <- file.exists(ml_file)
  if (has_moneyline) {
    dat_ml <- read.csv(ml_file)
  }
  
  ## Match and extract predictions
  history <- data.frame()
  new_moneyline <- data.frame()
  
  for (i in 1:nrow(vegas_lines)) {
    game <- vegas_lines[i, ]
    
    # Find matching predictions for spread
    spread_match <- dat_spread[
      dat_spread$HomeTeam == game$HomeTeam & 
        dat_spread$AwayTeam == game$AwayTeam &
        abs(dat_spread$VegasSpread - game$VegasSpread) < 0.6,
      ,
      drop=FALSE
    ][1,,drop=FALSE]
    
    # Find matching predictions for O/U
    ou_match <- dat_ou[
      dat_ou$AwayTeam == game$AwayTeam & 
        dat_ou$HomeTeam == game$HomeTeam &
        abs(dat_ou$VegasOverUnder - game$VegasOverUnder) < 0.6,
      ,
      drop=FALSE
    ][1,,drop=FALSE]
    
    # Find matching moneyline predictions (same tolerance for spread and O/U)
    if (has_moneyline) {
      ml_match <- dat_ml[
        dat_ml$AwayTeam == game$AwayTeam & 
          dat_ml$HomeTeam == game$HomeTeam &
          abs(dat_ml$VegasSpread - game$VegasSpread) < 0.6,
        ,
        drop=FALSE
      ][1,,drop=FALSE]
      
      if (nrow(ml_match) > 0) {
        new_moneyline <- rbind(new_moneyline, ml_match[1,,drop=FALSE])
      }
    }
    
    if (nrow(spread_match) > 0 && nrow(ou_match) > 0) {
      
      # Get probability estimates from moneyline file if available
      prob_home <- NA
      prob_away <- NA
      
      if (has_moneyline && nrow(ml_match) > 0) {
        prob_home <- ml_match$PredictedProbHomeTeamWins[1]
        prob_away <- ml_match$PredictedProbAwayTeamWins[1]
      }
      
      history <- rbind(history, data.frame(
        Date = as.Date(substr(game$DateTime, 1, 10)),
        AwayTeam = game$AwayTeam,
        HomeTeam = game$HomeTeam,
        VegasSpread = game$VegasSpread,
        PredictedSpread = spread_match$PredictedSpread[1],
        VegasOverUnder = game$VegasOverUnder,
        PredictedTotal = ou_match$PredictedTotalPointsScored[1],
        PredictedProbHomeWins = prob_home,
        PredictedProbAwayWins = prob_away,
        VegasImpliedProbHome = game$VegasImpliedProbHome,
        VegasImpliedProbAway = game$VegasImpliedProbAway
      ))
    }
  }
  
  if (nrow(history) == 0) {
    return(data.frame())
  }
  
  
  ## Append to history file (create if doesn't exist)
  history_file <- paste0(dir_prefix, '/history.csv')
  
  if (file.exists(history_file)) {
    existing <- read.csv(history_file)
    history <- rbind(existing, history)
    # Remove duplicates (same game on same date)
    history <- history[!duplicated(
      history[, c("Date", "AwayTeam", "HomeTeam")]), ]
  }
  
  write.csv(history, history_file, row.names = FALSE)
  
  return(history)
}

## Run the update
try({
  today_history <- update_history(api_key, dir_prefix, team_abbrev)
  }, silent=TRUE)

