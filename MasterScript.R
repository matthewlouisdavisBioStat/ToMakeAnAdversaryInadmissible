## Automatically upload data from the previous-night's games, assuming a 2024-2025 season
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
rm(list = ls())
prep_data_only <- T
update_matchups <- F
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/PredictAndLoadNBA_2024.R')

## Construct features from the raw data, prepare in a format suitable for model fitting
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/PrepDataForNBA.R')

## Fit base learners, stacked models, and obtain posteriors for variance components
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/FitStackedModels.R')

## Make predictions and decisions on bets
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
rm(list = ls())
prep_data_only <- F
update_matchups <- T
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/PredictAndLoadNBA_2024.R')

## Process the raw output of decision rules into a more user-friendly format
library(foreach)
dat_spread <- read.csv('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/SpreadBets.csv')
dat_ou <- read.csv('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/OverUnderBets.csv')
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
            'C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewSpreadBets.csv',
          row.names = F)
write.csv(newou, file = 
            'C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewOverUnderBets.csv',
          row.names = F)


## Email my decision rules to me and my friend
library(emayili)
library(dplyr)

# email <- envelope() %>%
#   from("matthewlouisdavis@gmail.com") %>%
#   to("myfriendwhogambles1776@gmail.com") %>%
#   subject("Today's Recommended Bets") %>%
#   text("Good afternon, here are my program's recommended bets.") %>%
#   attachment("Today's Recommended Bets are Included Here.",
#              path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/MoneyLineBets.csv",
#              name = "MoneyLineBets.csv") %>%
#   attachment("Today's Recommended Bets are Included Here.",
#              path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewSpreadBets.csv",
#              name = "SpreadBets.csv") %>%
#   attachment("Today's Recommended Bets are Included Here.",
#              path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewOverUnderBets.csv",
#              name = "OverUnderBets.csv")
# smtp <- server(host = "smtp.gmail.com",
#                port = 465,
#                username = "matthewlouisdavis@gmail.com",
#                password = "<>")
# smtp(email, verbose = F)
# rm(list = ls())

email <- envelope() %>%
  from("matthewlouisdavis@gmail.com") %>%
  to("matthewlouisdavis@gmail.com") %>%
  subject("Today's Recommended Bets") %>%
  text("Hello. I've attached the bets recommended by my program below. 
       As a reminder, the program cannot take into account off-court injuries, 
       recent trades, injuries, or drama. Bet at your own discretion.") %>%
  attachment("Today's Recommended Bets are Included Here.",
             path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/MoneyLineBets.csv",
             name = "MoneyLineBets.csv") %>% 
  attachment("Today's Recommended Bets are Included Here.",
             path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewSpreadBets.csv",
             name = "SpreadBets.csv") %>% 
  attachment("Today's Recommended Bets are Included Here.",
             path = "C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/NewOverUnderBets.csv",
             name = "OverUnderBets.csv")
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "matthewlouisdavis@gmail.com",
               password = "<>")
smtp(email, verbose = F)

rm(list = ls())
