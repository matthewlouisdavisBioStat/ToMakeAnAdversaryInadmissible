## ## ## ## 
## Use the script below to pull new NBA data, and make predictions given new data from Vegas
## ## ## ## 

################################################################################
  ## ## Setup ## ## 
# 
strt <- Sys.time()
for(k in paste0('./Stacking/',c(
           'glmer_constrained.R', 
           'HelperFunctions.R', 
           'make_dummy_Z_and_sigmalist.R',
           'make_glmerStackedModel.R', 
           'make_loglikelihood_MLMetric.R',
           'MLModel_DataPrep.R', 
           'predict_glmerStacked.R',
           'quantify_stacked_uncertainty.R', 
           'SoftmaxOperations.R', 
           'stepAIC_stacked.R'))){
  source(k)
}

################################################################################
## NBA schedule for today and matchups
################################################################################

#sched <- nbastatR::seasons_schedule(seasons = 2025)
#sched[sched$dateGame == as.character(Sys.Date()),]
date <- as.character(Sys.Date())

  url = "https://cdn.nba.com/static/json/staticData/scheduleLeagueV2_1.json"
  json <-
    jsonlite::fromJSON(url, simplifyVector = T,
                       simplifyDataFrame = T,
                       flatten = T)
    season <- json$leagueSchedule$seasonYear
    id <- json$leagueSchedule$leagueId
    tbl_dates <- json$leagueSchedule$gameDates
    sched_dat <- data.table::rbindlist(tbl_dates$games,fill = T)
    
    ## optional loading/saving sched
    #save(sched_dat, file = 'nba_schedule_data.RData')
    #load('nba_schedule_data.RData')
    
    sched_subs <- sched_dat[substr(sched_dat$gameDateUTC,1,10) == date,]
    
    ## No games or only 1 game, include tomorrow's predictions
     if(nrow(sched_subs) <= 1){
      warning("Including dates from tomorrow's games")
      sched_subs <- rbind(sched_subs,
              sched_dat[substr(sched_dat$gameDateUTC,1,10) == 
                             as.character(Sys.Date()+1),])
     }
     if(nrow(sched_subs) <= 1){
      warning("Using first game for dummy purposes")
      sched_subs <- sched_dat[1:2,]
     }
    sched_subs$slugMatchup <- paste0(sched_subs$awayTeam.teamTricode," @ ", 
                                     sched_subs$homeTeam.teamTricode)

teams <- sapply(sched_subs$slugMatchup, function(x){
  if(grepl(' vs.', x)){
    rev(unlist(strsplit(x, " vs. ")))
  } else if(grepl(' @ ', x)){
    unlist(strsplit(x, ' @ '))
  }
})
home_team <- teams[2,]
away_team <- teams[1,]
datetime <- as.character(Sys.time())
matchups_enter <- cbind(away_team,
                        home_team,
                        0,
                        0,
                        0,
                        0,
                        date)
playoffs <- F

################################################################################
## ## Load data and libraries 
################################################################################

start_algo <- Sys.time()
library(MachineShop)
library(dplyr)
library(recipes)
library(foreach)
library(doParallel)
library(purrr)
library(data.table)
library(doSNOW)
library(magrittr)
library(R.utils)
library(nbastatR)
options(curl_interrupt = TRUE)
load("bs.RData")

## how many games do we need previously for each outcome
lag_og <- 5
lag <- lag_og

## profit cutoff, make 2.5 cents per dollar
cutoff <- .025

## process and save matchups
matchups_og <- matchups_enter[,c(1:4),drop=FALSE]
if(update_matchups){
  dat <- read.csv('matchup_data.csv')
  colnames(matchups_enter) <- colnames(dat)
  dat %>%
    rbind(matchups_enter %>%
            as.data.frame) %>%
    unique %>%
    write.csv(file ='matchup_data.csv',row.names = F)
}
matchups_og <- matchups_og %>% 
  as.data.frame(stringsAsFactors = FALSE, drop = FALSE)

## extract spread/over under/moneyline odds
prior_mu_spread <- as.numeric(matchups_og[,3])
prior_mu_ou <- as.numeric(matchups_og[,4])
matchups_og <- matchups_og[, c(1, 2)]

cat("\n\n\n\n\n\n\n\n\n++++++++++++++++++++++++++++++++++++\n\n\n\n\n\n\n")

################################################################################
## ## Load last night's data, bind it to what we have with error handling
################################################################################

load('bs.RData')
years <- 2025
gl <- game_logs(years, result_types = 'team',
                assign_to_environment = FALSE) %>%
  mutate(outcome = as.numeric(as.factor(outcomeGame))-1)
gl <- gl[!(gl$dateGame %in%  c(Sys.Date())),] %>%
  mutate(outcome = as.numeric(as.factor(outcomeGame))-1)
gl <- gl[order(gl$dateGame),]
gl <- gl[!is.na(gl$outcomeGame),]
ids <- unique(gl$idGame)
start <- Sys.time()


ids <- ids[!(ids %in% bs$idGame)]
if(max(table(bs$idGame)) > 2){
  stop("duplicated ids")
}
if(length(ids) > 0) {
  for (id in ids[1:min(length(ids),100)]) {
    print(paste(grep(id, ids), length(ids), sep = "/"))
    bs_temp <- try({
      withTimeout({
        box_scores(
          game_ids = id,
          result_types = "team",
          box_score_types = c(
            "Advanced",
            "Scoring",
            "Four Factors",
            "Misc",
            "tracking"
          ),
          assign_to_environment = FALSE
        )$dataBoxScore %>%
          as.data.frame()
      }, timeout = 25000, onTimeout = "silent")
    }, silent = TRUE)
    if (class(bs_temp) == "data.frame") {
      bs <- bind_rows(bs, bs_temp)
    } else{
      print("failed")
    }
    end <- Sys.time()
    print(end - start)
  }
  end <- Sys.time()
  print(end - start)
  bs <- unique(bs)
  bs <- bs[bs$minExact >= 239,]
  save(bs,file = "bs.RData")
}

## duplicate to catch the errors on the first pull
bs <- bs[!(bs$idGame %in% ids),]
if(length(ids) > 0) {
  for (id in ids[1:min(length(ids),100)]) {
    print(paste(grep(id, ids), length(ids), sep = "/"))
    bs_temp <- try({
      withTimeout({
        box_scores(
          game_ids = id,
          result_types = "team",
          box_score_types = c(
            "Advanced",
            "Scoring",
            "Four Factors",
            "Misc",
            "tracking"
          ),
          assign_to_environment = FALSE
        )$dataBoxScore %>%
          as.data.frame()
      }, timeout = 25000, onTimeout = "silent")
    }, silent = TRUE)
    if (class(bs_temp) == "data.frame") {
      bs <- bind_rows(bs, bs_temp)
    } else{
      print("failed")
    }
    end <- Sys.time()
    print(end - start)
  }
  end <- Sys.time()
  print(end - start)
  bs <- unique(bs)
  bs <- bs[bs$minExact >= 239,]
  save(bs,file = "bs.RData")
}

bs <- bs[!(bs$idGame %in% ids),]
if(length(ids) > 0) {
  for (id in ids[1:min(length(ids),100)]) {
    print(paste(grep(id, ids), length(ids), sep = "/"))
    bs_temp <- try({
      withTimeout({
        box_scores(
          game_ids = id,
          result_types = "team",
          box_score_types = c(
            "Advanced",
            "Scoring",
            "Four Factors",
            "Misc",
            "tracking"
          ),
          assign_to_environment = FALSE
        )$dataBoxScore %>%
          as.data.frame()
      }, timeout = 25000, onTimeout = "silent")
    }, silent = TRUE)
    if (class(bs_temp) == "data.frame") {
      bs <- bind_rows(bs, bs_temp)
    } else{
      print("failed")
    }
    end <- Sys.time()
    print(end - start)
  }
  end <- Sys.time()
  print(end - start)
  bs <- unique(bs)
  bs <- bs[bs$minExact >= 239,]
  save(bs,file = "bs.RData")
}

bs <- bs[!(bs$idGame %in% ids),]
if(length(ids) > 0) {
  for (id in ids[1:min(length(ids),100)]) {
    print(paste(grep(id, ids), length(ids), sep = "/"))
    bs_temp <- try({
      withTimeout({
        box_scores(
          game_ids = id,
          result_types = "team",
          box_score_types = c(
            "Advanced",
            "Scoring",
            "Four Factors",
            "Misc",
            "tracking"
          ),
          assign_to_environment = FALSE
        )$dataBoxScore %>%
          as.data.frame()
      }, timeout = 25000, onTimeout = "silent")
    }, silent = TRUE)
    if (class(bs_temp) == "data.frame") {
      bs <- bind_rows(bs, bs_temp)
    } else{
      print("failed")
    }
    end <- Sys.time()
    print(end - start)
  }
  end <- Sys.time()
  print(end - start)
  bs <- unique(bs)
  bs <- bs[bs$minExact >= 239,]
  save(bs,file = "bs.RData")
}


bs <- bs[!(bs$idGame %in% ids),]
if(length(ids) > 0) {
  for (id in ids[1:min(length(ids),100)]) {
    print(paste(grep(id, ids), length(ids), sep = "/"))
    bs_temp <- try({
      withTimeout({
        box_scores(
          game_ids = id,
          result_types = "team",
          box_score_types = c(
            "Advanced",
            "Scoring",
            "Four Factors",
            "Misc",
            "tracking"
          ),
          assign_to_environment = FALSE
        )$dataBoxScore %>%
          as.data.frame()
      }, timeout = 25000, onTimeout = "silent")
    }, silent = TRUE)
    if (class(bs_temp) == "data.frame") {
      bs <- bind_rows(bs, bs_temp)
    } else{
      print("failed")
    }
    end <- Sys.time()
    print(end - start)
  }
  end <- Sys.time()
  print(end - start)
  bs <- unique(bs)
  bs <- bs[bs$minExact >= 239,]
  save(bs,file = "bs.RData")
}

min(table(bs$slugTeam[bs$idGame %in% gl$idGame[gl$yearSeason == 2025]]))


################################################################################
## ## Feature Engineering
################################################################################

## making an indicator for before and after 2017, before and after 2022
gl$before_or_after_2017 <- as.numeric(gl$yearSeason >= 2017)
gl$before_or_after_2022 <- as.numeric(gl$yearSeason >= 2022)

## optional, if training on playoff data is considered
gl$is_playoff_game <- as.numeric(gl$typeSeason == "Playoffs")
gl$is_playoff_game <- as.numeric(gl$typeSeason == "Playoffs")

## base variables
vars <- colnames(gl)[grepl("Team",colnames(gl))]
vars <- c(na.omit(vars[9:31]))
vars <- vars[!(vars %in% c("plusminusTeam",
                           "minutesTeam",
                           "outcome"))]
vars <- unique(c(vars))
nonpermin_vars <- c(c(vars[grep("pct",vars)]),"outcome","plusminusTeam")
non_lag_vars <- c("isB2BSecond",
                  "isB2BFirst",
                  "countDaysRestTeam",
                  "numberGameTeamSeason",
                  "ptsTeamNextGame")
vars <- vars[-grep("pct",vars)]


## box score variables
box_vars <-
  c(
    'pctAST',
    'pctOREB',
    'pctDREB',
    'pctTREB',
    'pctTOVE',
    'pctTOVTeam',
    'pctEFG',
    'pctTS',
    'ortgE',
    'ortg',
    'drtgE',
    'drtg',
    'netrtgE',
    'netrtg',
    'ratioASTtoTOV',
    'ratioAST',
    'paceE',
    'pace',
    'pacePer40PACE_PER40',
    'possessions',
    'ratioPIE',
    'pctFGAasFG2',
    'pctFGAasFG3',
    'pctPTSasFG2',
    'pctPTSasFG2asMR',
    'pctsPTSasFG3',
    'pctPTSasFB',
    'pctPTSasFT',
    'pctPTSasOffTOV',
    'pctPTSasPaint',
    'pctFG2MasAssisted',
    'pctFG2MasUnassisted',
    'pctFG3MasAssisted',
    'pctFG3MasUnassisted',
    'pctFGMasAssisted',
    'pctFGMasUnassisted',
    'pctEFGOpponent',
    'pctTOVOpponent',
    'pctOREBOpponent',
    'rateFTA',
    'rateFTAOpponent',
    'ptsOffTOV', 
    'ptsSecondChance', 'ptsFastBreak', 
    'ptsPaint', 'ptsOffTOVOpponent', 
    'ptsSecondChanceOpponent', 'ptsFastBreakOpponent',
    'ptsPaintOpponent', 'blk', 'blka', 'pf', 'pfd', 'passes',
    'fgmContested', 'fgaContested', 'pctFGContested', 
    'fgmUncontested', 'fgaUncontested', 'pctFGUncontested', 
    'fgmRimDefended', 'fgaRimDefended', 'pctFGRimDefended', 
    'orebChances', 'drebChances', 'trebChances', 'touches', 'astSecondary', 
    'ftAST', 'ast'
  ) %>% unique
scale_box_vars <- c(   'ptsOffTOV', 
                       'ptsSecondChance', 'ptsFastBreak', 
                       'ptsPaint', 'ptsOffTOVOpponent', 
                       'ptsSecondChanceOpponent', 'ptsFastBreakOpponent',
                       'ptsPaintOpponent', 'blka','pfd', 'passes',
                       'fgmContested', 'fgaContested',  
                       'fgmUncontested', 'fgaUncontested',
                       'fgmRimDefended', 'fgaRimDefended',
                       'orebChances', 
                       'drebChances', 'trebChances', 'touches', 'astSecondary', 
                       'ast',
                       'possessions',
                       'ftAST')
box_vars <- box_vars[!(box_vars %in% scale_box_vars)]
box_vars <- box_vars[paste0(box_vars) %in% colnames(bs)]
scale_box_vars <- scale_box_vars[scale_box_vars %in% colnames(bs)]
start <- Sys.time()
new_gl <- data.frame()
teams <- unique(gl$nameTeam)
bs <- unique(bs)
bs$teamName[bs$teamName == "Los Angeles Clippers"] <- "LA Clippers"
gl$nameTeam[gl$nameTeam == "Los Angeles Clippers"] <- "LA Clippers"
edit_vars <- c('logpts','sqrtpts')
average_vars <- unique(c(paste0(vars,"PerMinute"),
                         nonpermin_vars,
                         box_vars,
                         paste0(scale_box_vars,"PerMinute"),
                         'arcsinsqrt_pts',
                         'ptsTeam',
                         'ptsTeamOpp',
                         'logPtsTeam',
                         'logPtsTeamOpp',
                         'sqrtPtsTeam',
                         'sqrtPtsTeamOpp',
                         'outcomeOpp',
                         'spread',
                         edit_vars))
gl$logpts <- log(gl$ptsTeam)
gl$sqrtpts <- 2*sqrt(gl$ptsTeam + 3/8)
gl <- gl[gl$dateGame <= date,]
for(year in years){
  cat("\n\t", year)
  new_gl_temp <- data.frame()
  for(team in unique(gl$nameTeam)){
    print(paste(grep(team,teams),length(teams),sep = " / "))
    data <- gl %>%
      subset((nameTeam %in% team) & (yearSeason %in% year)) %>%
      as.data.frame()
    data <- data[order(as.Date(data$dateGame),decreasing = F),]
    if(nrow(data) > 0){
      
      ## record the next games outcome for training
      data <- scale_per_minute(data,vars)
      bs_temp <- bs[(bs$idGame %in% data$idGame) & 
                      (bs$idTeam %in% data$idTeam),]
      bs_temp_opp <- bs[(bs$idGame %in% data$idGame) & 
                          !(bs$idTeam %in% data$idTeam),]
      gl_temp <- gl[(gl$idGame %in% data$idGame) & 
                      (gl$idTeam %in% data$idTeam),]
      gl_temp_opp <- gl[(gl$idGame %in% data$idGame) & 
                          !(gl$idTeam %in% data$idTeam),]
      gl_temp$arcsinsqrt_pts <- asin(sqrt(
        (gl_temp$ptsTeam + 3/8)/
          (gl_temp$ptsTeam + 
             gl_temp_opp$ptsTeam +
             3/4)
      ))
      gl_temp$ptsTeam <- gl_temp$ptsTeam*240/gl_temp$minutesTeam
      gl_temp$sqrtPtsTeam <- 2*sqrt(gl_temp$ptsTeam + 3/8)
      gl_temp$logPtsTeam <- log(gl_temp$ptsTeam)
      gl_temp$ptsTeamOpp <-    gl_temp_opp$ptsTeam*240/
                                     gl_temp_opp$minutesTeam
      gl_temp$logPtsTeamOpp <- log(gl_temp_opp$ptsTeam*240/
                                     gl_temp_opp$minutesTeam)
      gl_temp$sqrtPtsTeamOpp <- 2*sqrt(gl_temp_opp$ptsTeam*240/
                                     gl_temp_opp$minutesTeam + 3/8)
      gl_temp$outcomeOpp <- gl_temp_opp$outcome
      gl_temp$spread <- gl_temp$ptsTeam - gl_temp$ptsTeamOpp
      gl_temp <- gl_temp[,c('idGame',
                            'arcsinsqrt_pts',
                            'ptsTeamOpp',
                            'logPtsTeamOpp',
                            'sqrtPtsTeamOpp',
                            'ptsTeam',
                            'sqrtPtsTeam',
                            'logPtsTeam',
                            'outcomeOpp',
                            'spread')]
      colnames <- colnames(bs_temp)
      colnames_opp <- colnames(bs_temp_opp)
      bs_temp <- scale_per_minute(bs_temp,scale_box_vars)
      bs_temp_opp <- scale_per_minute(bs_temp_opp,scale_box_vars)
      colnames(bs_temp_opp) <- paste0(colnames(bs_temp_opp),"_opp")
      colnames(bs_temp_opp)[grep('idGame_opp',colnames(bs_temp_opp))] <- 'idGame'
      
      ## the opponent stats 
      if(nrow(bs_temp) > 0){
        data <- merge(data,bs_temp,by="idGame") %>%
          merge(bs_temp_opp,by = "idGame") %>%
          merge(gl_temp,by = 'idGame')
      } else{
        print("skip")
        next
      }
      datatemp <- data
      dataLG <- data[,colnames(data) %in% average_vars]
      for(i in 1:nrow(data)){
        for(var in c(average_vars[average_vars %in% 
                                  colnames(data)],
                     paste0(average_vars,'_opp')[
                       paste0(average_vars,'_opp') %in% colnames(data)])){
          
          ## weighted average over all previous games
          w <- (sapply((1:max(i-1,1)),function(x)1/x))
          datatemp[i,var] <- weighted.mean(
            as.numeric(unlist(data[max(i-1,1):1,var])),
            w,
            na.rm = TRUE)
          
          ## drop-off of last-two available game's data versus weighted average
          dataLG[i,var] <- as.numeric(na.omit(unlist(data[i:1,var])))[1] -
                           datatemp[i,var]
        }
      }
      colnames(dataLG) <- paste0(colnames(dataLG),
                                 "LastGame")
      data <- bind_cols(datatemp,dataLG)
        data$year <- as.numeric(year)
        new_gl <- bind_rows(new_gl,data)
    }
  }
}
end <- Sys.time()
start-end

################################################################################
## ## Merging Matchup Data to Pre-Processed Data 
################################################################################

## manually merge the next ids based on matchups
new_gl$idTeam <- new_gl$slugTeam.x ## instead of chaning bs, gl etc.
new_gl$idTeam.x %>% unique %>% sort
matchups <- matchups_og
for (i in 1:nrow(matchups)) {
  matchups$nextGameID[i] <- i
  matchups$nextGameID[i] <- i
}
matchups <- data.frame(
  "idTeam" = c(matchups[,1], matchups[,2]),
  "nextGameID" = c(matchups$nextGameID, matchups$nextGameID),
  "locationGameNext" = c(rep("A", nrow(matchups)), rep("H", nrow(matchups)))
)
max_dates <- new_gl %>%
  group_by(idTeam) %>%
  summarize(max = max(dateGame,na.rm = T)) %>%
  mutate(merge_id = paste0(idTeam, max))
new_gl$merge_id <- paste0(new_gl$idTeam,
                          new_gl$dateGame)
new_gl_dated <- new_gl %>%
  subset((merge_id %in% max_dates$merge_id) &
           (idTeam %in% matchups$idTeam))
new_gl_away <-
  new_gl_dated[new_gl_dated$idTeam %in% 
                 matchups$idTeam[matchups$locationGameNext == "A"], ]
new_gl_home <-
  new_gl_dated[new_gl_dated$idTeam %in% 
                 matchups$idTeam[matchups$locationGameNext == "H"], ]
new_gl_home <- merge(new_gl_home, matchups, by = "idTeam")
new_gl_away <- merge(new_gl_away, matchups, by = "idTeam")
new_gl_merged <- merge(new_gl_home, new_gl_away, by = "nextGameID")
new_gl_merged$idGame.x <- NULL
new_gl_merged$idGame.y <- NULL
new_gl_merged$nextOutcome <- new_gl_merged$nextOutcome.x
new_gl_merged$nextOutcome.x = new_gl_merged$nextOutcome.y = NULL

## vars to use
cols <- 
  c(paste(colnames(data),'.x'),
    paste0(colnames(data),'.y'))

## more cleaning
new_gl_merged$isB2BFirst.x <-
  as.numeric(factor(new_gl_merged$isB2BFirst.x)) - 1
new_gl_merged$isB2BSecond.x <-
  as.numeric(factor(new_gl_merged$isB2BFirst.x)) - 1
new_gl_merged$isB2BFirst.y <-
  as.numeric(factor(new_gl_merged$isB2BFirst.y)) - 1
new_gl_merged$isB2BSecond.y <-
  as.numeric(factor(new_gl_merged$isB2BFirst.y)) - 1
cols <- cols[cols %in% colnames(new_gl_merged)]

## exclude these based on collinearities/variable importance plots
exclude_cols <- c('cityTeam.x',
                  'cityTeam.y',
                  'cityTeam_opp.x',
                  'cityTeam_opp.y',
                  'dateGame.x',
                  'dateGame.y',
                  'distMiles.x',
                  'distMiles.y',
                  'distMiles_opp.x',
                  'distMiles_opp.y',
                  'hasVideo.x',
                  'hasVideo.y',
                  'hasVideo_opp.x',
                  'hasVideo_opp.y',
                  colnames(new_gl_merged)[
                    grep('idTeam',colnames(new_gl_merged))],
                  colnames(new_gl_merged)[
                    grep('urlTeam',colnames(new_gl_merged))],
                  colnames(new_gl_merged)[
                    grep('teamName',colnames(new_gl_merged))],
                  colnames(new_gl_merged)[
                    grep('slug',colnames(new_gl_merged))],
                  colnames(new_gl_merged)[
                    grep('outcomeGame',colnames(new_gl_merged))],
                  colnames(new_gl_merged)[
                    grep('video',colnames(new_gl_merged))],
                  'team.x',
                  'team.y',
                  'team_opp.x',
                  'team_opp.y',
                  'nextGameID',
                  'nextOutcome',
                  c(paste0(c("pctDREBLastGame",
                             "pctOREBLastGame",
                             "pctTREBLastGame",
                             "pctDREBLastGame2",
                             "pctOREBLastGame2",
                             "pctTREBLastGame2"),
                           ".x"),
                    paste0(c("pctDREBLastGame",
                             "pctOREBLastGame",
                             "pctTREBLastGame",
                             "pctDREBLastGame2",
                             "pctOREBLastGame2",
                             "pctTREBLastGame2"),
                           ".y")),
                  c(paste0(c("pctDREBLastGame",
                             "pctOREBLastGame",
                             "pctTREBLastGame",
                             "pctDREBLastGame2",
                             "pctOREBLastGame2",
                             "pctTREBLastGame2"),
                           ".x"),
                    paste0(c("pctDREBLastGame",
                             "pctOREBLastGame",
                             "pctTREBLastGame",
                             "pctDREBLastGame2",
                             "pctOREBLastGame2",
                             "pctTREBLastGame2"),
                           ".y"),
                    paste0(c(  "pctDREB_oppLastGame",
                               "pctOREB_oppLastGame",
                               "pctTREB_oppLastGame",
                               "pctDREB_oppLastGame2",
                               "pctOREB_oppLastGame2",
                               "pctTREB_oppLastGame2"),
                           ".x"),
                    paste0(c("pctDREB_oppLastGame",
                             "pctOREB_oppLastGame",
                             "pctTREB_oppLastGame",
                             "pctDREB_oppLastGame2",
                             "pctOREB_oppLastGame2",
                             "pctTREB_oppLastGame2"),
                           ".y")),
                  c('countDaysNextGameTeam.x', 
                    'XXptsTeamNextGame.x', 
                    'countDaysNextGameTeam.y', 
                    'XXptsTeamNextGame.y', 
                    'minutesNextGame.y')
)

## final dataset prepared for prediction
df <- new_gl_merged[, !(colnames(new_gl_merged) %in% exclude_cols)] %>%
  apply(2, as.numeric) %>%
  as.data.frame
if (dim(df)[1] > dim(df)[2]) {
  df <- df %>% t %>% as.data.frame
}
rownames(df) <- rownames(new_gl_merged)
df$isB2BFirst.x <- as.numeric(df$isB2BFirst.x)
df$isB2BSecond.x <- as.numeric(df$isB2BFirst.x)
df$isB2BFirst.y <- as.numeric(df$isB2BFirst.y)
df$isB2BSecond.y <- as.numeric(df$isB2BSecond.y)
df_og <- df
nacols <- colnames(df[,apply(df,2,function(x)any(is.na(x)))])
print(nacols)


################################################################################
## ## Helper Functions and post-processing
################################################################################


  ## make a new Z matrix to replace in the super fit for random intercepts
  ## quickly automates this process


predict2 <- function(fit,
                     use_randint = T, 
                     typ = 'numeric', 
                     switch_z_sign = F, 
                     dff = df){
  Zmat <- (0*fit$super_fit$data$Zmat)[1:nrow(dff),]
  if(use_randint){
    for(i in 1:nrow(matchups_og)){
      if(switch_z_sign){
        ## matchups have - Z's 
        Zmat[i,paste0(c(matchups_og[i,1],
                        matchups_og[i,2]),"_",year)] <- c(1,-1)
      } else {
        
        ## everything else has positive Z's
        Zmat[i,paste0(c(matchups_og[i,1],
                        matchups_og[i,2]),"_",year)] <- c(1, 1)
      }
    }
    fit$super_fit$data$Zmat <- Zmat
  } else {
    for(i in 1:nrow(matchups_og)){
      Zmat[i,paste0(c(matchups_og[i,1],
                      matchups_og[i,2]),"_",year)] <- 0
    }
    fit$super_fit$data$Zmat <- Zmat
  }
  predict(fit,newdata = dff,type = typ)
}
rownames <- rownames(df)
df$nextOutcome <- NULL

## potential outcome variables, ignore as predictors
outcome_vars <- c('logPtsGame',
                  'ptsGame',
                  'spreadDiff',
                  'overtime',
                  'arcsinsqrt_prop',
                  'logitPropPts')

## last step; insert playoff interaction terms for all variables
for(col in colnames(df)[!(colnames(df) %in% outcome_vars)]){
  df$interaction <- df$is_playoff_game.x * as.numeric(unlist(df[[col]]))
  colnames(df)[colnames(df)=="interaction"] <- paste0("Playoffsx",
                                                      col)
}
if(playoffs){
  df$is_playoff_game <- 1
}
incl_random_efx <- T
fixed_weights <- F
rm(bs)
rm(new_gl)

## more cleaning for previous outcomes - can't do inside the loop
df$outcome.x <- asin(sqrt(df$outcome.x))
df$outcome.y <- asin(sqrt(df$outcome.y))
df$outcomeOpp.x <- asin(sqrt(df$outcomeOpp.x))
df$outcomeOpp.y <- asin(sqrt(df$outcomeOpp.y))
df <- unique(df)
years2keep <-
  c('2010',
    '2011',
    '2012',
    '2013',
    '2014',
    '2015',
    '2016',
    '2017',
    '2018',
    '2019',
    '2020',
    '2021',
    '2022',
    '2023',
    '2024',
    '2025'
  )
yrz <- foreach(year = years2keep,
               .combine = 'cbind') %do% {
                 1 * (df$year.x == year)
               }
colnames(yrz) <- paste0("year_", years2keep)
df <- bind_cols(df, yrz)

## feature reduc vars
load('no_featreduc_vars.RData')
no_featreduc_vars <- 
  no_featreduc_vars[!(no_featreduc_vars == 'numberGameTeamSeason.x')]

################################################################################
## ## Posterior-Predictive on Absolute Scale 
################################################################################
  
## overtime predictions
load('stacked_model_overtime.RData')

## proportion of overtime games that go past single overtime
alph <- 0.008013314 / (0.05116193 + 0.008013314) 
ot_preds <- ((predict2(stacked_model,use_randint = F, typ = 'prob')))
ot_preds2 <- alph * ot_preds
ot_preds <- ot_preds - ot_preds2
ot_preds <- cbind(1-ot_preds-ot_preds2,ot_preds,ot_preds2)
colnames(ot_preds) <- c("240","265","290")
print(ot_preds)
if(any(ot_preds < 0)){
  stop("There are negative probabilities of overtime????")
} else if(any(ot_preds[,1] < .8)){
  stop("No fucking way is there >20% of overtime - something is wrong!")
} else if(prep_data_only){
  print("Successful Data Update")
} else {
  
rm(stacked_model)
  
## points-per-48 predictions, scaled by overtime predictions
load('stacked_model_gaussian.RData')
linear_sigma <- stacked_model$super_fit$data$final_fit$tau

my_preds_per48 <- as.numeric((predict2(stacked_model)))
my_preds_linear <- sapply(1:length(my_preds_per48),function(p){
  my_preds_per48[p]*(ot_preds[p,1]) + 
    my_preds_per48[p]*(265/240)*ot_preds[p,2] + 
    my_preds_per48[p]*(290/240)*ot_preds[p,3]
})
my_preds_per48 <- rep(my_preds_per48,each = 81)


## average minutes per game
(avg_time <- sum((c(0.9408248,0.05116193,0.008013314) * 
                    c(48, (48+5), (48+10)))))
sigmaSq0 <-  max(1e-64, 17.72642^2 - 
             stacked_model$super_fit$data$final_fit$tau^2)
rm(stacked_model)

## get vegas predictions, based on log model predictions in fact
load('stacked_model_log.RData')

## Include variable for predicted minutes played,
## plug-in avg time, since this is just temporary
## we re-load the model, plug in the posterior-mean of overtime draws later
## to make our real poisson predictions
## this is just to get a meaningful spread of potential vegas predictions 
## for making potential decisions on
df$minutesPlayedPer48 <-  
  0
my_preds <- as.numeric(log(predict2(stacked_model,use_randint = F)))
vegas_preds <- c(sapply(exp(my_preds),function(x){
  x <- round(x)
  seq(x - 20, x + 20, 0.5)
}))
rm(stacked_model)
vegas_preds_per48 <- vegas_preds*(48/avg_time)

## Load model and posterior draws of variance parameters
load('stacked_stan_gaussian.RData')
dr <- stacked_stan$draws()
posterior_draws <- 
  cbind(c(as.data.frame(dr[,,2])[-c(1:2000),1],
          as.data.frame(dr[,,2])[-c(1:2000),2]),
        c(as.data.frame(dr[,,3])[-c(1:2000),1],
          as.data.frame(dr[,,3])[-c(1:2000),1]))
# posterior_draws <- cbind(
#   c(stacked_stan@sim$samples[[1]]$dispersion[-c(1:2000)],
#     stacked_stan@sim$samples[[2]]$dispersion[-c(1:2000)]),
#   c(stacked_stan@sim$samples[[1]]$sigmaSq_etahat[-c(1:2000)],
#     stacked_stan@sim$samples[[2]]$sigmaSq_etahat[-c(1:2000)]))

## Estimate of correlation
rho <-  0.8178977

## Doesn't change
rho_prior_mean <- mean(atanh(rbeta(5000000,2.25,1.5)))
rho_prior_var <- var(atanh(rbeta(5000000,2.25,1.5)))

## Prior on rho was estimated from 629 observations in 2022-2023
post_rho_sd <- sqrt(1/(1/rho_prior_var + (629-3)))
post_rho_mean <- (rho*(629-3) + rho_prior_mean/rho_prior_var)*post_rho_sd^2
posterior_predictive_linear <- foreach(i = 1:nrow(posterior_draws),
                                       .combine = 'rbind') %do% {
                                         disp <- posterior_draws[i,1]
                                         
   ## divide by proportion of variability explained by vegas already
   ## what is the proportion of variability explained by vegas,
   ## that can't be explained simply because they are predicting the same thing
   ## what information from vegas alone is present in our predictions?
                                         sigmaSq_etahat <- posterior_draws[i,2]
                                         var_eta_given_y <- 1/(1/disp + 
                                                                 1/sigmaSq0)
                                         var <- 1/(1/(sigmaSq0) + 1/
                                                     (sigmaSq_etahat + 
                                                        var_eta_given_y))
                                         w1 <- var/sigmaSq0
                                         w2 <- var/(sigmaSq_etahat + 
                                                      var_eta_given_y)
                                         eta_sampled <- 
                                           rnorm(length(my_preds_per48))*
                                           sqrt(var + 
                                                2*tanh(
                                                  rnorm(1, 
                                                        post_rho_mean, 
                                                        post_rho_sd))*
                                                  w1*
                                                  w2*
                                                  sqrt(sigmaSq0)*
                                                  sqrt((sigmaSq_etahat + 
                                                          var_eta_given_y))
                                            ) + 
                                           (my_preds_per48 /  
                                              (sigmaSq_etahat + 
                                                 var_eta_given_y) + 
                                              vegas_preds_per48 / 
                                              (sigmaSq0)) * var
                                         
                                         rnorm(length(my_preds_per48))*
                                           sqrt(disp) + eta_sampled
                                       } 



################################################################################
## ## Incorporate overtime posterior predictive
################################################################################

load('stacked_stan_overtime.RData')
dr <- stacked_stan$draws()
posterior_draws <- 
  cbind(c(as.data.frame(dr[,,dim(dr)[3]])[-c(1:2000),1],
          as.data.frame(dr[,,dim(dr)[3]])[-c(1:2000),1]))
# posterior_draws <- 
#   cbind(c(stacked_stan@sim$samples[[1]]$sigma_etahat[-c(1:2000)],
#   stacked_stan@sim$samples[[2]]$sigma_etahat[-c(1:2000)]))
rm(stacked_stan, dr)
my_preds <- ot_preds[,2] + ot_preds[,3]
vegas_preds_ot <- 0.05800569 
sigmaSq0 <- 1/(16223*vegas_preds_ot * (1 - vegas_preds_ot))
logit <- function(x)log(x/(1-x))

## Posterior predictiveof overtime draws
posterior_predictive_scaled <- foreach(i = 1:nrow(posterior_draws),
                                       .combine = 'rbind') %do% {
                                         
                                         sigma_etahat <- posterior_draws[i,1]
                                         
                                         var_eta_given_y <- 
                                           1/((16223*my_preds*(1-my_preds)) + 
                                                                 1/sigmaSq0)
                                         var <- 1/(1/(sigmaSq0) + 
                                                     1/(sigma_etahat^2 + 
                                                        var_eta_given_y))
                                         
                                         eta_sampled <- rnorm(length(my_preds))*
                                           sqrt(var) + 
                                           (logit(my_preds) / 
                                              (sigma_etahat^2 + 
                                               var_eta_given_y) + 
                                              logit(vegas_preds_ot) / 
                                              (sigmaSq0)) * var
                                         
                                         eta_sampled
                                       }
posterior_predictive_probs <- 1/(1+exp(-posterior_predictive_scaled))[,
                                    rep(1:ncol(posterior_predictive_scaled),
                                        each = 81)]
post_ot_short <- 1/(1+exp(-colMeans(posterior_predictive_scaled)))
prob_ot <- rep(post_ot_short, each = 81)

## our posterior draws are united with posterior predictive of overtime
posterior_predictive_linear <- 
  posterior_predictive_linear *
  (1 + posterior_predictive_probs*(1-alph)*5/48 + 
       posterior_predictive_probs*alph*10/48)
cum_probs <- t(sapply(1:ncol(posterior_predictive_linear),
                      function(i){
                        x <- posterior_predictive_linear[,i]
                        vegas_pred <- vegas_preds[i]
                        c(mean(x < (vegas_pred)),
                          mean(x > (vegas_pred)))
                      }))
post_sd_linear <- apply(posterior_predictive_linear,2,sd)
map <- apply(posterior_predictive_linear,2,mean)
colnames(cum_probs) <- c("P(mcmc < line)","P(mcmc > line)")

## P(mcmc < spread) means spread is too much in favor of home team, bet over
## P(mcmc > spread) means spread is too much in favor of away team, bet under

## mu0 how much will home team win by
## mu how much I think they will win by
## post mean: the bayesian estimate of how much they will win by
p_win <- apply(cum_probs,1,max)
p_lose <- apply(cum_probs,1,min)
bet <- ifelse(.909*p_win - 1*p_lose > cutoff,
              ifelse(cum_probs[,1] < cum_probs[,2],
                     "bet over",
                     "bet under"),
              "don't bet")
post_map <- map
absscale <- cbind(cum_probs,vegas_preds,rep(my_preds_linear,each = 81),
                  post_map,.909*p_win - 1*p_lose,bet)
colnames(absscale)[4] <- "my_preds"


################################################################################
## ## Posterior-Predictive on Log-Scale
################################################################################

## Raw predictions per 48 minutes
load('stacked_model_log.RData')
tau <- stacked_model$super_fit$data$final_fit$tau
df$minutesPlayedPer48 <- 0

## Regulation time
my_preds_a <- exp(as.numeric(log(predict2(stacked_model,
                                          dff = df, 
                                          use_randint = T))))
my_preds_a_pois <- (my_preds_a / tau^2)
df$minutesPlayedPer48 <- log((48+5)/48)

## Overtime
my_preds_b <- exp(as.numeric(log(predict2(stacked_model, 
                                          dff = df, 
                                          use_randint = T))))
my_preds_b_pois <- (my_preds_b / tau^2)
df$minutesPlayedPer48 <- log((48+10)/48)

## Double overtime
my_preds_c <- exp(as.numeric(log(predict2(stacked_model, 
                                          use_randint = T, 
                                          dff = df))))
my_preds_c_pois <- (my_preds_c / tau^2)
my_preds_log <- log(
              (1-post_ot_short)*(my_preds_a) +
              (1-alph)*post_ot_short*(my_preds_b) +
              alph*post_ot_short*(my_preds_c)
              )
my_preds_total <- exp(my_preds_log)*0.5 + my_preds_linear*0.5
my_preds <- rep(my_preds_log, each = 81)
sigmaSq0s <- 0.08112754^2  -  1/(stacked_model$super_fit$data$final_fit$tau^2 * 
                                   vegas_preds) # variance of log pois

## posterior distributions for parameters of interest
load('stacked_stan_log.RData')
#stacked_stan <- stacked$stacked_stan
dr <- stacked_stan$draws()
posterior_draws <- 
  cbind(1,
        (c(as.data.frame(dr[,,dim(dr)[3]])[-c(1:2000),1],
           as.data.frame(dr[,,dim(dr)[3]])[-c(1:2000),2])/mean(my_preds_total)))
# posterior_draws <- cbind(1,
#                       c(stacked_stan@sim$samples[[1]]$sigma_etahat[-c(1:2000)],
#                         stacked_stan@sim$samples[[2]]$sigma_etahat[-c(1:2000)]))
my_preds_pois <- my_preds-log(tau^2)
vegas_preds_pois <- log(vegas_preds)-log(tau^2)
## extracted from prior analysis - the estimated correlation coefficient
rho <- 0.9783063
post_rho_mean <- (rho*(629-3) + rho_prior_mean/rho_prior_var)*post_rho_sd^2
## basic estimate of how much the posterior sd tends to be overestimated
posterior_predictive_log <- foreach(i = 1:nrow(posterior_draws),
                                       .combine = 'rbind') %do% {
                                         disp <- posterior_draws[i,1]
                                         
                                        
                                         my_preds_pois_temp <- my_preds_pois
                                         
                                         
                                         sigma_etahat <- posterior_draws[i,2] / 
                                           tau
                                         var_eta_given_y <- 1/
                                           (exp(my_preds_pois_temp) + 
                                              1/sigmaSq0s)
                                         
                                         
                                         
                                         var <- 1/(1/(sigmaSq0s) + 
                                                     1/(sigma_etahat^2 + 
                                                     var_eta_given_y))
                                         
                                         w1 <- var/sigmaSq0s
                                         w2 <- var/(sigma_etahat^2 + 
                                                      var_eta_given_y)
                                         eta_sampled <- rnorm(
                                           length(my_preds_pois_temp))*
                                           sqrt(var + 
                                                  2*tanh(rnorm(1, 
                                                               post_rho_mean, 
                                                               post_rho_sd))*
                                                  w1*
                                                  w2*
                                                  sqrt(sigmaSq0s)*
                                                  sqrt((sigma_etahat^2 + 
                                                          var_eta_given_y))
                                           ) + 
                                           (my_preds_pois_temp / 
                                              (sigma_etahat^2+var_eta_given_y) + 
                                            vegas_preds_pois / 
                                              (sigmaSq0s)) * var
                                         
                                         tau^2 * rpois(length(eta_sampled),
                                                       exp(eta_sampled))
                                         
                                       }
cum_probs <- t(sapply(1:ncol(posterior_predictive_log),
                   function(i){
                     x <- posterior_predictive_log[,i]
                     vegas_pred <- vegas_preds[i]
                     c(mean(x < vegas_pred),
                       mean(x > vegas_pred))
                   }))
map <- apply(posterior_predictive_log,2,mean)
post_sd_log <- apply(posterior_predictive_log,2,sd)
colnames(cum_probs) <- c("P(mcmc < line)","P(mcmc > line)")

## mu0 how much will home team win by
## mu how much I think they will win by
## post mean: the bayesian estimate of how much they will win by
p_win <- apply(cum_probs,1,max)
p_lose <- apply(cum_probs,1,min)
bet <- ifelse((.909*p_win - 1*p_lose) > cutoff,
              ifelse(cum_probs[,1] < cum_probs[,2],
                     "bet over",
                     "bet under"),
              "don't bet")

## the 'expectation' ie sum of x * p(x)
post_map <- map
logscale <- cbind(cum_probs,
                   vegas_preds,
                   exp(my_preds),
                   post_map,.909*p_win - 1*p_lose,bet)
post_draws_log <- posterior_predictive_log
colnames(logscale)[4] <- "my_preds"


################################################################################
## ## Organize OU output
################################################################################


## Finally....
## make bet when log scale and abs scale are in agreement
absscale <- as.data.frame(absscale)
logscale <- as.data.frame(logscale)


colnames(logscale) <- colnames(absscale)
final_bet <- cbind(absscale[,'bet'],
                   logscale[,'bet']) %>%
  apply(1,function(x)if(x[1] == x[2]) x[1] else "don't bet")
final_overunder <- cbind(rep(new_gl_merged$dateGame.x,each = 81),
                         matchups_og[rep(1:nrow(matchups_og),
                                         each = 81),],
                         absscale$bet,
                         logscale$bet,
                         final_bet,
                         absscale$my_preds,
                         logscale$my_preds,
                         vegas_preds,
               absscale[,'post_map'],
               logscale[,'post_map'],
               post_sd_linear,
               post_sd_log,
               as.character(Sys.time()))

colnames(final_overunder) <- 
                   c("Date",
                     "AwayTeam",
                     "HomeTeam",
                     "BetAbs",
                     "BetLog",
                     "FinalBet",
                     "Predict Abs",
                     "Predict Log",
                     "Over/Under",
                     "Posterior Abs",
                     "Posterior Log",
                     "SD Abs",
                     "SD Log",
                     "DateBet")

## Save history of decision rules
# past_data <- read.csv("history_of_bets2024_overunder.csv",
#                       stringsAsFactors = F)
# rownames(final_overunder) <- NULL
# colnames(past_data) <- colnames(final_overunder)
# write.csv(unique(rbind(past_data,final_overunder)),
#           file = "history_of_bets2024_overunder.csv",
#                        row.names = F)

cat("\n\n-----------------------------------------\n\n")

################################################################################
## ## Spread 
################################################################################

load('stacked_model_spread.RData')
my_preds <- as.numeric((predict2(stacked_model,
                                 use_randint = T,
                                 switch_z_sign =  T)))
my_preds_raw <- my_preds
vegas_preds_spread <- c(sapply(my_preds,function(x){
  x <- round(x)
  seq(x - 20, x + 20, 0.5)
}))
my_preds <- rep(my_preds, each = 81) 

## we actually KNOW the true variance, luckily, from prior analysis and VSTs
sigmaSq0 <-  (12.39497)^2 - stacked_model$super_fit$data$final_fit$tau^2
if(sigmaSq0 < 0){
  sigmaSq0 <- (12.39497)^2  - mse(response(stacked_model),
                                  predict(stacked_model))
  if(sigmaSq0 < 0){
    sigmaSq0 <- abs(sigmaSq0)/2 # damn, this sucks
    print("Damn our model sucks")
    Sys.sleep(5)
  }
} 


## extract posterior draws of variance components
rm(stacked_model)
load('stacked_stan_spread.RData')
dr <- stacked_stan$draws()
posterior_draws <- 
  cbind(c(as.data.frame(dr[,,2])[-c(1:2000),1],
          as.data.frame(dr[,,2])[-c(1:2000),2]),
        c(as.data.frame(dr[,,3])[-c(1:2000),1],
          as.data.frame(dr[,,3])[-c(1:2000),1]))

## fixed prior correlation based on based analysis
rho <- sqrt(1-0.5342973) # = 0.6824242
post_rho_mean <- (rho*(629-3) + rho_prior_mean/rho_prior_var)*post_rho_sd^2
posterior_predictive_spread <- foreach(i = 1:nrow(posterior_draws),
                                       .combine = 'rbind') %do% {
                                         disp <- posterior_draws[i,1]
                                         sigmaSq_etahat <- posterior_draws[i,2]
                                         var_eta_given_y <- 1/(1/sigmaSq0 + 
                                                                 1/disp)
                          
                                         
                                         var <- 1/(1/(sigmaSq0) + 
                                                   1/(sigmaSq_etahat + 
                                                        var_eta_given_y))
                                         
                                         w1 <- var/sigmaSq0
                                         w2 <- var/(sigmaSq_etahat + 
                                                      var_eta_given_y)
                                         eta_sampled <- rnorm(length(my_preds))*
                                           sqrt(var + 
                                                  2*tanh(rnorm(1, 
                                                               post_rho_mean, 
                                                               post_rho_sd))*
                                                  w1*
                                                  w2*
                                                  sqrt(sigmaSq0)*
                                                  sqrt((sigmaSq_etahat + 
                                                          var_eta_given_y))
                                           ) + 
                                           (my_preds / (sigmaSq_etahat + 
                                                          var_eta_given_y) +
                                              vegas_preds_spread / (sigmaSq0)) * 
                                           var
                                         y <- 
                                           rnorm(length(my_preds))*sqrt(disp) + 
                                           eta_sampled
                                         y
                                       }
cum_probs_spread_raw <- t(sapply(1:length(my_preds),function(k){
  c(mean(posterior_predictive_spread[,k] > vegas_preds_spread[k] 
         ),
    mean(posterior_predictive_spread[,k] < vegas_preds_spread[k]
         ))
}))
cum_probs_homewin_raw <- sapply(1:length(my_preds),function(k){
  mean(posterior_predictive_spread[,k] > 0)
})
prior_mu_logit_raw <- vegas_preds_spread
post_map_raw <- apply(posterior_predictive_spread,2,mean)
post_sd_raw <- apply(posterior_predictive_spread,2,sd)
p_win_raw <- apply(cum_probs_spread_raw,1,max)
p_lose_raw <- apply(cum_probs_spread_raw,1,min)
betz <- apply(cum_probs_spread_raw,
              1,function(x)if(
                x[1] > x[2]) "favor home team" else "favor away team")
bet_raw <- ifelse((0.909*p_win_raw - p_lose_raw) > cutoff,
              betz,
              "don't bet")


################################################################################
## ## Arcsin Square-Root proportion of points scored by home team
################################################################################

load('stacked_model_arcsin.RData')
my_preds <- as.numeric((predict2(stacked_model,
                                 use_randint = T,
                                 switch_z_sign = T)))
my_preds_expanded <- rep(my_preds, each = 81)
hometeam_preds_expanded_raw <- (rep(my_preds_total, each = 81) + 
                                vegas_preds_spread)/2
vegas_preds_arcsin <- c(sapply(1:length(hometeam_preds_expanded_raw),
                               function(j){
  asin(sqrt((hometeam_preds_expanded_raw[j]+3/8)/(my_preds_total[
    (j-1) %/% 81 + 1] + 3/4)))
}))


## code for obtaining the constant term
# load('stacked_model_log.RData')
# pr_tot <- predict(stacked_model, type = 'numeric')
# act_tot <- as.numeric(response(stacked_model))
# load('stacked_model_arcsin.RData')
# pr <- predict(stacked_model)
# pr_arc  <- pr
# act_arc <- response(stacked_model)
# z <- 2*sqrt(pr_tot)*pr_arc - 2*sqrt(act_tot)*act_arc
# sd(z)
# # [1] 1.182951
# var(z) - 1
# # [1] 0.3993731
# qqnorm((z-mean(z))/sd(z))
# abline(0,1)


const <- 0.3993731*0.25/rep(my_preds_total, each = 81)
# > mean(total_actual) - from final look 2023, same script as sigmaSq0s
# [1] 230.5397
# we correct for the fact that this prior is computed from average only in 2023,
# so we adjust for the predicted total of each game instead
sigmaSq0s <- 0.02869278^2*(rep(my_preds_total, each = 81)/230.5397) - 
             const

## = total variance - inherent variance = vegas variance
rm(stacked_model)
load('stacked_stan_arcsin.RData')
dr <- stacked_stan$draws()
posterior_draws <- 
  cbind(c(as.data.frame(dr[,,2])[-c(1:2000),1],
          as.data.frame(dr[,,2])[-c(1:2000),2]),
        c(as.data.frame(dr[,,3])[-c(1:2000),1],
          as.data.frame(dr[,,3])[-c(1:2000),1]))
# posterior_draws <- cbind(c(stacked_stan@sim$samples[[1]]$dispersion[-c(1:2000)],
#                       stacked_stan@sim$samples[[2]]$dispersion[-c(1:2000)]),
#                     c(stacked_stan@sim$samples[[1]]$sigmaSq_etahat[-c(1:2000)],
#                       stacked_stan@sim$samples[[2]]$sigmaSq_etahat[-c(1:2000)]))
#my_preds_og <- my_preds
rho <- 0.7996677
post_rho_mean <- (rho*(629-3) + rho_prior_mean/rho_prior_var)*post_rho_sd^2
posterior_predictive_arcsin <- foreach(i = 1:nrow(posterior_draws),
                                .combine = 'rbind') %do% {
  disp <- posterior_draws[i,1]
  sigmaSq_etahat <- posterior_draws[i,2] 
  var_eta_given_y <- 1/(1/sigmaSq0s + 1/disp) 
  var <- 1/(1/(sigmaSq0s) + 1/(sigmaSq_etahat+var_eta_given_y))
  w1 <- var/sigmaSq0s
  w2 <- var/(sigmaSq_etahat + var_eta_given_y)
  eta_sampled <- rnorm(length(my_preds_expanded))*
    sqrt(var + 
           2*tanh(rnorm(1, 
                        post_rho_mean, 
                        post_rho_sd))*
           w1*
           w2*
           sqrt(sigmaSq0s)*
           sqrt((sigmaSq_etahat + var_eta_given_y))
  ) + 
    (my_preds_expanded / (sigmaSq_etahat + var_eta_given_y) + 
       ## this is the posterior distribution sampled of theta
     vegas_preds_arcsin / (sigmaSq0s)) * var 
  eta_sampled
}

################################################################################
## ## Get Posterior Distribution of Spread 
################################################################################

half_linear_variance <- 0.25*(avg_time/48)^2*linear_sigma^2
spread_draws <- list()
for(k in 1:length(my_preds_expanded)){
    total <- rep(my_preds_total, each = 81)[k]
    prop_home <- sample(posterior_predictive_arcsin[,k],
                        25000,
                        replace = (length(posterior_predictive_arcsin[,k]) <
                                    25000))
    
    ## our "posterior" draw, if tau/sigma fixed, overtime ignored, 
    ## and uniform priors used
    ## and predictions assumed independent (so this is a psuedo-draw)
    ## this will be optimistic, 
    ## but will stabilize the draws of proportion, 
    ## won't affect the mean of such draws,
    ## and is better than simply plugging-in prediction
    total_draw <- rnorm(length(total), 
                        total, 
                        sqrt(0.25*total*tau^2 + # var quassi-poisson model
                             half_linear_variance)) # var of linear model 
    
    ## generate normal from the posterior mean of the 
    ## arcsin-sqrt transform with var ~ 1/(4*total)
    y <- rnorm(25000,prop_home,0.5/sqrt(abs(total_draw)))
    
    ## transform back to get an estimate of the home proportion of points scored
    home_pts <- (sin(y))^2*(total + 3/4) - 3/8
    away_pts <- total-home_pts
    spread_draws[[k]] <- home_pts - away_pts
}
cum_probs_spread <- t(sapply(1:length(my_preds_expanded),function(k){
  c(mean(spread_draws[[k]] > vegas_preds_spread[k]),
    mean(spread_draws[[k]] <  vegas_preds_spread[k]))
}))
cum_probs_homewin <- sapply(1:length(my_preds_expanded),function(k){
  mean(spread_draws[[k]] > 0)
})
prior_mu_logit <- vegas_preds_spread
post_map_arcsin <- apply(posterior_predictive_arcsin,2,mean)
post_sd_arcsin <- apply(posterior_predictive_arcsin,2,sd)
p_win <- apply(cum_probs_spread,1,max)
p_lose <- apply(cum_probs_spread,1,min)
betz <- apply(cum_probs_spread,
              1,function(x)if(
                x[1] > x[2]) "favor home team" else "favor away team")
bet_arcsin <- ifelse((0.909*p_win - p_lose) > cutoff,
              betz,
              "don't bet")

################################################################################
## ## More bet post-processing
################################################################################

spread_bet <- ifelse((bet_arcsin == bet_raw) & bet_arcsin != "don't bet",
              bet_arcsin,
              "don't bet")
arcsinscale <- cbind(
  rep(as.character(new_gl_merged$dateGame.x),each = 81),
  cum_probs_spread,
  vegas_preds_arcsin,
  my_preds_expanded,
  post_map_arcsin,
  post_sd_arcsin,
  .909*p_win - 1*p_lose,
  bet_arcsin,
  as.character(Sys.time()),
  final_overunder$HomeTeam,
  final_overunder$AwayTeam)
rawscale <- cbind(
  rep(as.character(new_gl_merged$dateGame.x),each = 81),
  cum_probs_spread_raw,
  vegas_preds_spread,
  my_preds_raw,
  post_map_raw,
  post_sd_raw,
  .909*p_win_raw - 1*p_lose_raw,
  bet_raw,
  as.character(Sys.time()),
  final_overunder$HomeTeam,
  final_overunder$AwayTeam)
colnames(arcsinscale) <- c("Date",
                          "ProbAwayTeamWinsSpread",
                          "ProbHomeTeamWinsSpread",
                          "Prior",
                          "Predicted",
                          "PostMean",
                          "PostSD",
                          "ExpectedProfit",
                          "Bet",
                          "DateBet",
                          "HomeTeam",
                          "AwayTeam")
colnames(rawscale) <- colnames(arcsinscale)

# # ## Append predictions and decision rules 
# past_data <- read.csv("history_of_bets2024_arcsin.csv",
#                       stringsAsFactors = F)
# colnames(past_data) <- colnames(arcsinscale)
# write.csv(unique(rbind(past_data,arcsinscale)),
#           file = "history_of_bets2023_arcsin.csv",
#           row.names = F)
# 
# past_data <- read.csv("history_of_bets2024_spread.csv",
#                       stringsAsFactors = F)
# colnames(past_data) <- colnames(rawscale)
# write.csv(unique(rbind(past_data,rawscale)),
#           file = "history_of_bets2024_spread.csv",
#           row.names = F)
cat("\n\n-----------------------------------------\n\n")

################################################################################
## ## Moneyline Bets 
################################################################################


## Probability of winning, based on the posterior-predictive of the spread
home_team_prob_win <- sapply(1:ncol(posterior_predictive_spread),
                             function(k){
                               x <- posterior_predictive_spread[,k]
                               mean( x > 0.5)
                             })
away_team_prob_win <- sapply(1:ncol(posterior_predictive_spread),
                             function(k){
                               x <- posterior_predictive_spread[,k]
                               mean( x < 0.5)
                             })

## Determine if bets are worth it, based on the odds given and the spread
moneyline_bets_spread <- foreach(i = 1:length(home_team_prob_win), 
        .combine = 'rbind') %do% {
  cutoff_home_team <- uniroot(function(x){
    if(x < 0){
      y <- abs(100/x)
    } else{
      y <- x / 100
    }
    home_team_prob_win[i]*y - 1
  },
  interval = c(-2500000,2500000))[[1]]
  cutoff_away_team <- uniroot(function(x){
    if(x < 0){
      y <- abs(100/x)
    } else{
      y <- x / 100
    }
    away_team_prob_win[i]*y - 1
  },
  interval = c(-2500000,2500000))[[1]]
  ## if the odds are greater than this, we bet
  return(c(cutoff_home_team, cutoff_away_team))
}


## Repeat for the posterior-predictive of the asin-square root transformed data
home_team_prob_win <- sapply(1:ncol(posterior_predictive_arcsin),
                             function(k){
                               x <- spread_draws[[k]]
                               mean( x > 0.5)
                             })
away_team_prob_win <- sapply(1:ncol(posterior_predictive_arcsin),
                             function(k){
                               x <- spread_draws[[k]]
                               mean( x < 0.5)
                             })
moneyline_bets_arcsin <- foreach(i = 1:length(home_team_prob_win), 
                                 .combine = 'rbind') %do% {
                                   cutoff_home_team <- uniroot(function(x){
                                     if(x < 0){
                                       y <- abs(100/x)
                                     } else{
                                       y <- x / 100
                                     }
                                     home_team_prob_win[i]*y - 1
                                   },
                                   interval = c(-2500000,2500000))[[1]]
                                   cutoff_away_team <- uniroot(function(x){
                                     if(x < 0){
                                       y <- abs(100/x)
                                     } else{
                                       y <- x / 100
                                     }
                                     away_team_prob_win[i]*y - 1
                                   },
                                   interval = c(-2500000,2500000))[[1]]
                                   ## if the odds are greater than this, we bet
                                   return(c(cutoff_home_team, cutoff_away_team))
                                 }

moneyline_bets <- sapply(1:2,function(j){
  sapply(1:nrow(moneyline_bets_arcsin),function(i){
    round(max(moneyline_bets_arcsin[i,j],
        moneyline_bets_spread[i,j]))
  })
})
colnames(moneyline_bets) <- c("Minimum Odds Home", "Minimum Odds Away")


################################################################################
## ## Cleaning
################################################################################

## final posterior is the unweighted average of log and pts-per-48 
post_pts <- 0.5*as.numeric(final_overunder$`Posterior Log`) + 
            0.5*as.numeric(final_overunder$`Posterior Abs`)

## moneyline results
moneyline_res <- cbind(matchups_enter[rep(1:nrow(matchups_enter),each = 81),
                                      1:2],
                       vegas_preds_spread,
                       home_team_prob_win,
                       away_team_prob_win,
                       moneyline_bets)

## spread results
spread_res <- cbind(matchups_enter[rep(1:nrow(matchups_enter),each = 81),
                                   1:2],
                    rawscale[,c('Prior')],
                    rawscale[,c('PostMean')],
                    spread_bet)

## over under results
ou_res <- cbind(matchups_enter[rep(1:nrow(matchups_enter),each = 81),
                               1:2],
                final_overunder$`Over/Under`,
                round(post_pts,2),
                c(final_bet))


colnames(moneyline_res)<- c("AwayTeam",
                            "HomeTeam",
                            "VegasSpread",
                            "PredictedProbHomeTeamWins",
                            "PredictedProbAwayTeamWins",
                            "MinimumOddsForBettingOnHome",
                            "MinumumOddsForBettingOnAway")
colnames(ou_res) <- c("AwayTeam",
                      "HomeTeam",
                      "VegasOverUnder",
                      "PredictedTotalPointsScored",
                      "BetOverOrUnder?")
colnames(spread_res) <- c("AwayTeam",
                          "HomeTeam",
                          "VegasSpread",
                          "PredictedSpread",
                          "BetAwayOrHomeTeamOnSpread?")

## Post-hoc fix: for gaps/inconsistencies in over under bets
## Because posterior-decision rules are approximations, sometimes we have 
## alternating pro bets/anti bets: take the mot conservative approach here,
## replacing any pro bets less extreme than the anti bet as another anti bet
for(i in 2:(nrow(ou_res)-1)){
  ## if entry above and below bet, you bet: 
  ## if entry above and below don't bet, don't bet
  if( (ou_res[i, ncol(ou_res)] != ou_res[i+1, ncol(ou_res)]) &
      (ou_res[i-1, ncol(ou_res)] == ou_res[i+1, ncol(ou_res)])){
    ou_res[i, ncol(ou_res)] <-  ou_res[i+1, ncol(ou_res)]
  }
}
## fix it: for gaps in spread bets
for(i in 2:(nrow(spread_res)-1)){
  ## if entry above and below bet, you bet: 
  ## if entry above and below don't bet, don't bet
  if( (spread_res[i, ncol(spread_res)] != spread_res[i+1, ncol(spread_res)]) &
      (spread_res[i-1, ncol(spread_res)] == spread_res[i+1, ncol(spread_res)])){
    spread_res[i, ncol(spread_res)] <-  spread_res[i+1, ncol(spread_res)]
  }
}

################################################################################
## Write results to ccv files
################################################################################

write.csv(moneyline_res,file = "MoneyLineBets.csv",row.names = F)
write.csv(spread_res,file = "SpreadBets.csv",row.names = F)
write.csv(ou_res,file = "OverUnderBets.csv", row.names = F)
nd <- Sys.time()
print(nd - strt)


}

