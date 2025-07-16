
################################################################################
## ## Setup
################################################################################

rm(list = ls())
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
library(MachineShop)
library(recipes)
library(foreach)
library(doParallel)
library(dplyr)
library(purrr)
library(data.table)
library(doSNOW)
library(magrittr)
library(R.utils)
library(MASS)
library(nbastatR)


## how many games do we need previously before we predict an outcome
lag_og <- 5
options(curl_interrupt = TRUE)

################################################################################
## ## Pull Data
################################################################################

#load('bs_playoffs.RData')
lag <- lag_og
years <- 2014:2025

## playoff games
# gl <- game_logs(years[years != 2023], result_types = 'team',
#                 season_types = c("Playoffs"),
#                 assign_to_environment = FALSE) %>%
#   mutate(outcome = as.numeric(as.factor(outcomeGame))-1)
# gl_playoffs <- gl[order(gl$dateGame),]

## regular season games
gl <- game_logs(years, result_types = 'team',
                season_types = c("Regular Season"),
                assign_to_environment = FALSE) %>%
  mutate(outcome = as.numeric(as.factor(outcomeGame))-1)
gl_reg <- gl[order(gl$dateGame),]
#gl <- rbind(gl_playoffs,gl_reg)

## keep regular season only for now
gl <- rbind(gl_reg)
gl <- gl[order(gl$dateGame),]
gl <- gl[!is.na(gl$outcomeGame),]
table(gl$typeSeason)


load('bs.RData')
bs <- rbind(bs)

ids <- unique(gl$idGame)
start <- Sys.time()
ids <- ids[!(ids %in% bs$idGame)]
ids <- ids[as.character(ids) != "21201214"] %>% rev

################################################################################
## Gather all data, combine into one dataframe
## take weighted averages, per 40 min, etc.
gl$is_playoff_game <- as.numeric(gl$typeSeason == "Playoffs")

##
vars <- colnames(gl)[grepl("Team",colnames(gl))]
vars <- c(na.omit(vars[9:31]))
vars <- vars[!(vars %in% c("plusminusTeam",
                           "minutesTeam",
                           "outcome"))]
vars <- unique(c(vars))
nonpermin_vars <- c(c(vars[grep("pct",vars)]),
                    "outcome",
                    "plusminusTeam")
non_lag_vars <- c("isB2BSecond",
                  "isB2BFirst",
                  "countDaysRestTeam",
                  "numberGameTeamSeason",
                  "ptsTeamNextGame")
vars <- vars[-grep("pct",vars)]


##
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
                       'orebChances', 'drebChances', 
                       'trebChances', 'touches', 'astSecondary', 
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


## For each year, each team, build up a long-form dataframe 
## Each entry produces 
##' 1) outcome information for the next game
##' 2) stats for the last game
##' 3) a weighted average of stats for past games

## Fix bug of some weird NAs, 
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
##
gl$logpts <- log(gl$ptsTeam)
gl$sqrtpts <- 2*sqrt(gl$ptsTeam + 3/8)
cl <- makeCluster(round(length(years)))
registerDoParallel(cl)
clusterExport(cl,ls())
new_gl <- foreach(year = years, 
                  .combine = 'rbind',
                  .packages = c('MachineShop',
                                'recipes',
                                'foreach',
                                'doParallel',
                                'dplyr',
                                'purrr',
                                'data.table',
                                'doSNOW',
                                'magrittr',
                                'R.utils',
                                'MASS',
                                'nbastatR')) %dopar% {
  cat(c("\n\t", year))
  temp_for_parallel <- data.frame()
  new_gl_temp <- data.frame()
  for(team in unique(gl$nameTeam)){
    print(paste(grep(team,teams),length(teams),sep = " / "))
    data <- gl %>%
      subset((nameTeam %in% team) & (yearSeason %in% year)) %>%
      as.data.frame()
    data <- data[order(as.Date(data$dateGame),decreasing = F),]
    if(nrow(data) > 0){
      
      ## record the next games outcome for training
      data$XXptsTeamNextGame <- as.numeric(c(data$ptsTeam[c(2:nrow(data))],NA))
      data <- scale_per_minute(data,vars)
      data$nextOutcome <- c(data$outcome[c(2:nrow(data))],NA)
      data$nextGameID <- c(data$idGame[c(2:nrow(data))],NA)
      data$locationNextGame <- c(data$locationGame[c(2:nrow(data))],NA)
      data$ptsTeamNextGame <- data$XXptsTeamNextGame
      data$minutesNextGame <- c(data$minutes[c(2:nrow(data))],NA)
      
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
      colnames(bs_temp_opp)[grep('idGame_opp',colnames(bs_temp_opp))] <- 
                                                                  'idGame'
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
      #dataLG2 <- data[,colnames(data) %in% average_vars]
      for(i in 1:nrow(data)){
        for(var in c(
             average_vars[average_vars %in% colnames(data)],
             paste0(average_vars,'_opp')[paste0(average_vars,'_opp') %in% 
                                         colnames(data)])){
          
          ## weighted average over all previous games
          w <- (sapply((1:max(i-1,1)),function(x)1/x))
          datatemp[i,var] <- weighted.mean(
            as.numeric(unlist(data[max(i-1,1):1,var])),
            w,
            na.rm = TRUE)
          
          ## drop-off of last available game data versus weighted average
          dataLG[i,var] <- as.numeric(na.omit(unlist(data[i:1,var])))[1] -
                           datatemp[i,var]
        }
      }
      colnames(dataLG) <- paste0(colnames(dataLG),
                                 "LastGame")
      data <- bind_cols(datatemp,dataLG)
      data$year <- as.numeric(year)
      data$team <- team
      temp_for_parallel <- bind_rows(temp_for_parallel,data)
    }
  }
  temp_for_parallel
}
end <- Sys.time()
start-end
stopCluster(cl)


################################################################################
## ## Choosing and cleaning features for modelling
################################################################################

## Vars to use
cols <- 
  c(paste(colnames(data),'.x'),
    paste0(colnames(data),'.y'))

## Merge home team/away team data
new_gl_home <- new_gl[!is.na(new_gl$nextGameID) & 
                        new_gl$locationNextGame == "H",]
new_gl_away <- new_gl[!is.na(new_gl$nextGameID) & 
                        new_gl$locationNextGame == "A",]
new_gl_merged <- merge(new_gl_home,new_gl_away,by = "nextGameID")
new_gl_merged$idGame.x <- NULL
new_gl_merged$idGame.y <- NULL
new_gl_merged$nextOutcome <- new_gl_merged$nextOutcome.x
new_gl_merged$nextOutcome.x = new_gl_merged$nextOutcome.y = NULL


## clean
new_gl_merged$isB2BFirst.x <- as.numeric(factor(new_gl_merged$isB2BFirst.x))-1
new_gl_merged$isB2BSecond.x <- as.numeric(factor(new_gl_merged$isB2BFirst.x))-1
new_gl_merged$isB2BFirst.y <- as.numeric(factor(new_gl_merged$isB2BFirst.y))-1
new_gl_merged$isB2BSecond.y <- as.numeric(factor(new_gl_merged$isB2BFirst.y))-1
nextOutcome <- cbind(new_gl_merged$nextOutcome)
rownames(new_gl_merged) <- paste0("a_",rownames(new_gl_merged))
rownames(nextOutcome) <- rownames(new_gl_merged)

new_gl_merged$ptsGame <- new_gl_merged$ptsTeamNextGame.x +
  new_gl_merged$ptsTeamNextGame.y
new_gl_merged <- new_gl_merged[new_gl_merged$ptsGame > 125,]

################################################################################
## ## Construct outcomes of interest for modelling
################################################################################

## spread difference of square roots
new_gl_merged$spreadDiff <- 
  new_gl_merged$ptsTeamNextGame.x - 
  new_gl_merged$ptsTeamNextGame.y

## overtime periods
new_gl_merged$overtime <- 
  factor(new_gl_merged$minutesNextGame.x,
  levels = sort(unique(new_gl_merged$minutesNextGame.x)))
new_gl_merged$overtime[
  as.numeric(as.character(new_gl_merged$overtime)) > 290] <- 290
new_gl_merged$overtime <- factor(as.character(new_gl_merged$overtime),
                                 levels  = unique(sort(new_gl_merged$overtime)))

## log pts game
new_gl_merged$logPtsGame <- log(new_gl_merged$ptsTeamNextGame.x +
                                new_gl_merged$ptsTeamNextGame.y)

## logit proportion of points scored by home team
propPtsHomeTeam <- new_gl_merged$ptsTeamNextGame.x/(
  new_gl_merged$ptsTeamNextGame.x + new_gl_merged$ptsTeamNextGame.y)
new_gl_merged$logitPropPts <- log(propPtsHomeTeam/(1-propPtsHomeTeam))

## arcsin-sqrt proportion of points scored by home team
 propPtsHomeTeam <- new_gl_merged$ptsTeamNextGame.x/(
   new_gl_merged$ptsTeamNextGame.x + new_gl_merged$ptsTeamNextGame.y)
 new_gl_merged$arcsinsqrt_prop <- asin(sqrt(
   (new_gl_merged$ptsTeamNextGame.x + 3/8)/
   (new_gl_merged$ptsTeamNextGame.x + 
    new_gl_merged$ptsTeamNextGame.y +
    3/4)
 ))
   

new_gl_merged$ptsGame <- 240 * (new_gl_merged$ptsTeamNextGame.x +
                                new_gl_merged$ptsTeamNextGame.y)/
                                new_gl_merged$minutesNextGame.x

################################################################################
## ## Merge the final data together
################################################################################


rownames(new_gl_merged) <- paste(1:nrow(new_gl_merged),
                                 (paste(paste(new_gl_merged$slugTeam.x.y,
                                              sep = "_"),
                                        paste(new_gl_merged$slugTeam.x.x,
                                              sapply(new_gl_merged$nextGameID, 
                                                     function(id)paste0(
                                                       as.Date(
                                                         max(
                                                         as.Date(
                                                           gl$dateGame[
                                                             gl$idGame == id]
                                                           ))))),
                                              sep = "_"),
                                        sep = "at")),
                                 sep = ":")
exclude_cols <- c(
          'ptsTeam.x',
          'ptsTeam.y',
          'ptsTeamLastGame.x',
          'ptsTeamLastGame.y',
          'cityTeam.x',
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
          colnames(new_gl_merged)[grep('idTeam',colnames(new_gl_merged))],
          colnames(new_gl_merged)[grep('urlTeam',colnames(new_gl_merged))],
          colnames(new_gl_merged)[grep('teamName',colnames(new_gl_merged))],
          colnames(new_gl_merged)[grep('slug',colnames(new_gl_merged))],
          colnames(new_gl_merged)[grep('outcomeGame',colnames(new_gl_merged))],
          colnames(new_gl_merged)[grep('video',colnames(new_gl_merged))],
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

## Start constructing our final data set for analysis
df <- new_gl_merged[,!(colnames(new_gl_merged) %in% exclude_cols)]
for(i in 1:nrow(df)){
  away_team <- unique(gl$slugTeam[gl$idGame == new_gl_merged$nextGameID[i] & 
                                  gl$locationGame == "A"])
  home_team <- unique(gl$slugTeam[gl$idGame == new_gl_merged$nextGameID[i] & 
                                    gl$locationGame == "H"])
  date <- unique(gl$dateGame[gl$idGame == new_gl_merged$nextGameID[i]])
  rownames(df)[i] <- paste0(i,":",away_team,"at",home_team,"_",date)
}
df <- na.omit(df)
rownames <- rownames(df)

## Final merge, removing NAs
df <- df %>%
  apply(2,as.numeric) %>%
  as.data.frame
rownames(df) <- rownames
if(dim(df)[1] < dim(df)[2]){
  df <- df %>% t %>% as.data.frame
}
df$ptsTeamNextGame.x = df$ptsTeamNextGame.y = NULL
df <- df[df$logpts.x > -Inf & df$logpts.y > -Inf,]
if(dim(df)[1] < dim(df)[2]){
  df <- df %>% t %>% as.data.frame
}
df$ptsTeamNextGame.x = df$ptsTeamNextGame.y = NULL
df$minutesNextGame.x = NULL


## Cleaning
df$isB2BFirst.x <- as.numeric(df$isB2BFirst.x)
df$isB2BSecond.x <- as.numeric(df$isB2BFirst.x)
df$isB2BFirst.y <- as.numeric(df$isB2BFirst.y)
df$isB2BSecond.y <- as.numeric(df$isB2BSecond.y)
df$nextOutcome <- NULL
df$pctUSG.x = df$pctUSG.y = NULL
df$yearSeason.x <- NULL

## Can't do inside the loop
## Felates arcsin-square root proportion of games won, weighted for more recent
## games
df$outcome.x <- asin(sqrt(df$outcome.x))
df$outcome.y <- asin(sqrt(df$outcome.y))
df$outcomeOpp.x <- asin(sqrt(df$outcomeOpp.x))
df$outcomeOpp.y <- asin(sqrt(df$outcomeOpp.y))

## these variables are the outcomes of interest we may model from this data
outcome_vars <- c('logPtsGame',
                  'ptsGame',
                  'spreadDiff',
                  'overtime',
                  'arcsinsqrt_prop',
                  'logitPropPts')

## remove near-linearly dependent columns, variables with near-0 variance
df <- df[,(apply(df,2,function(x)mean(is.na(x))) == 0)]
df <- df[,(apply(df,2,function(x)length(unique(x)) > 1))]

## remove duplicates of variables (shared by box score, game logs etc.) and 
## other, further undesired columns
df$year.y <- NULL
df$yearSeason.y <- NULL
df$minutes.x.x <- NULL
df$minutes.x.y <- NULL
df$minutes.y.x <- NULL
df$minutes.y.y <- NULL
df$logpts.x <- NULL
df$logpts.y <- NULL
df$logptsLastGame.x <- NULL
df$logptsLastGame.y <- NULL
df$sqrtpts.x <- NULL
df$sqrtpts.y <- NULL
df$sqrtptsLastGame.x <- NULL
df$sqrtptsLastGame.y <- NULL
df$blk.x <- NULL
df$blk.y <- NULL
df$blk_opp.x <- NULL
df$blk_opp.y <- NULL
df$blk_oppLastGame.x <- NULL
df$blk_oppLastGame.y <- NULL
df$pf.x <- NULL
df$pf.y <- NULL
df$pf_opp.x <- NULL
df$pf_opp.y <- NULL
df$pf_oppLastGame.x <- NULL
df$pf_oppLastGame.y <- NULL
df$ast.x <- NULL
df$ast.y <- NULL
df$ast_opp.x <- NULL
df$ast_opp.y <- NULL
df$ast_oppLastGame.x <- NULL
df$ast_oppLastGame.y <- NULL
df$minutes_opp.x <- NULL
df$minutes_opp.y <- NULL
df$minutes_oppLastGame.x <- NULL
df$minutes_oppLastGame.y <- NULL
df$pfLastGame.x <- NULL
df$pfLastGame.y <- NULL

## Filter where passes per min > 0: sorts many missing variables from 38 games
df <- df[df$passesPerMinute.x != 0,]
df <- df[!(df$numberGameTeamSeason.y <= lag_og),]

## Filter out predictors with near-0 variance, or highly correlated
newdat <- recipe(logPtsGame ~ .,
                 data = df[,-sapply(outcome_vars[-1],
                                    function(x)grep(x,colnames(df)))]) %>%
  step_corr(all_numeric_predictors(),
            threshold = 0.95) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_numeric_predictors()) %>%
  prep %>%
  juice
miss_cols <- cbind(colnames(df)[!(colnames(df) %in% colnames(newdat))])

## I want these variables, even if they are filtered from the correlation filter
keep_cols <- unique(c(colnames(newdat),
                miss_cols[grep('netrtg',miss_cols)],
               'arcsinsqrt_pts.x',
               'arcsinsqrt_pts.y',
'ptsTeamPerMinute.x',
'ptsTeamPerMinuteLastGame.x',
'ptsTeamPerMinute.y',
'ptsTeamPerMinuteLastGame.y',
'ptsTeamOpp.x',
'ptsTeamOppLastGame.x',
'ptsTeamOpp.y',
'ptsTeamOppLastGame.y',
'logPtsTeam.x',
'logPtsTeamLastGame.x',
'logPtsTeamOpp.x',
'logPtsTeamOppLastGame.x',
'logPtsTeam.y',
'logPtsTeamLastGame.y',
'logPtsTeamOpp.x',
'logPtsTeamOpp.y',
'logPtsTeamOppLastGame.y',
'pctFG3Team.x',
'pctFG3Team.y',
'pctFG3TeamLastGame.x',
'pctFG3TeamLastGame.y',
'fgaTeamPerMinute.x',
'fgaTeamPerMinute.y',
'fgaTeamPerMinuteLastGame.x',
'fgaTeamPerMinuteLastGame.y',
'blkaPerMinute.x',
'blkaPerMinute.y',
'blkaPerMinuteLastGame.x',
'blkaPerMinuteLastGame.y',
'netrtgELastGame.y',
'netrtgELastGame.x',
'netrtgE.y',
'netrtgE.x',
'blkaPerMinute.x',
'blkaPerMinute.y',
'blkaPerMinuteLastGame.x',
'blkaPerMinuteLastGame.y',
'possessionsPerMinute_opp.x',
'possessionsPerMinute_opp.y',
'possessionsPerMinute_oppLastGame.x',
'possessionsPerMinute_oppLastGame.y',
'possessionsPerMinute.x',
'possessionsPerMinute.y',
'possessionsPerMinuteLastGame.x',
'possessionsPerMinuteLastGame.y',
                outcome_vars))
save(keep_cols,file = 'keep_cols.RData')
load('keep_cols.RData')
df <- df[,colnames(df) %in% keep_cols]
years2keep <-
  c(
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
    '2024'#,
    #'2025' # baseline
  )
yrz <- foreach(year = years2keep,
               .combine = 'cbind') %do% {
                 1*(df$year.x == year)
               }
colnames(yrz) <- paste0("year_",years2keep)
df <- bind_cols(df,yrz)

## final clean
df <- df[,(apply(df,2,function(x)mean(is.na(x))) == 0)]
df <- df[,(apply(df,2,function(x)length(unique(x)) > 1))]
save(df,outcome_vars,file = "C:\\Users\\defgi\\Documents\\AbsolutelyStackedSupplementaryFiles\\recipes.RData")
