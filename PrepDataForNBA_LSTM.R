## Run this script for preparing/cleaning data for modelling purposes
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
lag_og <- 5
options(curl_interrupt = TRUE)
convert_arcsinsqrt_to_my_vst <- function(arcsinsqrtp){
  p <- sin(arcsinsqrtp)^2
  sqrt(p*(1-p))*log(p/(1-p))
}
my_vst <- function(p){
  sqrt(p*(1-p))*log(p/(1-p))
}
################################################################################ 

load('bs_playoffs.RData')
lag <- lag_og
years <- 2010:2023
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

## 
#gl <- rbind(gl_playoffs,gl_reg)
## keep regular season only for rigor
gl <- rbind(gl_reg)
gl <- gl[order(gl$dateGame),]
gl <- gl[!is.na(gl$outcomeGame),]
table(gl$typeSeason)

##
bs_playoff <- bs
load('bs.RData')
bs_reg <- bs
#bs <- rbind(bs_reg,bs_playoff)
bs <- rbind(bs_reg)

ids <- unique(gl$idGame)
start <- Sys.time()
ids <- ids[!(ids %in% bs$idGame)]
ids <- ids[as.character(ids) != "21201214"] %>% rev
################################################################################
## Gather all data, combine into one dataframe
## take weighted averages, per 40 min, etc.

#gl$before_or_after_2017 <- as.numeric(gl$yearSeason >= 2017)
#gl$before_or_after_2022 <- as.numeric(gl$yearSeason >= 2022)
gl$is_playoff_game <- as.numeric(gl$typeSeason == "Playoffs")

##
vars <- colnames(gl)[grepl("Team",colnames(gl))]
vars <- c(na.omit(vars[9:31]))
vars <- vars[!(vars %in% c("plusminusTeam",
                           "minutesTeam",
                           "outcome"))]
vars <- unique(c(vars))
nonpermin_vars <- c(c(vars[grep("pct",vars)]),"outcome","plusminusTeam")
non_lag_vars <- c("isB2BSecond","isB2BFirst","countDaysRestTeam","numberGameTeamSeason",
                  #"before_or_after_2017",
                  #"before_or_after_2022",
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
                       'orebChances', 'drebChances', 'trebChances', 'touches', 'astSecondary', 
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

################################################################################

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
cl <- makeCluster(length(years)/2)
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
      ## add-in for special vst
      # gl_temp$arcsinsqrt_pts <- 2*gl_temp$arcsinsqrt_pts*sqrt((gl_temp$ptsTeam + 
      #                                                          gl_temp_opp$ptsTeam))
      ##
      gl_temp$ptsTeamOpp <-    gl_temp_opp$ptsTeam*240/gl_temp_opp$minutesTeam
      gl_temp$logPtsTeamOpp <- log(gl_temp_opp$ptsTeam*240/gl_temp_opp$minutesTeam)
      gl_temp$sqrtPtsTeamOpp <- 2*sqrt(gl_temp_opp$ptsTeam*240/gl_temp_opp$minutesTeam + 3/8)
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
      datatemp <-data[,!(colnames(data) %in% average_vars)]
      dataGRU <- data[,colnames(data) %in% average_vars]
      for(i in 1:nrow(data)){
        for(var in c(average_vars[average_vars %in% colnames(data)],
                     paste0(average_vars,'_opp')[paste0(average_vars,'_opp') %in% colnames(data)])){
          ## last game
          dataGRU[i,var] <- (na.omit(unlist(data[i:1,var])))[1]
        }
      }
      colnames(dataGRU) <- paste0(colnames(dataGRU),
                                  "_GRU")
      data <- 
        datatemp %>% 
        bind_cols(dataGRU)
      if(nrow(data) < lag){
        data <- data.frame()
      }
      if(nrow(data) >= lag){
        data$year <- as.numeric(year)
        data$team <- team
        temp_for_parallel <- bind_rows(temp_for_parallel,data)
      }
    }
  }
  temp_for_parallel
}
end <- Sys.time()
start-end
stopCluster(cl)


################################################################################
## Choosing and preparing raw covariates for modelling

## vars to use
cols <- 
  c(paste(colnames(data),'.x'),
    paste0(colnames(data),'.y'))

## merge home team/away team data
new_gl_home <- new_gl[!is.na(new_gl$nextGameID) & new_gl$locationNextGame == "H",]
new_gl_away <- new_gl[!is.na(new_gl$nextGameID) & new_gl$locationNextGame == "A",]
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
## Construct outcomes of interest for modelling

## spread difference of square roots
new_gl_merged$spreadDiff <- 
  new_gl_merged$ptsTeamNextGame.x - 
  new_gl_merged$ptsTeamNextGame.y
  # 2*sqrt(new_gl_merged$ptsTeamNextGame.x + 3/8) -
  # 2*sqrt(new_gl_merged$ptsTeamNextGame.y + 3/8)

## overtime is counted as 0 in spreadDiff land
new_gl_merged$spreadDiff <- ifelse(new_gl_merged$minutesNextGame.x > 240,
                                   0,
                                   new_gl_merged$spreadDiff)

## overtime periods
new_gl_merged$overtime <- factor(new_gl_merged$minutesNextGame.x,
                                 levels = sort(unique(new_gl_merged$minutesNextGame.x)))
new_gl_merged$overtime[as.numeric(as.character(new_gl_merged$overtime)) > 290] <-
  290
new_gl_merged$overtime <- factor(as.character(new_gl_merged$overtime),
                                 levels  = unique(sort(new_gl_merged$overtime)))

## log pts game
new_gl_merged$logPtsGame <- log(new_gl_merged$ptsTeamNextGame.x +
                                new_gl_merged$ptsTeamNextGame.y)

## pts game per 240 min
# new_gl_merged$ptsGame <- 240 * new_gl_merged$ptsGame/
#   new_gl_merged$minutesNextGame.x

## logit proportion of points scored by home team
propPtsHomeTeam <- new_gl_merged$ptsTeamNextGame.x/(new_gl_merged$ptsTeamNextGame.x + 
                                                    new_gl_merged$ptsTeamNextGame.y)
new_gl_merged$logitPropPts <- log(propPtsHomeTeam/(1-propPtsHomeTeam))

## arcsin-sqrt proportion of points scored by home team
 propPtsHomeTeam <- new_gl_merged$ptsTeamNextGame.x/(new_gl_merged$ptsTeamNextGame.x + new_gl_merged$ptsTeamNextGame.y)
 new_gl_merged$arcsinsqrt_prop <- asin(sqrt(
   (new_gl_merged$ptsTeamNextGame.x + 3/8)/
   (new_gl_merged$ptsTeamNextGame.x + 
    new_gl_merged$ptsTeamNextGame.y +
    3/4)
 ))
   
 ##
# edit: 
 # new_gl_merged$arcsinsqrt_prop <- 2 * 
 #   new_gl_merged$arcsinsqrt_prop * 
 #   sqrt((200))
 ##

 new_gl_merged$ptsGame <- 240 * (new_gl_merged$ptsTeamNextGame.x +
                                 new_gl_merged$ptsTeamNextGame.y)/
   new_gl_merged$minutesNextGame.x
 
 ## home and away team points per 48 minutes
 new_gl_merged$ptsScored_home <- #2*sqrt(
   24*new_gl_merged$ptsTeamNextGame.x/
                               new_gl_merged$minutesNextGame.x 
 #+ 3/8)
 new_gl_merged$ptsScored_away <- #2*sqrt(
   24*new_gl_merged$ptsTeamNextGame.y/
                               new_gl_merged$minutesNextGame.y# + 3/8)

###############################################################################
## Merge the final data together

rownames(new_gl_merged) <- paste(1:nrow(new_gl_merged),
                                 (paste(paste(new_gl_merged$slugTeam.x.y,
                                              sep = "_"),
                                        paste(new_gl_merged$slugTeam.x.x,
                                              new_gl_merged$dateGame.x,sep = "_"),
                                        sep = "at")),
                                 sep = ":")
#cols <- paste0(cols,)
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
                  c(paste0(c("pctDREB_GRU",
                             "pctOREB_GRU",
                             "pctTREB_GRU",
                             "pctDREB_GRU2",
                             "pctOREB_GRU2",
                             "pctTREB_GRU2"),
                           ".x"),
                    paste0(c("pctDREB_GRU",
                             "pctOREB_GRU",
                             "pctTREB_GRU",
                             "pctDREB_GRU2",
                             "pctOREB_GRU2",
                             "pctTREB_GRU2"),
                           ".y")),
                  c(paste0(c("pctDREB_GRU",
                             "pctOREB_GRU",
                             "pctTREB_GRU",
                             "pctDREB_GRU2",
                             "pctOREB_GRU2",
                             "pctTREB_GRU2"),
                           ".x"),
                    paste0(c("pctDREB_GRU",
                             "pctOREB_GRU",
                             "pctTREB_GRU",
                             "pctDREB_GRU2",
                             "pctOREB_GRU2",
                             "pctTREB_GRU2"),
                           ".y"),
                    paste0(c(  "pctDREB_opp_GRU",
                               "pctOREB_opp_GRU",
                               "pctTREB_opp_GRU",
                               "pctDREB_opp_GRU2",
                               "pctOREB_opp_GRU2",
                               "pctTREB_opp_GRU2"),
                           ".x"),
                    paste0(c("pctDREB_opp_GRU",
                             "pctOREB_opp_GRU",
                             "pctTREB_opp_GRU",
                             "pctDREB_opp_GRU2",
                             "pctOREB_opp_GRU2",
                             "pctTREB_opp_GRU2"),
                           ".y")),
                  c('countDaysNextGameTeam.x', 
                    'XXptsTeamNextGame.x', 
                    'countDaysNextGameTeam.y', 
                    'XXptsTeamNextGame.y', 
                    'minutesNextGame.y')
)
df <- new_gl_merged[,!(colnames(new_gl_merged) %in% exclude_cols)] 
top_vars <- unique(c(
  
  vars,
  box_vars,
  non_lag_vars,
  average_vars,
  scale_box_vars,
  nonpermin_vars,
  paste0(box_vars,'_opp'),
  paste0(average_vars,'_opp'),
  paste0(scale_box_vars,'_opp')
  
))
top_vars <- top_vars[sapply(top_vars,function(tv)any(grepl(tv,colnames(df))))]
top_vars_x <- c(sapply(top_vars,function(v)paste0(paste0(v,'_GRU'),'.x')))
top_vars_y <- c(sapply(top_vars,function(v)paste0(paste0(v,'_GRU'),'.y')))
top_vars_x <- c(top_vars_x,
                'numberGameTeamSeason.x',
                'year.x')
top_vars_y <- c(top_vars_y,
                'numberGameTeamSeason.y',
                'year.y')
df <- df[,colnames(df) %in% c(c(top_vars_x,
                                top_vars_y),
                              'ptsScored_home',
                              'ptsScored_away')]
df$idGame <- new_gl_merged$nextGameID
df <- df %>%
  na.omit 
df$team_year.x <- (merge(df[,c('ptsScored_home','idGame')],
       gl[gl$locationGame == "H",
          c('slugTeam','slugOpponent','dateGame','idGame','yearSeason')]) %>%
    mutate(team_year = paste0(
      slugTeam,
      "_",
      yearSeason
    )))$team_year
df$team_year.y <- (merge(df[,c('ptsScored_home','idGame')],
                         gl[gl$locationGame == "A",
                            c('slugTeam','slugOpponent','dateGame','idGame','yearSeason')]) %>%
                     mutate(team_year = paste0(
                       slugTeam,
                       "_",
                       yearSeason
                     )))$team_year
rownames <- (merge(df[,c('ptsScored_home','idGame')],
              gl[gl$locationGame == "A",
              c('slugTeam','slugOpponent','dateGame','idGame')]) %>%
  mutate(rownames = paste0(
    slugTeam,
    "at",
    slugOpponent,
    "_",
    dateGame
  )))$rownames
rownames <- paste0(1:nrow(df),":",rownames)

## final merge, removing nas
df[,!(colnames(df) %in% c("team_year.x",
                          "team_year.y"))] <- df[,!(colnames(df) %in% c("team_year.x",
                                                                        "team_year.y"))] %>%
  apply(2,as.numeric) %>%
  as.data.frame
rownames(df) <- rownames

## duplicate, swap colnames
df_swap <- df
for(col in colnames(df)){
  nch <- nchar(col)
  if(grepl(".x",col)){
    colnames(df_swap)[colnames(df) == col] <- 
      paste0(
        substr(col,
               1,
               nch-2),
        ".y",
        collapse = ""
      )
  } else if(grepl(".y",col)){
    colnames(df_swap)[colnames(df) == col] <- 
      paste0(
        substr(col,
               1,
               nch-2),
        ".x",
        collapse = ""
      )
  }
}
df_swap$ptsScored_aw.y <- NULL
df_swap$ptsScored_aw.x <- NULL
df_swap$ptsScored_home <- df$ptsScored_away
df_swap$ptsScored_away <- df$ptsScored_home
rownames(df_swap) <- paste0(rownames(df),
                            "_reversed")
df_swap$year.y <- NULL
df$year.y <- NULL
df$location <- 1 # 1 for home
df_swap$location <- 0 #0 for away
df <- rbind(df,df_swap[,colnames(df)])
save(df, 
  file = 
"C:\\Users\\defgi\\Documents\\AbsolutelyStackedSupplementaryFiles\\recipes_lstm.RData")