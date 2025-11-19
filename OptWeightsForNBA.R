
################################################################################
## ## Setup
################################################################################

setwd(dir_prefix)
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
years <- 2014:2026

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
## impute these variables to be equal to be the average of median and mean of non-0 entries
# when equal to 0, when tracking data is missing
mean_median_impute <- c(
  'touches',
  'trebChances',
  'drebChances',
  'orebChances',
  'distMiles',
  'pctFGRimDefended',
  'fgaRimDefended',
  'fgmRimDefended',
  'pctFGUncontested',
  'fgaUncontested',
  'fgmUncontested',
  'pctFGContested',
  'fgaContested',
  'fgmContested',
  'passes'
)
rws <- which(rowMeans(abs(bs[,mean_median_impute])) == 0)
bs[rws, mean_median_impute] <- apply(bs[-rws,mean_median_impute], 2, median, na.rm = TRUE)*0.5 + 
  apply(bs[-rws,mean_median_impute], 2, mean, na.rm = TRUE)*0.5

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

## Parallelism
cl <- makeCluster(round(length(years)))
registerDoParallel(cl)
clusterExport(cl,ls())

## Loss function for optimization
loss_fxn <- function(par){
  alpha <- par[1]
  beta <- par[2]
  gamma <- par[3]
  res <- foreach(year = years, 
                 .combine = 'c',
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
                                 loss <- 0
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
                                     for(i in 1:nrow(data)){
                                       ## abs difference between weighted average over all previous games vs. actual game = loss
                                       w <- sapply(max(i-1, 1):1,
                                                   function(x)exp(alpha + beta*log(i) + gamma*abs(i - x)))
                                       for(var in c(
                                         average_vars[average_vars %in% colnames(data)],
                                         paste0(average_vars,'_opp')[paste0(average_vars,'_opp') %in% 
                                                                     colnames(data)])){
                                         loss <- loss + sum(na.omit(c(abs(
                                           weighted.mean(
                                             as.numeric(unlist(data[max(i-1, 1):1,var])),
                                             w,
                                             na.rm = TRUE) - 
                                             data[i, var]
                                         ), 0)))
                                       }
                                     }
                                   }
                                 }
                                 loss
                               }
  ## actual loss
  sum(res / (nrow(bs) * ncol(bs)))
}

## optimize
opt <- optim(
  c(0, 0, -0.1),
  fn = loss_fxn
)
print(opt$par)
# [1]  0.09772250  0.05226559 -0.26998223


## timing, stop cluster
end <- Sys.time()
start-end
# Time difference of -52.42339 mins
stopCluster(cl)


