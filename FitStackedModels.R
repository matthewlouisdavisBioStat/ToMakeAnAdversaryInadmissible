rm(list=ls())
library(MachineShop)
library(recipes)
library(parallel)
library(foreach)
library(doParallel)
library(lubridate)
library(rstan)
library(earth)
library(randomForestSRC)
library(quadprog)
library(Matrix)
library(MASS)
library(StanHeaders)
dir_prefix <- 'C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/'
setwd(paste0(dir_prefix,'Stacking'))
for(k in c('glmer_constrained.R', 
'HelperFunctions.R', 
'make_dummy_Z_and_sigmalist.R',
'make_glmerStackedModel.R', 
'make_loglikelihood_MLMetric.R',
'MLModel_DataPrep.R', 
'predict_glmerStacked.R',
'quantify_stacked_uncertainty.R', 
'SoftmaxOperations.R', 
'stepAIC_stacked.R')){
  source(k)
}
std <- function(x)(x-mean(x))/sd(x)
log_std <- function(x){
  xx <- log(x)
  (xx-mean(xx))/sd(xx)
}
## Prep NBA Data
prep_fxn <- function(outcome,rand_int_per_year_per_team,link, df){
  
  ## response (y) variables
  outcome_vars <- c("logPtsGame",
                    "ptsGame",
                    "spreadDiff",
                    "moneyLine",
                    "overtime",
                    "spreadDiffOT",
                    "logitPropPts",
                    "arcsinsqrt_prop")
  outcome_vars <- outcome_vars[outcome_vars != outcome]
  fo <- as.formula(paste0(outcome, " ~ ."))
  df$spreadDiffOT <- ifelse(as.numeric(df$overtime) > 240,
                            0,
                            df$spreadDiff)
  
  ## if logit ensemble, set up factors
  df$moneyLine <- factor(
    ifelse(df$spreadDiff > 0,
           1,
           0),
    levels = c(0,1)
  )
  
  ## if overtime, set up factors
  df$overtime <- factor(
    df$overtime
  )
  
  ## from manually selected variables looking at varimp plots
  load(paste0(dir_prefix, outcome, "_vars2keep.RData"))
  
  ## keep log or linear of most important variable 
  if(outcome %in% c("logPtsGame","arcsinsqrt_prop","overtime")){
    if(outcome %in% c("logPtsGame")){
      df$minutesPlayedPer48 <- log(as.numeric(as.character(df$overtime))/240)
    }
    df$ptsTeamOpp.x <- NULL
    df$ptsTeamOpp.y <- NULL
    df$ptsTeamOppLastGame.x <- NULL
    df$ptsTeamOppLastGame.y <- NULL
    df$ptsTeamPerMinute.x <- NULL
    df$ptsTeamPerMinute.y <- NULL
    df$ptsTeamPerMinuteLastGame.x <- NULL
    df$ptsTeamPerMinuteLastGame.y <- NULL
  } else if(outcome %in% c('ptsGame','spreadDiff')){
    df$logPtsTeamOpp.x <- NULL
    df$logPtsTeamOpp.y <- NULL
    df$logPtsTeamOppLastGame.x <- NULL
    df$logPtsTeamOppLastGame.y <- NULL
    df$logPtsTeam.x <- NULL
    df$logPtsTeam.y <- NULL
    df$logPtsTeamLastGame.x <- NULL
    df$logPtsTeamLastGame.y <- NULL
  } 
  
  ## recipe object
  rec <- recipe(fo, data = df[,!(colnames(df) %in% c(outcome_vars)),])
  if(outcome == "moneyLine"){
    rec <- rec %>%
    role_case(stratum = moneyLine)
  } else if(outcome == "logPtsGame" & link == "log"){
    rec$template$logPtsGame <- 
      PoissonVariate(as.integer(exp(rec$template$logPtsGame)))
  }
  
  ## setting/specifying home and away teams
  away_team_year <- sapply(rownames(df),function(x){
    a <- ((strsplit(x,":") %>% unlist)[-c(1)] %>%
            paste(collapse = "") %>%
            strsplit("at") %>% unlist)[1]
    b <- df[x,"year.x"]
    paste(a,b,sep = "_")
  })
  home_team_year <- sapply(rownames(df),function(x){
    a <- ((strsplit(x,":") %>% unlist)[-c(1)] %>%
            paste(collapse = "") %>%
            strsplit("at") %>% unlist)[-1]
    a <- ((strsplit(a,"_") %>% unlist)[c(1)] %>%
            paste(collapse = ""))
    b <- df[x,"year.x"]
    paste(a,b,sep = "_")
  })
  dates <- sapply(rownames(df),function(x){
    a <- ((strsplit(x,":") %>% unlist)[-c(1)] %>%
            paste(collapse = "") %>%
            strsplit("at") %>% unlist)[-1]
    paste0((strsplit(a,"_") %>% unlist)[c(2)] %>%
            paste(collapse = ""))
  })
  fact <- factor(unique(c(home_team_year,away_team_year)))
  home_team_year <- factor(home_team_year,levels = levels(fact))
  away_team_year <- factor(away_team_year,levels = levels(fact))
  M <- length(levels(fact))
  N <- nrow(df)
  
  ## random efx design matrix of home team
  Z_home <- sapply(1:M,function(m){ 
    z <- rep(0,N)
    z[as.numeric(home_team_year) == m] <- 1
    z
  })
  colnames(Z_home) <- sapply(1:M,function(col){
    as.character(unique(c(home_team_year,away_team_year)[
      as.numeric(c(home_team_year,away_team_year)) == col]))
  })
  
  ## random efx design matrix of away team
  Z_away <- sapply(1:M,function(m){
    z <- rep(0,N)
    z[as.numeric(away_team_year) == m] <- 1
    z
  })
  colnames(Z_away) <- colnames(Z_home)
  year <- df$year.x
  Z_year <- sapply(unique(year),function(y){
    z <- rep(0,N)
    z[as.numeric(year) == y] <- 1
    z
  })
  
  ## For matchups, random intercepts are subtractive (ie moneyline)
  ## home team random intercept is positive, away team is negative
  if(outcome %in% 
     c("moneyLine",
       "spreadDiff",
       "spreadDiffOT",
       "logitPropPts",
       "arcsinsqrt_prop")){
    Z_away <- -Z_away
  }
  
  ## Final Z
  Z <- Z_home + Z_away
  
  
  ## should random intercepts be nested by team AND year? 
  ## = Should there be a different variance component for each year?
  if(rand_int_per_year_per_team){
    
    ## var components for each year
    sigma_list <- lapply(unique(year),function(y){
      grep(y,levels(fact))
    })
    names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
    for(i in 1:length(sigma_list)){
      names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
                                    length(sigma_list[[i]]))
    }
  
  } else {
    
    ## var components for all years
    sigma_list <- lapply(1,function(x)1:ncol(Z))
    names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
    for(i in 1:length(sigma_list)){
      names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
                                    length(sigma_list[[i]]))
    }
    
  }

  return(list("Z" = Z,
              "rec" = rec,
              "sigma_list" = sigma_list))
  
}
save(prep_fxn,file = "prep_fxn.RData")

################################################################################
## ## Models
################################################################################

## Candidate elastic nets
glmnet_log1 <- GLMNetModel(alpha = 0.131569752731917, 
                           lambda = 0.160641925132831)
glmnet_log2 <- GLMNetModel(alpha = 0.0393612877056719, 
                           lambda = 0.361444331548869)
glmnet_arcsin1 <- GLMNetModel(alpha = 0.998620520450233,
                              lambda = 0.000102821332032989)
glmnet_arcsin2 <- GLMNetModel(alpha = 0.0371,
                              lambda = 0.000851)
glmnet_ptsGame1 <- GLMNetModel(alpha = 0.111141463511846, 
                               lambda = 0.652699910955847)
glmnet_ptsGame2 <- GLMNetModel(alpha  =  0.062750752005012012,
                               lambda = 0.3614443315488693)
glmnet_overtime1 <- GLMNetModel(alpha = 0.000106,
                                lambda = 0.834)
glmnet_overtime2 <- GLMNetModel(alpha = 0.0629961016739724,
                                lambda = 0.986369632474371)
glmnet_spread1 <- GLMNetModel(alpha = 0.490903982843887,
                              lambda =  0.130657717492298)
glmnet_spread2 <- GLMNetModel(alpha =  0.05357894736842105,
                              lambda =  0.791578947368421)
## EarthModel with log-transform
EarthModel_Log1 <-
  MLModel(
    name = "EarthModel_Log1",
    packages = c("earth"),
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = F,
    fit = function(formula, data, weights, ...) {

      ## exclude names, weights, strata
      data[['(strata)']] <- NULL
      data[['(weights)']] <- NULL
      data[['(names)']] <- NULL
      temp <- data[,-which(colnames(data) %in% c(paste0(formula)[[2]],
                                                 no_featreduc_vars))]

      ## isolate numerics with non-zero variance
      temp <- as(temp,'matrix')
      temp <- apply(temp,2,as.numeric)
      temp <- temp[,!c(apply(temp,2,function(x)any(is.na(x))))]
      temp <- temp[,!c(apply(temp,2,function(x)sd(x) < 1.5e-8))]

      ## set up new data
      juiced <- cbind(as.data.frame(temp),
                      data[,which(colnames(data) %in% no_featreduc_vars)])
      sdd <- sd(log(data[[(paste0(formula)[[2]])]]))
      juiced[[(paste0(formula)[[2]])]] <-
        (log(data[[(paste0(formula)[[2]])]]))/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""),
                                       " ~ .",
                                       collapse = " "))
      fitt <- earth(new_formula,
                   juiced,
                   degree = 2,
                   nprune = 216)
      return(list("fitt" = fitt,
                  "sd" = sdd,
                  "cols" = colnames(temp)))
    },
    predict = function(object, newdata, ...) {

      fitt <- object$fitt
      temp <- apply(as(newdata,'matrix'),2,as.numeric)[,object$cols]
      dat <- cbind(as.data.frame(temp),
                   newdata[,which(colnames(newdata) %in% no_featreduc_vars)])
      if (nrow(dat) == 1) {
        exp(predict(object$fitt, newdata = rbind(dat, dat),
                    type = "response")[1]*object$sd)
      } else {
        exp(predict(object$fitt,
                    newdata = dat,
                    type = "response")*object$sd)
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )
EarthModel_Log2 <-
  MLModel(
    name = "EarthModel_Log2",
    packages = c("earth"),
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = F,
    fit = function(formula, data, weights, ...) {
      
      ## exclude names, weights, strata
      data[['(strata)']] <- NULL
      data[['(weights)']] <- NULL
      data[['(names)']] <- NULL
      temp <- data[,-which(colnames(data) %in% c(paste0(formula)[[2]],
                                                 no_featreduc_vars))]
      
      ## isolate numerics with non-zero variance
      temp <- as(temp,'matrix')
      temp <- apply(temp,2,as.numeric)
      temp <- temp[,!c(apply(temp,2,function(x)any(is.na(x))))]
      temp <- temp[,!c(apply(temp,2,function(x)sd(x) < 1.5e-8))]
      
      ## set up new data
      juiced <- cbind(as.data.frame(temp),
                      data[,which(colnames(data) %in% no_featreduc_vars)])
      sdd <- sd(log(data[[(paste0(formula)[[2]])]]))
      juiced[[(paste0(formula)[[2]])]] <-
        (log(data[[(paste0(formula)[[2]])]]))/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""),
                                       " ~ .",
                                       collapse = " "))
      fitt <- earth(new_formula,
                    juiced,
                    degree = 3,
                    nprune = 216)
      return(list("fitt" = fitt,
                  "sd" = sdd,
                  "cols" = colnames(temp)))
    },
    predict = function(object, newdata, ...) {
      
      fitt <- object$fitt
      temp <- apply(as(newdata,'matrix'),2,as.numeric)[,object$cols]
      dat <- cbind(as.data.frame(temp),
                   newdata[,which(colnames(newdata) %in% no_featreduc_vars)])
      if (nrow(dat) == 1) {
        exp(predict(object$fitt, newdata = rbind(dat, dat),
                    type = "response")[1]*object$sd)
      } else {
        exp(predict(object$fitt,
                    newdata = dat,
                    type = "response")*object$sd)
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )


## xgbost candidates
xgb_log1 <- XGBTreeModel(eta = 0.0160293443233614, 
                         alpha = 14.0992037301476, 
                         lambda = 14.3960447751306, 
                         max_depth = 4, 
                         colsample_bytree = 0.104695859987987, 
                         nrounds = 3750)
xgb_log2 <- XGBTreeModel(eta =0.0765210590879272 ,  
                         alpha = 0.782788079231977,
                         lambda = 4.87005806683252, 
                         max_depth = 4, 
                         colsample_bytree = 0.853095574966735, 
                         nrounds = 65)
xgb_ptsGame1 <- XGBTreeModel(eta = 0.00785181301659651, 
                             alpha = 1.23178552926401, 
                             lambda = 14.8031203410937, 
                             max_depth = 3, 
                             colsample_bytree = 0.359980966226431, 
                             nrounds = 4904)
xgb_ptsGame2 <- XGBTreeModel(eta = 0.00475,
                             alpha = 12.31785529264016, 
                             lambda = 14.61027015357104, 
                             max_depth = 4, 
                             colsample_bytree = .2638691770262085, 
                             nrounds = 3543)
xgb_overtime1 <- XGBTreeModel(
  eta = 0.0234,
  lambda = 4.38,
  subsample = 0.9025991,
  alpha = 0.448,
  max_depth = 2,
  nrounds = 1991
)
xgb_overtime2 <- XGBTreeModel(
    eta = 0.2,
    gamma = 0,
    max_depth = 8,
    max_leaves = 127,
    nrounds = 14,
    alpha = 0,
    lambda = 1.5625,
    subsample = 0.6
)
xgb_arcsin1 <- XGBTreeModel(eta = 0.0212529273624768, 
                            alpha = 0.31552593158267, 
                            lambda = 5.65785420381872, 
                            max_depth = 2, 
                            colsample_bytree = 0.5714594751189, 
                            nrounds = 1864)
xgb_arcsin2 <- XGBTreeModel(eta = .001430283751367824,
                            alpha = 2.3958333333333335,
                            lambda = 1.7708333333333335,
                            max_depth = 3,
                            colsample_bytree = .5289966501877643,
                            nrounds = 1802)
xgb_spread1 <- XGBTreeModel(eta = 0.00451819277824399, 
                            alpha = 4.48424704423815, 
                            lambda = 7.59495424841589, 
                            max_depth = 9, 
                            colsample_bytree = 0.125432669365546, 
                            nrounds = 1732)
xgb_spread2 <- XGBTreeModel(eta = 0.00222859228523049,
                            alpha = 7.76807834823558,
                            lambda = 0.317515154310968,
                            max_depth = 7,
                            colsample_bytree = 0.103640618402278,
                            nrounds = 2544)



## save the candidate models
save(
  
  glmnet_log1,
  glmnet_log2,
  glmnet_overtime1,
  glmnet_overtime2,
  glmnet_ptsGame1,
  glmnet_ptsGame2,
  glmnet_arcsin1,
  glmnet_arcsin2,
  
  glmnet_spread1,
  glmnet_spread2,
  xgb_spread1,
  xgb_spread2,
  
  xgb_log1,
  xgb_log2,
  xgb_overtime1,
  xgb_overtime2,
  xgb_ptsGame1,
  xgb_ptsGame2,
  xgb_arcsin1,
  xgb_arcsin2,
  EarthModel_Log1,
  EarthModel_Log2,
  file = 'candidate_models.RData')

################################################################################
## ## Load and export data
################################################################################

## Load our data frame constructed from PrepData
load(paste0(dir_prefix,"recipes.RData"))
outcome <- 'ptsGame'

## Set up data for different outcomes
prep_list_linear <- prep_fxn("ptsGame", rand_int_per_year_per_team = FALSE, link = "linear", df)
prep_list_log <- prep_fxn("logPtsGame", rand_int_per_year_per_team = FALSE, link = "log", df)
prep_list_overtime <- prep_fxn("overtime", rand_int_per_year_per_team = FALSE, link = "logit", df)
prep_list_arcsin <- prep_fxn("arcsinsqrt_prop", rand_int_per_year_per_team = FALSE, link = "linear", df)
prep_list_spread <- prep_fxn("spreadDiff", rand_int_per_year_per_team = FALSE, link = "linear", df)

## Save and export recipe objects
rec <- prep_list_linear$rec
Z <- prep_list_linear$Z
sigma_list <- prep_list_linear$sigma_list
no_featreduc_vars <- c('year.x',
                       'numberGameTeamSeason.x',
                       'numberGameTeamSeason.y',
                       colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))<=4)])
save(no_featreduc_vars,file = 'no_featreduc_vars.RData')
no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]


################################################################################
## ## Fit models
################################################################################

setwd(dir_prefix)
## Loop through each of the 5 outcomes
#' 1) total points per 48 as Gaussian distributed
#' 2) total points per 48 as Overdispersed Poisson distributed
#' 3) overtime as Bernoulli event
#' 4) spread as Guassian distributed
#' 5) arcsin_sqrt as Gaussian distributed
#' clusterExport(cl0,unique(c(c(as.vector(lsf.str())),ls()[ls() != 'cl0'])))
for (ijk in c(1, 2, 4, 5)) {
  start_time <- Sys.time()
  
  if (ijk == 1) {
    
    # Linear (Gaussian) regression model
    outcome <- "ptsGame"
    cl2 <- makeCluster(8)
    registerDoParallel(cl2)
    
    # Extract model components from prepared data
    rec <- prep_list_linear$rec
    Z <- prep_list_linear$Z
    sigma_list <- prep_list_linear$sigma_list
    
    # Define variables to exclude from feature reduction
    no_featreduc_vars <- intersect(
      c(
        "year.x", "numberGameTeamSeason.x", "numberGameTeamSeason.y",
        names(rec$template)[sapply(rec$template, function(i) length(unique(i)) == 2)]  
      ),
      colnames(rec$template)
    )
    
    # Export necessary objects to clusters  
    clusterExport(cl2, c(
      ls(), "rec", "Z", "sigma_list", "solve.QP", "nearPD",
      "ginv", "no_featreduc_vars", "earth"
    ))
    
    # Fit stacked model with glmnet, xgboost, and earth as base learners
    stacked_model <- fit(rec,
                         model = SuperModel(
                           glmnet_ptsGame1,
                           xgb_ptsGame2,
                           EarthModel(degree = 3, nprune = 250),
                           model = make_glmerStackedModel(
                             link = "linear",
                             incl_random_efx = TRUE,
                             tau_0 = 1,
                             est_dispersion = TRUE,
                             min_learners = 3,
                             Z = Z,
                             sigma_list = sigma_list,
                             use_qp = TRUE,
                             fixed_weights = FALSE,
                             weights = NULL
                           ),
                           control = CVControl(folds = 16, seed = 52245)
                         )
    )
    
    # Quantify uncertainty of stacked model using Stan
    stacked_stan <- stacked_model %>%
      quantify_stacked_uncertainty(
        return_fit = FALSE,
        iter = 40000,
        warmup = 2000, 
        cores = 8,
        take_a_break_first = TRUE,
        filename = "ptsGame_stan.RData",
        verbose = FALSE,
        mu_eta = mean(rec$template$ptsGame),
        sigma_eta = sd(rec$template$ptsGame)
      )
    
    # Save fitted models  
    save(stacked_model, file = "stacked_model_gaussian.RData")
    if (!inherits(stacked_stan, "try-error")) {
      save(stacked_stan, file = "stacked_stan_gaussian.RData")  
    }
    
    # Clean up
    rm(stacked_model, stacked_stan)
    stopCluster(cl2)
    
  ## Repeat for other outcomes of interest
  } else if (ijk == 2) {
    
    # Quasi-Poisson regression model
    cl2 <- makeCluster(8)
    registerDoParallel(cl2)  
    
    rec <- prep_list_log$rec
    Z <- prep_list_log$Z
    sigma_list <- prep_list_log$sigma_list
    
    no_featreduc_vars <- intersect(
      c(
        "year.x", "numberGameTeamSeason.x", "numberGameTeamSeason.y",
        names(rec$template)[sapply(rec$template, function(i) length(unique(i)) == 2)]
      ),
      colnames(rec$template)
    )
    
    clusterExport(cl2, c(
      ls(), "rec", "Z", "sigma_list", "solve.QP", "no_featreduc_vars", "earth"  
    ))
    
    stacked_model <- fit(rec,
                         model = SuperModel(
                           glmnet_log1,
                           xgb_log1,
                           EarthModel_Log2,
                           model = make_glmerStackedModel(
                             link = "log",
                             incl_random_efx = TRUE,
                             tau_0 = 1,
                             est_dispersion = TRUE,
                             min_learners = 3,
                             Z = Z,
                             sigma_list = sigma_list,
                             use_qp = TRUE 
                           ),
                           all_vars = FALSE,
                           control = CVControl(folds = 16, seed = 52245)
                         )
    )
    
    stacked_stan <- stacked_model %>%
      quantify_stacked_uncertainty(
        return_fit = FALSE,
        iter = 40000,
        warmup = 2000,
        take_a_break_first = TRUE,
        cores = 8,
        filename = "logPtsGame_stan.RData", 
        verbose = FALSE,
        mu_eta = mean(rec$template$logPtsGame),
        sigma_eta = sd(rec$template$logPtsGame)
      )
    
    save(stacked_model, file = "stacked_model_log.RData")
    if (!inherits(stacked_stan, "try-error")) {
      save(stacked_stan, file = "stacked_stan_log.RData")
    }
    
    rm(stacked_model, stacked_stan)
    stopCluster(cl2)
    
  } else if (ijk == 3) {
    
    # Logistic regression model for overtime
    cl2 <- makeCluster(8)
    registerDoParallel(cl2)
    
    rec <- prep_list_overtime$rec
    rec$template$overtime <- factor(ifelse(rec$template$overtime == 240, 0, 1))
    Z <- prep_list_overtime$Z  
    sigma_list <- prep_list_overtime$sigma_list
    
    no_featreduc_vars <- intersect(
      setdiff(
        c(
          "year.x", "numberGameTeamSeason.x", "numberGameTeamSeason.y",
          names(rec$template)[sapply(rec$template, function(i) length(unique(i)) == 2)]
        ),
        "overtime"
      ),
      colnames(rec$template)
    )
    
    clusterExport(cl2, c(
      ls(), "rec", "Z", "sigma_list", "no_featreduc_vars", "earth"
    ))
    
    stacked_model <- fit(rec,
                         model = SuperModel(
                           glmnet_overtime1,
                           glmnet_overtime2,
                           RandomForestModel(ntree = 50, mtry = 50),
                           model = make_glmerStackedModel(
                             link = "logit",
                             incl_random_efx = TRUE,
                             tau_0 = 1,
                             est_dispersion = FALSE,
                             min_learners = 2,
                             Z = Z,
                             sigma_list = sigma_list, 
                             use_qp = TRUE,
                             fixed_weights = FALSE,
                             weights = NULL
                           ),
                           all_vars = FALSE,
                           control = CVControl(folds = 16, seed = 52245)
                         )
    )
    
    stacked_stan <- stacked_model %>%
      quantify_stacked_uncertainty(
        return_fit = FALSE,
        iter = 40000,
        warmup = 2000,
        cores = 8,
        take_a_break_first = FALSE,
        mu_eta = log(
          mean(as.character(rec$template$overtime) != "0") /
            (1 - mean(as.character(rec$template$overtime) != "0"))  
        ),
        filename = "overtime_stan.RData",
        verbose = FALSE
      )
    
    save(stacked_model, file = "stacked_model_overtime.RData")
    if (!inherits(stacked_stan, "try-error")) {
      save(stacked_stan, file = "stacked_stan_overtime.RData")  
    }
    
    rm(stacked_model, stacked_stan)
    stopCluster(cl2)
    
  } else if (ijk == 4) {
    
    # Linear regression model for point spread
    cl2 <- makeCluster(8)
    registerDoParallel(cl2)
    
    rec <- prep_list_spread$rec  
    Z <- prep_list_spread$Z
    sigma_list <- prep_list_spread$sigma_list
    
    no_featreduc_vars <- intersect(
      c(
        "year.x", "numberGameTeamSeason.x", "numberGameTeamSeason.y",
        names(rec$template)[sapply(rec$template, function(i) length(unique(i)) == 2)]
      ),
      colnames(rec$template) 
    )
    
    clusterExport(cl2, c(
      ls(), "rec", "Z", "sigma_list", "no_featreduc_vars", "earth"
    ))
    
    stacked_model <- fit(rec,
                         model = SuperModel(
                           glmnet_spread1,
                           xgb_spread1, 
                           EarthModel(degree = 2, nprune = 216),
                           model = make_glmerStackedModel(
                             link = "linear",
                             incl_random_efx = TRUE,
                             tau_0 = 1,
                             est_dispersion = TRUE,
                             min_learners = 3,
                             Z = Z,
                             sigma_list = sigma_list,
                             use_qp = TRUE,
                             fixed_weights = FALSE,
                             weights = NULL
                           ),
                           all_vars = FALSE,
                           control = CVControl(folds = 16, seed = 52246)
                         )
    )
    
    stacked_stan <- stacked_model %>%
      quantify_stacked_uncertainty(
        return_fit = FALSE, 
        iter = 40000,
        warmup = 2000,
        cores = 8,
        take_a_break_first = TRUE,
        filename = "spread_stan.RData",
        verbose = FALSE, 
        mu_eta = mean(rec$template$spreadDiff),
        sigma_eta = sd(rec$template$spreadDiff)
      )
    
    save(stacked_model, file = "stacked_model_spread.RData")
    if (!inherits(stacked_stan, "try-error")) {
      save(stacked_stan, file = "stacked_stan_spread.RData")
    }
    
    rm(stacked_model, stacked_stan)
    stopCluster(cl2)
    
  } else if (ijk == 5) {
    
    # Linear regression model for arcsin sqrt proportion of points scored by home team
    cl2 <- makeCluster(8)  
    registerDoParallel(cl2)
    
    rec <- prep_list_arcsin$rec
    Z <- prep_list_arcsin$Z
    sigma_list <- prep_list_arcsin$sigma_list
    
    no_featreduc_vars <- intersect(
      c(
        "year.x", "numberGameTeamSeason.x", "numberGameTeamSeason.y", 
        names(rec$template)[sapply(rec$template, function(i) length(unique(i)) == 2)]
      ),
      colnames(rec$template)
    )
    
    clusterExport(cl2, c(
      ls(), "rec", "Z", "sigma_list", "no_featreduc_vars", "earth"  
    ))
    
    stacked_model <- fit(rec,
                         model = SuperModel(
                           EarthModel_Log1,
                           glmnet_arcsin2,
                           xgb_arcsin1,
                           model = make_glmerStackedModel(
                             link = "linear",
                             incl_random_efx = TRUE,
                             tau_0 = 1,
                             est_dispersion = TRUE, 
                             min_learners = 3,
                             Z = Z,
                             sigma_list = sigma_list,
                             use_qp = TRUE,
                             fixed_weights = FALSE,
                             weights = NULL
                           ),
                           all_vars = FALSE,
                           control = CVControl(folds = 16, seed = 52245)
                         )
    )
    
    stacked_stan <- stacked_model %>%
      quantify_stacked_uncertainty(
        return_fit = FALSE,
        iter = 40000,
        warmup = 2000,
        cores = 8,
        take_a_break_first = TRUE, 
        filename = "arcsin_stan.RData",
        verbose = FALSE,
        mu_eta = mean(rec$template$arcsinsqrt_prop),
        sigma_eta = sd(rec$template$arcsinsqrt_prop)  
      )
    
    save(stacked_model, file = "stacked_model_arcsin.RData")
    if (!inherits(stacked_stan, "try-error")) {
      save(stacked_stan, file = "stacked_stan_arcsin.RData")
    }
    
    rm(stacked_model, stacked_stan)
    stopCluster(cl2)
  }
}