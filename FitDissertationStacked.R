library(MachineShop)
library(recipes)
#library(pander)
library(parallel)
library(foreach)
library(doParallel)
##source('AdHocAbsStacked.R')
library(lubridate)
#source("set_optim_thompson.R")
library(rstan)
cl0 <- makeCluster(5)
registerDoParallel(cl0)
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
## for the kmeans deal
machine_fit <- MachineShop::fit
my_vst <- function(p,t){
  sqrt(t*p*(1-p))*log(p/(1-p))
}
## Prep NBA Data
prep_fxn <- function(outcome,rand_int_per_year_per_team,link,
                     use_vst = F){
  
  ## should we use MY approach instead of arcsin-sqrt?
  if(use_vst){
    df$arcsinsqrt_prop <- sapply(1:nrow(df),function(i){
      my_vst(sin(df$arcsinsqrt_prop[i])^2,
             exp(df$logPtsGame[i]))
    })
  }
  
  outcome_vars <- c('logPtsGame',
                    'ptsGame',
                    'spreadDiff',
                    'moneyLine',
                    'overtime',
                    'spreadDiffOT',
                    'logitPropPts',
                    'arcsinsqrt_prop')
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
  
  ## recipe object
  rec <- recipe(fo, data = df[,!(colnames(df) %in% outcome_vars)]) ## specify outcome here
  if(outcome == "moneyLine"){
    rec <- rec %>%
    role_case(stratum = moneyLine)
  #} else if(outcome == "overtime"){
    #invisible()
    # rec <- rec %>%
    #   role_case(stratum = overtime)
  } else if(outcome == "logPtsGame" & link == "log"){
    rec$template$logPtsGame <- PoissonVariate(as.integer(exp(rec$template$logPtsGame)))
  }
  
  ## setting/specifying home and away teams
  away_team_year <- sapply(rownames(df),function(x){
    a <- ((strsplit(x,":") %>% unlist)[-c(1)] %>%
            paste(collapse = "") %>%
            strsplit("at") %>% unlist)[1]
    # b <-  ((strsplit(x,"_") %>% unlist)[-c(1)] %>%
    #          paste(collapse = "") %>%
    #          strsplit("-") %>% unlist)[1]
    b <- df[x,'year.x']
    paste(a,b,sep = "_")
  })
  home_team_year <- sapply(rownames(df),function(x){
    a <- ((strsplit(x,":") %>% unlist)[-c(1)] %>%
            paste(collapse = "") %>%
            strsplit("at") %>% unlist)[-1]
    a <- ((strsplit(a,"_") %>% unlist)[c(1)] %>%
            paste(collapse = ""))
    # b <-  ((strsplit(x,"_") %>% unlist)[-c(1)] %>%
    #          paste(collapse = "") %>%
    #          strsplit("-") %>% unlist)[1]
    b <- df[x,'year.x']
    paste(a,b,sep = "_")
  })
  fact <- factor(unique(c(home_team_year,away_team_year)))
  home_team_year <- factor(home_team_year,levels = levels(fact))
  away_team_year <- factor(away_team_year,levels = levels(fact))
  M <- length(levels(fact))
  N <- nrow(df)
  Z_home <- sapply(1:M,function(m){ # random efx design matrix
    z <- rep(0,N)
    z[as.numeric(home_team_year) == m] <- 1
    z
  })
  colnames(Z_home) <- sapply(1:M,function(col){
    as.character(unique(c(home_team_year,away_team_year)[as.numeric(c(home_team_year,away_team_year)) == col]))
  })
  Z_away <- sapply(1:M,function(m){ # random efx design matrix
    z <- rep(0,N)
    z[as.numeric(away_team_year) == m] <- 1
    z
  })
  colnames(Z_away) <- colnames(Z_home)
  # year <- sapply(rownames(df),function(x){
  #   (strsplit(x,"_") %>% unlist)[-1] %>%
  #     lubridate::year() %>%
  #     as.numeric
  # })
  year <- df$year.x
  Z_year <- sapply(unique(year),function(y){
    z <- rep(0,N)
    z[as.numeric(year) == y] <- 1
    z
  })
  ## For matchups, random intercepts are subtractive (ie moneyline)
  ## home team random intercept is positive, away team is negative
  if(outcome %in% c("moneyLine","spreadDiff","spreadDiffOT",'logitPropPts','arcsinsqrt_prop')){
    Z_away <- -Z_away
  }
  
  ## Final Z
  Z <- Z_home + Z_away
  
  
  ## should random intercepts be nested by team AND year? Should there be a different variance component for each year?
  if(rand_int_per_year_per_team){
    
    ## var comp. for each year
    sigma_list <- lapply(unique(year),function(y){
      grep(y,levels(fact))
    })
    names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
    for(i in 1:length(sigma_list)){
      names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
                                    length(sigma_list[[i]]))
    }
    
  } else {
    
    ## var comp. for all years
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
save(prep_fxn,file = 'prep_fxn.RData')
#############################################################################
## Models
glmnet_log1 <- GLMNetModel(alpha = 0.131569752731917, 
                           lambda = 0.160641925132831)
glmnet_log2 <- GLMNetModel(alpha = 0.0393612877056719, 
                           lambda = 0.361444331548869)
glmnet_arcsin1 <- GLMNetModel(alpha = 0.0257027080212529,
                              lambda = 0.0401604812832077)
glmnet_arcsin2 <- GLMNetModel(alpha = 0.725398693177939,
                             lambda = 0.0195)
glmnet_ptsGame1 <- GLMNetModel(alpha = 0.675097690370721, 
                               lambda = 0.160641925132831)
glmnet_ptsGame2 <- GLMNetModel(alpha  =  0.062750752005012012,
                               lambda = 0.3614443315488693)
glmnet_overtime1 <- GLMNetModel(alpha = 4.01604812832077e-06,
                                lambda = 14.497933743238)
glmnet_overtime2 <- GLMNetModel(alpha = 0.860916221219995,
                                lambda = 1e-8)
glmnet_spread1 <- GLMNetModel(alpha = 0.390938189003257,
                              lambda =  0.160641925132831)
glmnet_spread2 <- GLMNetModel(alpha =  0.112003566250738,
                              lambda =  0.642567700531323)

## in general
NNetPCAModel <- 
  MLModel(
    name = "NNetPCAModel",
    packages = c("nnet"),
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = TRUE,
    fit = function(formula, data, weights, ...) {
      pc <- prcomp(data[,-which(colnames(data) %in% paste0(formula)[[2]])],
                   scale. = T,
                   center = T,
                   rank. = 149)
      juiced <- predict(pc,data[,-which(colnames(data) %in% paste0(formula)[[2]])])
      meann = mean((data[[(paste0(formula)[[2]])]]))
      sdd = sd((data[[(paste0(formula)[[2]])]]))
      juiced[[(paste0(formula)[[2]])]] <-  
        (data[[(paste0(formula)[[2]])]] - meann)/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""), 
                                       " ~ .",
                                       collapse = " "))
      fitt <- nnet(new_formula,
                   juiced,
                   MaxNWts = 2500000, 
                   size = 16,
                   maxit = 2500,
                   decay = 341)
      return(list("pc" = pc,
                  "fitt" = fitt,
                  "mean" = meann,
                  "sd" = sdd))
    },
    predict = function(object, newdata, ...) {
      
      fitt <- object$fitt
      dat <- predict(object$pc,newdata)
      if (nrow(dat) == 1) {
        predict(object$fitt, newdata = rbind(dat, dat),
                             type = "raw")[1]*object$sd + object$mean
      } else {
        predict(object$fitt, 
                             newdata = dat, 
                             type = "raw")*object$sd + object$mean
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )


## for spread
NNetPCAModel_spread <- 
  MLModel(
    name = "NNetPCAModel",
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = TRUE,
    fit = function(formula, data, weights, ...) {
      recc <- step_pca((recipe(formula,data) %>%
                          step_YeoJohnson(all_numeric_predictors(),
                                          -all_of(no_featreduc_vars)) %>%
                          step_normalize(all_numeric_predictors(),
                                         -all_of(no_featreduc_vars))),
                       all_numeric_predictors(),
                       -all_of(no_featreduc_vars),
                       num_comp = 149)
      juiced <- juice(prep(recc))
	meann = mean((data[[(paste0(formula)[[2]])]]))
	sdd = sd((data[[(paste0(formula)[[2]])]]))
	juiced[[(paste0(formula)[[2]])]] <-  
          (data[[(paste0(formula)[[2]])]] - meann)/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""), 
                                       " ~ .",
                                       collapse = " "))
      fitt <- MachineShop::fit(new_formula,
                               juiced,
                               NNetModel(MaxNWts = 2500000, 
				                    size = 16,
                            maxit = 2500,
                            decay = 341))
      return(list("recc" = recc,
                  "fitt" = fitt,
			"mean" = meann,
			"sd" = sdd))
    },
    predict = function(object, newdata, ...) {
      
      recc <- object$recc
      fitt <- object$fitt
      dat <- recc %>%
        prep %>%
        bake(new_data = newdata)
      typ <- recc$template[ncol(recc$template)][[1]]
      if(length(unique(typ)) > 2){
        resptyp <- "numeric"
      } else {
        resptyp <- "prob"
      }
      if (nrow(dat) == 1) {
        MachineShop::predict(fitt, newdata = rbind(dat, dat),
                             type = resptyp)[1]
      } else {
        MachineShop::predict(fitt, newdata = dat, type = resptyp)
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )

## for arcsin
NNetPCAModel_arcsin <- 
  MLModel(
    name = "NNetPCAModel",
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = TRUE,
    fit = function(formula, data, weights, ...) {
      recc <- step_pca((recipe(formula,data) %>%
                          step_YeoJohnson(all_numeric_predictors(),
                                          -all_of(no_featreduc_vars)) %>%
                          step_normalize(all_numeric_predictors(),
                                         -all_of(no_featreduc_vars))),
                       all_numeric_predictors(),
                       -all_of(no_featreduc_vars),
                       num_comp = 149)
      juiced <- juice(prep(recc))
	meann = mean((data[[(paste0(formula)[[2]])]]))
	sdd = sd((data[[(paste0(formula)[[2]])]]))
	juiced[[(paste0(formula)[[2]])]] <-  
          (data[[(paste0(formula)[[2]])]] - meann)/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""), 
                                       " ~ .",
                                       collapse = " "))
      fitt <- MachineShop::fit(new_formula,
                               juiced,
                               NNetModel(MaxNWts = 2500000, 
				    size = 16,
                            maxit = 2500,
                            decay = 341))
      return(list("recc" = recc,
                  "fitt" = fitt,
			"mean" = meann,
			"sd" = sdd))
    },
    predict = function(object, newdata, ...) {
      
      recc <- object$recc
      fitt <- object$fitt
      dat <- recc %>%
        prep %>%
        bake(new_data = newdata)
      typ <- recc$template[ncol(recc$template)][[1]]
      if(length(unique(typ)) > 2){
        resptyp <- "numeric"
      } else {
        resptyp <- "prob"
      }
      if (nrow(dat) == 1) {
        MachineShop::predict(fitt, newdata = rbind(dat, dat),
                             type = resptyp)[1]*object$sd + object$mean
      } else {
        MachineShop::predict(fitt, newdata = dat, type = resptyp)*object$sd + object$mean
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )

## for gaussian
NNetPCAModel_ptsGame <- 
  MLModel(
    name = "NNetPCAModel",
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = TRUE,
    fit = function(formula, data, weights, ...) {
      recc <- step_pca((recipe(formula,data) %>%
                          step_YeoJohnson(all_numeric_predictors(),
                                          -all_of(no_featreduc_vars)) %>%
                          step_normalize(all_numeric_predictors(),
                                         -all_of(no_featreduc_vars))),
                       all_numeric_predictors(),
                       -all_of(no_featreduc_vars),
                       num_comp = 200)
      juiced <- juice(prep(recc))
	meann = mean((data[[(paste0(formula)[[2]])]]))
	sdd = sd((data[[(paste0(formula)[[2]])]]))
	juiced[[(paste0(formula)[[2]])]] <-  
          (data[[(paste0(formula)[[2]])]] - meann)/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""), 
                                       " ~ .",
                                       collapse = " "))
      fitt <- MachineShop::fit(new_formula,
                               juiced,
                               NNetModel(MaxNWts = 2500000, 
				    size = 16,
                            maxit = 2500,
                            decay = 341))
      return(list("recc" = recc,
                  "fitt" = fitt,
			"mean" = meann,
			"sd" = sdd))
    },
    predict = function(object, newdata, ...) {
      
      recc <- object$recc
      fitt <- object$fitt
      dat <- recc %>%
        prep %>%
        bake(new_data = newdata)
      typ <- recc$template[ncol(recc$template)][[1]]
      if(length(unique(typ)) > 2){
        resptyp <- "numeric"
      } else {
        resptyp <- "prob"
      }
      if (nrow(dat) == 1) {
        MachineShop::predict(fitt, newdata = rbind(dat, dat),
                             type = resptyp)[1]*object$sd + object$mean
      } else {
        MachineShop::predict(fitt, newdata = dat, type = resptyp)*object$sd + object$mean
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )
NNetPCAModel_logPtsGame <-
  MLModel(
    name = "NNetPCAModel",
    response_types = c("BinomialVariate",
                       "factor", "matrix", "NegBinomialVariate", "numeric",
                       "PoissonVariate", "Surv"),
    weights = TRUE,
    fit = function(formula, data, weights, ...) {
      ## converts outcome to log-scale
      recc <- step_pca((recipe(formula,data) %>%
                          step_YeoJohnson(all_numeric_predictors(),
                                          -all_of(no_featreduc_vars)) %>%
                          step_normalize(all_numeric_predictors(),
                                         -all_of(no_featreduc_vars))),
                       all_numeric_predictors(),
                       -all_of(no_featreduc_vars),
                       num_comp = 200)
      juiced <- juice(prep(recc))
	    meann = mean(log(data[[(paste0(formula)[[2]])]]))
	    sdd = sd(log(data[[(paste0(formula)[[2]])]]))
	    juiced[[(paste0(formula)[[2]])]] <-  
          ( log(data[[(paste0(formula)[[2]])]]) - meann)/sdd
      new_formula <- as.formula(paste0(paste0("",paste0(formula)[[2]],"",
                                              collapse = ""), 
                                       " ~ .",
                                       collapse = " "))
      juiced <- juice(prep(recc))
      fitt <- MachineShop::fit(new_formula,
                               juiced,
                               NNetModel(MaxNWts = 2500000, 
				                    size = 16,
                            maxit = 2500,
                            decay = 341))
      return(list("recc" = recc,
                  "fitt" = fitt,
			"mean" = meann,
			"sd" = sdd))
    },
    predict = function(object, newdata, ...) {
      
      recc <- object$recc
      fitt <- object$fitt
      dat <- recc %>%
        prep %>%
        bake(new_data = newdata)
      typ <- recc$template[ncol(recc$template)][[1]]
      if(length(unique(typ)) > 2){
        resptyp <- "numeric"
      } else {
        resptyp <- "prob"
      }
      if (nrow(dat) == 1) {
        exp(MachineShop::predict(fitt, newdata = rbind(dat, dat),
                                 type = resptyp)[1]*object$sd + object$mean)
      } else {
        exp(MachineShop::predict(fitt, newdata = dat, type = resptyp)*object$sd + object$mean)
      }
    },
    varimp = function(object, ...) {
      NULL
    }
  )

xgb_log1 <- XGBTreeModel(eta = 0.0106293443233614, 
                         alpha = 14.0992037301476, 
                         lambda = 14.3960447751306, 
                         max_depth = 6, 
                         colsample_bytree = 0.104695859987987, 
                         nrounds = 3730)
xgb_log2 <- XGBTreeModel(eta = .005924400204094127, 
				                 alpha = 19.07557652884861,
                         lambda = .06932938849786296, 
                         max_depth = 7, 
                         colsample_bytree = 0.1462228497723118, 
                         nrounds = 4910)
xgb_ptsGame1 <- XGBTreeModel(eta = 0.00785181301659651, 
                             alpha = 1.23178552926401, 
                             lambda = 14.8031203410937, 
                             max_depth = 3, 
                             colsample_bytree = 0.359980966226431, 
                             nrounds = 4904)
xgb_ptsGame2 <- XGBTreeModel(eta = .007129105400701519, 
                             alpha = 12.31785529264016, 
                             lambda = 14.61027015357104, 
                             max_depth = 4, 
                             colsample_bytree = .2638691770262085, 
                             nrounds = 3543)
xgb_overtime1 <- XGBTreeModel(
  eta = 0.0234,
  lambda = 4.38,
  subsample = 1,
  alpha = 0.448,
  max_depth = 2,
  nrounds = 467
)
xgb_overtime2 <- XGBTreeModel(
  eta = 0.0550,
  lambda = 0.278,
  alpha = 4.38,
  nrounds = 509,
  max_depth = 7,
  subsample = 1
)
xgb_arcsin1 <- XGBTreeModel(eta = 0.0212529273624768, 
                            alpha = 0.31552593158267, 
                            lambda = 5.65785420381872, 
                            max_depth = 2, 
                            colsample_bytree = 0.5714594751189, 
                            nrounds = 1864)
xgb_arcsin2 <- XGBTreeModel(eta = .001430283751367824, 
                            alpha = 3.255611595450319, 
                            lambda = 2.819995554455091, 
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




save(
  
  #NNetPCA_arcsin,
  #NNetPCA_log,
  #NNetPCA_ptsGame,
  #NNetPCA_overtime,
  NNetPCA,
  
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
  
 NNetPCAModel_ptsGame,
 NNetPCAModel_logPtsGame,
 NNetPCAModel_arcsin,
 NNetPCAModel_spread,
  
  file = 'candidate_models.RData')
#source("set_optim_thompson.R")
library(MachineShop)
library(recipes)
#library(pander)
library(parallel)
library(foreach)
library(doParallel)
library(quadprog)
library(Matrix)
library(MASS)
library(rstan)
library(StanHeaders)
load('prep_fxn.RData')
#load('recipes_playoffs.RData')
load('recipes.RData')
load('candidate_models.RData')
outcome <- 'ptsGame'
prep_list_linear <- prep_fxn('ptsGame',T,'linear')
prep_list_overtime <- prep_fxn('overtime',T,'logit')
prep_list_log <- prep_fxn('logPtsGame',T,'log')
prep_list_arcsin <- prep_fxn('arcsinsqrt_prop',F,'linear')
prep_list_spread <- prep_fxn('spreadDiff',F,'linear')
rec <- prep_list_linear$rec
Z <- prep_list_linear$Z
sigma_list <- prep_list_linear$sigma_list
no_featreduc_vars <- c('year.x',
                       'numberGameTeamSeason.x',
                       'numberGameTeamSeason.y',
                       colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
save(no_featreduc_vars,file = 'no_featreduc_vars.RData')
no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
clusterExport(cl0,unique(c(c(as.vector(lsf.str())),ls()[ls() != 'cl0'])))


foreach(ijk = c(1,2,4,5),
        .packages = c('doParallel',
                                'parallel',
                                'recipes',
                                'dplyr',
                                'foreach',
                                'MachineShop',
                                'glmnet',
                                'earth',
                                'randomForestSRC',
                                'quadprog',
                                'MASS',
                                'Rsolnp',
                                'kknn',
                                'xgboost',
                                'rBayesianOptimization',
                                'nnet',
                                'pso',
                                'survival',
                                'lubridate',
                                'rstan')) %dopar% {
                                  
                                  # set.seed(52246)
                                  #setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
                                  # setwd('Z:/Dissertation/Stacking')
                                  ## rm(list = ls()))
                                  # library(MachineShop)
                                  # library(recipes)
                                  # library(pander)
                                  # library(parallel)
                                  # library(foreach)
                                  # library(doParallel)
                                  # #source('AdHocAbsStacked.R')
                                  # library(lubridate)
                                  # source("set_optim_thompson.R")
                                  # library(rstan)
                                  
                                  ################################
                                  ################################
                                  ## Run Models
                                  (strt <- Sys.time())
                                  ################################################################################
                                  ## Linear (Gaussian) Regression
                                  # rm(list = ls()))
                                  if(ijk == 1){
                                    # source("set_optim_thompson.R")
                                    # library(MachineShop)
                                    # library(recipes)
                                    # library(pander)
                                    # library(parallel)
                                    # library(foreach)
                                    # library(doParallel)
                                    # library(quadprog)
                                    # library(Matrix)
                                    # library(MASS)
                                    # #source('AdHocAbsStacked.R')
                                    # library(rstan)
                                    # library(StanHeaders)
                                    # load('prep_fxn.RData')
                                    # load('recipes_playoffs.RData')
                                    # load('candidate_models.RData')
                                    outcome <- 'ptsGame'
                                    cl2 <- makeCluster(10)
                                    registerDoParallel(cl2)
                                    ##
                                    #prep_list <- prep_fxn('ptsGame',T,'linear')
                                    rec <- prep_list_linear$rec
                                    Z <- prep_list_linear$Z
                                    sigma_list <- prep_list_linear$sigma_list
                                    no_featreduc_vars <- c('year.x',
                                                           'numberGameTeamSeason.x',
                                                           'numberGameTeamSeason.y',
                                                           colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
                                    #source("quantify_stacked_uncertainty2.R")
                                    clusterExport(cl2,c(c(as.vector(lsf.str())),
                                                        'rec',
                                                        'Z',
                                                        'sigma_list',
                                                        'solve.QP',
                                                        'nearPD',
                                                        'ginv',
                                                        'no_featreduc_vars'))
                                    stacked_model <- fit(rec,
                                      model =  SuperModel(
                                      #NNetPCAModel_ptsGame,
                                      #glmnet_ptsGame1,
                                      glmnet_ptsGame2,
                                      #xgb_ptsGame1,
                                      xgb_ptsGame2,
                                      EarthModel(degree = 2,nprune = 216),
                                      model = make_glmerStackedModel(
                                        link = 'linear',
                                        incl_random_efx = T,
                                        tau_0 = 1,
                                        est_dispersion = T,
                                        min_learners = 3,
                                        Z = Z,
                                        sigma_list = sigma_list,
                                        use_qp = T,
                                        fixed_weights = F,
                                        weights = NULL
                                      ),
                                      control = CVControl(folds = 20, seed = 52245))
                                    ) 
                                    stacked_stan <- stacked_model %>%
                                      quantify_stacked_uncertainty(return_fit = F,
                                                                   iter = 10000,
                                                                   warmup = 1000,
                                                                   take_a_break_first = T,
                                                                   filename = paste0('ptsGame_stan.RData'),
                                                                   verbose = F,
                                                                   mu_eta = mean(rec$template$ptsGame),
                                                                   sigma_eta = sd(rec$template$ptsGame)) # set as marginal distr. of points scored
                                    save(stacked_model,file = 'stacked_model_gaussian.RData')
                                    save(stacked_stan, file = 'stacked_stan_gaussian.RData')
                                    stopCluster(cl2)
                                  } else if(ijk == 2){
                                    
                                    
                                    #Sys.sleep(120)
                                    
                                    ################################################################################
                                    ## Quasi-Poisson Regression
                                    # rm(list = ls()))
                                    # source("set_optim_thompson.R")
                                    # library(MachineShop)
                                    # library(recipes)
                                    # library(pander)
                                    # library(parallel)
                                    # library(foreach)
                                    # library(doParallel)
                                    # library(quadprog)
                                    # library(Matrix)
                                    # library(MASS)
                                    # #source('AdHocAbsStacked.R')
                                    # library(rstan)
                                    # library(StanHeaders)
                                    # load('prep_fxn.RData')
                                    # load('recipes_playoffs.RData')
                                    # load('candidate_models.RData')
                                    cl2 <- makeCluster(10)
                                    registerDoParallel(cl2)
                                    ##
                                    rec <- prep_list_log$rec
                                    Z <- prep_list_log$Z
                                    sigma_list <- prep_list_log$sigma_list
                                    no_featreduc_vars <- c('year.x',
                                                           'numberGameTeamSeason.x',
                                                           'numberGameTeamSeason.y',colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
                                    clusterExport(cl2,c(c(as.vector(lsf.str())),
                                                        'rec',
                                                        'Z',
                                                        'sigma_list',
                                                        'solve.QP',
                                                        'no_featreduc_vars'))
                                    
                                    stacked_model <- fit(rec,model =  SuperModel(
                                      #NNetPCAModel_logPtsGame,
                                      #glmnet_log1,
                                      glmnet_log2,
                                      xgb_log1,
                                      #xgb_log2,
                                      EarthModel(degree = 2,nprune = 216),
                                      model = make_glmerStackedModel(
                                        link = 'log',
                                        incl_random_efx = T,
                                        tau_0 = 1,
                                        est_dispersion = T,
                                        min_learners = 2,
                                        Z = Z,
                                        sigma_list = sigma_list,
                                        use_qp = T
                                      ),
                                      all_vars = F,
                                      control = CVControl(folds = 20, seed = 52245))
                                    ) 
                                    stacked_stan <- stacked_model %>%
                                      quantify_stacked_uncertainty(return_fit = F,
                                                                   iter = 20000,
                                                                   warmup = 2000,
                                                                   take_a_break_first = T,
                                                                   filename = paste0('logPtsGame_stan.RData'),
                                                                   verbose = F,
                                                                   mu_eta = mean(rec$template$logPtsGame),
                                                                   sigma_eta = sd(rec$template$logPtsGame))
                                    #fit <- stacked$fit
                                    #stan <- stacked$stan
                                    #save(stan,file = ' logPtsGame_stan.RData')
                                    #save(fit,file = 'quasipoisson_fit.RData')
                                    # pander(rbind(c("Weights for Selected Base Learners (Quasi-Poisson): ",
                                    #                c(fit$super_fit$data$final_fit$weights))))
                                    save(stacked_model,file = 'stacked_model_log.RData')
                                    save(stacked_stan, file = 'stacked_stan_log.RData')
                                    # traceplot(stacked$stan.result)
                                    stopCluster(cl2)
                                  } else if(ijk == 3){
                                    
                                    
                                    ################################################################################
                                    # Logistic Regression for Any Overtime
                                    # rm(list = ls()))
                                    # source("C:/Users/defgi/Documents/set_optim_thompson.R")
                                    # library(MachineShop)
                                    # library(recipes)
                                    # library(pander)
                                    # library(parallel)
                                    # library(foreach)
                                    # library(doParallel)
                                    # library(quadprog)
                                    # library(Matrix)
                                    # library(MASS)
                                    # #source('AdHocAbsStacked.R')
                                    # library(rstan)
                                    # library(StanHeaders)
                                    # load('prep_fxn.RData')
                                    # load('recipes_playoffs.RData')
                                    # load('candidate_models.RData')
                                    cl2 <- makeCluster(10)
                                    registerDoParallel(cl2)
                                    ##
                                    rec <- prep_list_overtime$rec
                                    rec$template$overtime <- factor(as.character(ifelse(rec$template$overtime == 240,
                                                                                        0,
                                                                                        1)))
                                    Z <- prep_list_overtime$Z
                                    sigma_list <- prep_list_overtime$sigma_list
                                    ##
                                    no_featreduc_vars <- c('year.x',
                                                           'numberGameTeamSeason.x',
                                                           'numberGameTeamSeason.y',
                                                           colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars != "overtime"]
                                    clusterExport(cl2,c(c(as.vector(lsf.str())),
                                                        'rec',
                                                        'Z',
                                                        'sigma_list',
                                                        'no_featreduc_vars'))
                                    
                                    stacked_model <- fit(rec,model =  SuperModel(
                                      #NNetPCA,
                                      glmnet_overtime1,
                                      glmnet_overtime2,
                                      xgb_overtime1,
                                      xgb_overtime2,
                                      EarthModel(degree = 2,nprune = 216),
                                      model = make_glmerStackedModel(
                                        link = 'logit',
                                        incl_random_efx = T,
                                        tau_0 = 1,
                                        est_dispersion = F,
                                        min_learners = 2,
                                        Z = Z,
                                        sigma_list = sigma_list,
                                        use_qp = T,
                                        fixed_weights = F,
                                        weights = NULL
                                      ),
                                      all_vars = F,
                                      control = CVControl(folds = 20, seed = 52245))
                                    ) 
                                      stacked_stan <- stacked_model %>% quantify_stacked_uncertainty(return_fit = F,
                                                                   iter = 30000,
                                                                   warmup = 2500,
                                                                   take_a_break_first = F,
                                                                   mu_eta = log(mean(as.character(rec$template$overtime) != "0")/(
                                                                     1- mean(as.character(rec$template$overtime) != "0")
                                                                   )),
                                                                   filename = paste0('overtime_stan.RData'),
                                                                   verbose = T)
                                   # fit <- stacked$fit
                                    save(stacked_model,file = 'stacked_model_overtime.RData')
                                    save(stacked_stan, file = 'stacked_stan_overtime.RData')
                                    # pander(rbind(c("Weights for Selected Base Learners (Softmax): ",
                                    #                c(fit$super_fit$data$final_fit$weights))))
                                    # traceplot(stacked$stan.result)
                                    stopCluster(cl2)
                                  } else if(ijk == 4){
                                    
                                    ################################################################################
                                    # Linear Regression for Spread: Home - Away Team
                                    # rm(list = ls()))
                                    # source("C:/Users/defgi/Documents/set_optim_thompson.R")
                                    # library(MachineShop)
                                    # library(recipes)
                                    # library(pander)
                                    # library(parallel)
                                    # library(foreach)
                                    # library(doParallel)
                                    # library(quadprog)
                                    # library(Matrix)
                                    # library(MASS)
                                    # #source('AdHocAbsStacked.R')
                                    # library(rstan)
                                    # library(StanHeaders)
                                    # load('prep_fxn.RData')
                                    # load('recipes_playoffs.RData')
                                    # load('candidate_models.RData')
                                    cl2 <- makeCluster(10)
                                    registerDoParallel(cl2)
                                    ##
                                    rec <- prep_list_spread$rec
                                    Z <- prep_list_spread$Z
                                    sigma_list <- prep_list_spread$sigma_list
                                    ##
                                    no_featreduc_vars <- c('year.x',
                                                           'numberGameTeamSeason.x',
                                                           'numberGameTeamSeason.y',
                                                           colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
                                    clusterExport(cl2,c(c(as.vector(lsf.str())),
                                                        'rec',
                                                        'Z',
                                                        'sigma_list',
                                                        'no_featreduc_vars'))
                                    
                                    stacked_model <- fit(rec,model =  SuperModel(
                                      #NNetPCAModel_spread,
                                      glmnet_spread1,
                                      #glmnet_spread2,
                                      xgb_spread1,
                                      #xgb_spread2,
                                      EarthModel(degree = 2,nprune = 216),
                                      model = make_glmerStackedModel(
                                        link = 'linear',
                                        incl_random_efx = T,
                                        tau_0 = 1,
                                        est_dispersion = T,
                                        min_learners = 3,
                                        Z = Z,
                                        sigma_list = sigma_list,
                                        use_qp = T,
                                        fixed_weights = F,
                                        weights = NULL
                                      ),
                                      all_vars = F,
                                      control = CVControl(folds = 20, seed = 52245))
                                    ) 
                                    stacked_stan <- stacked_model %>%
                                      quantify_stacked_uncertainty(return_fit = F,
                                                                   iter = 10000,
                                                                   warmup = 1000,
                                                                   take_a_break_first = T,
                                                                   filename = paste0('spread_stan.RData'),
                                                                   verbose = F,
                                                                   mu_eta = mean(rec$template$spreadDiff),
                                                                   sigma_eta = sd(rec$template$spreadDiff))
                                    save(stacked_model,file = 'stacked_model_spread.RData')
                                    save(stacked_stan, file = 'stacked_stan_spread.RData')
                                    # pander(rbind(c("Weights for Selected Base Learners (Softmax): ",
                                    #                c(fit$super_fit$data$final_fit$weights))))
                                    # traceplot(stacked$stan.result)
                                    stopCluster(cl2)
                                  } else if(ijk == 5){
                                    
                                    ################################################################################
                                    # Linear Regression for Arcsin-Square Root Prop Points Scored by Home Team
                                    # rm(list = ls()))
                                    # source("C:/Users/defgi/Documents/set_optim_thompson.R")
                                    # library(MachineShop)
                                    # library(recipes)
                                    # library(pander)
                                    # library(parallel)
                                    # library(foreach)
                                    # library(doParallel)
                                    # library(quadprog)
                                    # library(Matrix)
                                    # library(MASS)
                                    # #source('AdHocAbsStacked.R')
                                    # library(rstan)
                                    # library(StanHeaders)
                                    # load('prep_fxn.RData')
                                    # load('recipes_playoffs.RData')
                                    # load('candidate_models.RData')
                                    cl2 <- makeCluster(10)
                                    registerDoParallel(cl2)
                                    ##
                                    rec <- prep_list_arcsin$rec
                                    Z <- prep_list_arcsin$Z
                                    sigma_list <- prep_list_arcsin$sigma_list
                                    ##
                                    no_featreduc_vars <- c('year.x',
                                                           'numberGameTeamSeason.x',
                                                           'numberGameTeamSeason.y',
                                                           colnames(rec$template)[apply(rec$template,2,function(i)length(unique(i))==2)])
                                    no_featreduc_vars <- no_featreduc_vars[no_featreduc_vars %in% colnames(rec$template)]
                                    clusterExport(cl2,c(c(as.vector(lsf.str())),
                                                        'rec',
                                                        'Z',
                                                        'sigma_list',
                                                        'no_featreduc_vars'))
                                    
                                    stacked_model <- fit(rec,model =  SuperModel(
                                      NNetPCAModel_arcsin,
                                      #glmnet_arcsin1,
                                      #glmnet_arcsin2,
                                      xgb_arcsin1,
                                      #xgb_arcsin2,
                                      EarthModel(degree = 2,nprune = 216),
                                      model = make_glmerStackedModel(
                                        link = 'linear',
                                        incl_random_efx = T,
                                        tau_0 = 1,
                                        est_dispersion = T,
                                        min_learners = 3,
                                        Z = Z,
                                        sigma_list = sigma_list,
                                        use_qp = F,
                                        fixed_weights = F,
                                        weights = NULL
                                      ),
                                      all_vars = F,
                                      control = CVControl(folds = 20, seed = 52245))
                                    ) 
                                    stacked_stan <- stacked_model %>%
                                      quantify_stacked_uncertainty(return_fit = F,
                                                                   iter = 10000,
                                                                   warmup = 1000,
                                                                   take_a_break_first = T,
                                                                   filename = paste0('arcsin_stan.RData'),
                                                                   verbose = F,
                                                                   mu_eta = mean(rec$template$arcsinsqrt_prop),
                                                                   sigma_eta = sd(rec$template$arcsinsqrt_prop))
                                    save(stacked_model,file = 'stacked_model_arcsin.RData')
                                    save(stacked_stan, file = 'stacked_stan_arcsin.RData')
                                    # pander(rbind(c("Weights for Selected Base Learners (Softmax): ",
                                    #                c(fit$super_fit$data$final_fit$weights))))
                                    # traceplot(stacked$stan.result)
                                    stopCluster(cl2)
                                  }
                                }