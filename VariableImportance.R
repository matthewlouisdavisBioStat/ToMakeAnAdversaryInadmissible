set.seed(52245)
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
library(MachineShop)
library(recipes)
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/AdHocAbsStacked.R')

## choose response to provide variable importance for
outcome <- 'arcsin'

## load model 
load(paste0('stacked_model_',outcome,'.RData'))
fit <- stacked_model
rm(stacked_model)
strt <- Sys.time()
loglik_metric <- make_loglikelihood_MLMetric(fit$super_fit$data$link_function,
                                             fit$super_fit$data$final_fit$tau^(1+(outcome == 'log' | outcome == 'overtime')),# tau^2 for other distr. besides gaussian
                                             T)
## variables to ignore
load('no_featreduc_vars.RData')
no_featreduc_vars <- no_featreduc_vars[!(no_featreduc_vars == 'numberGameTeamSeason.x')]

## run permutation importance with 4 repeats, log-likelihood as response
vrpm <-
  varimp(fit,
         metric = loglik_metric,
         scale = F,
         sample = 4)
plot(vrpm)
save(vrpm,
     file = 
       paste0('dec_varimp_',
              outcome,
              ".RData"))
fn <- Sys.time()
print(strt - fn)
