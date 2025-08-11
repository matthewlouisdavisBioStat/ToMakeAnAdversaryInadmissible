set.seed(52245)
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
library(MachineShop)
library(recipes)
library(pander)
library(parallel)
library(foreach)
library(doParallel)
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/AdHocAbsStacked.R')
library(lubridate)
library(rstan)
outcome <- 'arcsin'

## load model 
load(paste0('stacked_model_',outcome,'.RData'))
fit <- stacked_model
rm(stacked_model)
strt <- Sys.time()
loglik_metric <- make_loglikelihood_MLMetric(fit$super_fit$data$link_function,
                                             fit$super_fit$data$final_fit$tau^(1+(outcome == 'log' | outcome == 'overtime')),# tau^2 for other distr. besides gaussian
                                             T)
## feature reduc vars
load('no_featreduc_vars.RData')
no_featreduc_vars <- no_featreduc_vars[!(no_featreduc_vars == 'numberGameTeamSeason.x')]
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

# permuted psuedo-Wilk's test, with BH adjustment
pvals <- p.adjust((1-pchisq(vrpm$`Permute.mean.log-likelihood`, df = 1)), 'BH')
passes_chi2 <- rownames(vrpm)[pvals <=1 & vrpm > 1e-8]

save(passes_chi2, file = paste0(outcome,'_vars2keep.RData'))
