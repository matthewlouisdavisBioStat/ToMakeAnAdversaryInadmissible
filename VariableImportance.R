set.seed(52245)
setwd('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles')
#setwd('Z:/Dissertation/Stacking')
## rm(list = ls())
library(MachineShop)
library(recipes)
library(pander)
library(parallel)
library(foreach)
library(doParallel)
source('C:/Users/defgi/Documents/AbsolutelyStackedSupplementaryFiles/AdHocAbsStacked.R')
library(lubridate)
library(rstan)
# for(k in c('glmer_constrained.R', 
#            'HelperFunctions.R', 
#            'make_dummy_Z_and_sigmalist.R',
#            'make_glmerStackedModel.R', 
#            'make_loglikelihood_MLMetric.R',
#            'MLModel_DataPrep.R', 
#            'predict_glmerStacked.R',
#            'quantify_stacked_uncertainty.R', 
#            'SoftmaxOperations.R', 
#            'stepAIC_stacked.R')){
#   source(k)
# }
####
outcome <- 'arcsin'
####

## load model 
load(paste0('stacked_model_',outcome,'.RData'))
fit <- stacked_model
rm(stacked_model)
# cl <- makeCluster(5)
# registerDoParallel(cl)
strt <- Sys.time()
loglik_metric <- make_loglikelihood_MLMetric(fit$super_fit$data$link_function,
                                             fit$super_fit$data$final_fit$tau^(1+(outcome == 'log' | outcome == 'overtime')),# tau^2 for other distr. besides gaussian
                                             T)
## feature reduc vars
load('no_featreduc_vars.RData')
no_featreduc_vars <- no_featreduc_vars[!(no_featreduc_vars == 'numberGameTeamSeason.x')]

#####################################
# clusterExport(cl,unique(c('no_featreduc_vars',
#                   c(as.vector(lsf.str()),
#                     'bake',
#                     'prep',
#                     'step_kmeans',
#                     'loglik_metric',
#                     'juice',
#                    'all_of',
#                    'all_numeric_predictors',
#                    'recipe',
#                    'step_normalize'))))
vrpm <- #foreach(i = 1:5,
        #        .packages = c('MachineShop',
        #                      'recipes',
        #                     'foreach',
        #                     'doParallel')) %dopar% {
  # cl2 <- makeCluster(2)
  # registerDoParallel(cl2)
  # clusterExport(cl2,unique(c('no_featreduc_vars',
  #                           c(as.vector(lsf.str()),
  #                             'bake',
  #                             'prep',
  #                             'step_kmeans',
  #                             'loglik_metric',
  #                             'juice',
  #                             'all_of',
  #                             'all_numeric_predictors',
  #                             'recipe',
  #                             'step_normalize'))))
  #v <- 
  varimp(fit,
         metric = loglik_metric,
         scale = F,
         sample = 4)
  #stopCluster(cl2)
  #v
#}



plot(vrpm)


save(vrpm,
     file = 
       paste0('dec_varimp_',
              outcome,
              ".RData"))
fn <- Sys.time()
print(strt - fn)
# stopCluster(cl)

# permuted psuedo-Wilk's test, with BH adjustment
pvals <- p.adjust((1-pchisq(vrpm$`Permute.mean.log-likelihood`, df = 1)), 'BH')
passes_chi2 <- rownames(vrpm)[pvals <=1 & vrpm > 1e-8]
save(passes_chi2, file = paste0(outcome,'_vars2keep.RData'))