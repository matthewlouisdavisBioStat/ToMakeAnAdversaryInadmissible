#' Parameterize and quantify uncertainty of stacked models for deriving posterior predictive distributions.
#' @param fit a fit object returned by running the SuperModel() function with make_glmerStackedModel() as the super learner.
#' @param chain number of chains to include for sampling.
#' @param iter number of iterations per chain.
#' @param warmup number of warmup iterations per chain.
#' @param cores number of cores to use when running in parallel.
#' @param prior_sigmaSq_etahat the prior distribution to impose on the sigmaSq_etahat, the parameter that captures the uncertainty of the form of the model conditional upon the observed data
#' @param prior_dispersion the prior distribution to impose on the dispersion parameter of an exponential family model (i.e. sigma squared for normal distribution) for sampling purposes
#' @param filename .RData file to save posterior draws to.
#' @param return_fit T or F, whether to return the stacked model's fit along with the posterior draws
#' @param stopImplicitCluster_option T or F, whether to automatically stop any other active running clusters at the time of sampling
#' @param take_a_break_first T or F, whether to have the computer wait 2 minutes before starting sampling.
#' @param verbose T or F, whether rstan verbose ouput should be printed to the console while running (defaults to T).
#' @mu_eta Scalar or vector of prior means - defaults to vector of observed outcomes otherwise
#' @sigma_eta Scalar or vector of prior means - defaults to 5*sd(Y) otherwise
#'
#' @details given a stacked model as fit using the procedures above paired with SuperModel(), this determines a parameter sigmaSq_etahat that captures uncertainty across past observations that can be be used to construct future posterior predictive intervals. For REF families with some dispersion parameter (i.e. for Weibull, Normal) this is estimated as well.
#'
#' @import rstan
#' @import doParallel
#' @return rstan output and saved posterior draws.
#'
#' @examples
#' \donttest{
#'
#'  q <- quantify_stacked_uncertainty(fit,
#'                              return_fit = T,
#'                              iter = 1000,
#'                              chain = 1,
#'                               cores = 2,
#'                                warmup = 100,
#'                                prior_sigmaSq_etahat = "gamma(1,1)",
#'                                prior_dispersion = "inv_gamma(1,1)",
#'                               filename = "boilerplate_template.RData")
#'  traceplot(q$stan.result)
#' }
#' @export
quantify_stacked_uncertainty <- function(fit,
                                         chain = 2,
                                         iter = 25000,
                                         warmup = 1000,
                                         cores = parallel::detectCores() / 2,
                                         prior_sigmaSq_etahat = paste0('inv_gamma(1,1)'),
                                         prior_dispersion = NULL,
                                         filename = NULL,
                                         return_fit = F,
                                         stopImplicitCluster_option = T,
                                         take_a_break_first = F,
                                         mu_eta = NULL,
                                         sigma_eta = NULL,
                                         verbose = T) {
  if(stopImplicitCluster_option){
    stopImplicitCluster()
  }
  if(take_a_break_first){
    Sys.sleep(120)
  }
  require(rstan)
  incl_random_efx <- ifelse2(sum(abs(fit$super_fit$data$Zmat)) == 0,
                             F,
                             T)
  ## data taken from model fit object
  Y <- response(fit)
  incl_random_efx <-
    any(as.numeric(unlist(
      fit$super_fit$data$final_fit$u_update
    )) != 0)
  link_function <- fit$super_fit$data$link_function
  if(link_function %in% c('softmax','logit')){
    preds <- predict(fit, type = 'prob')
  } else if(link_function %in% c('linear','log')){
    preds <- predict(fit, type = 'numeric')
  } else {
    preds <- predict(fit)
  }

  ## stan setup
  options(mc.cores = cores)
  rstan_options(auto_write = TRUE)

  ## parameterize for different link functions
  l <- fit$super_fit$data$link_function
  if (l == "linear") {
    link_likelihood <- 'Y ~ normal(eta,sqrt(dispersion))'
    etahat <- preds
  } else if (l == 'logit') {
    link_likelihood <- 'Y ~ bernoulli_logit(eta);'
    etahat <- log(preds / (1 - preds))
    Y <- as.numeric(Y)-1
  } else if (l == 'log') {
    dispersion <- fit$super_fit$data$final_fit$tau^2
    ## for Exponential Dispersion Poisson
    link_likelihood <- paste0(
      "for (nn in 1:N) {
                                      target += Y[nn]*eta[nn]*",
      1 / dispersion,
      " - exp(eta[nn])*",
      1 / dispersion,
      ";
                              }"
    )
    etahat <- log(preds)
  } else if (l == 'probit') {
    link_likelihood <-  paste0('for (nn in 1:N){
                                Y[nn] ~ bernoulli(Phi_approx(eta[nn]));
                               }')
    etahat <- qnorm(preds)
    Y <- as.numeric(Y)-1
  } else if (l == 'inverse') {
    link_likelihood <-  'Y ~ exponential(-1/eta);'
    etahat <- -1 / preds
  } else if (l == 'weibull') {
    Yall <- (sapply(Y, function(y) {
      y <- as.character(y)
      if (grepl("[+]", y))
        y <- substr(y, 1, nchar(y) - 1)
      as.numeric(y)
    }))
    Y <- as.numeric(Yall)[1:length(Y)]
    censored <- 1 - as.numeric(Yall)[-c(1:length(Y))]
    etahat <- log(as.numeric(preds))
  } else if (l == 'softmax') {
    Y <- as.numeric(Y)
    etahat <- t(apply(preds, 1, function(x)
      log(x / (sum(
        x
      )))))
  }
  ## linear has special case for which explicit integration-out of eta is possible
  if (l == 'linear') {
    stan.model <- paste0(
      "
data{
  int N;     // Total number of observations
  real Y[N]; // Numeric Outcome
  real etahat[N]; // Predicted etahat
  real mu_eta[N]; // hyperparameter of prior normal expectation on eta
  real<lower=0> sigmaSq_eta; // hyperparameter of prior normal variance on eta
}
parameters{
  ",paste0(ifelse2(is.null(prior_dispersion),
  "real<lower=0> tau",
  "real<lower=0> dispersion")),";
  real<lower=0> sigmaSq_etahat;
}
transformed parameters{
  ",paste0(ifelse2(is.null(prior_dispersion),
                   "real<lower=0> dispersion;",
                   "")),"
  real<lower=0> sigmaSq_1;
  real mu_1[N];
  ",paste0(ifelse2(is.null(prior_dispersion),
                   "dispersion = 1/tau;",
                   "")),"
  sigmaSq_1 = 1/(1/dispersion + 1/sigmaSq_eta);
  for(nn in 1:N){
    mu_1[nn] = (mu_eta[nn]/sigmaSq_eta + Y[nn]/dispersion)*sigmaSq_1;
  }
}
model {
  sigmaSq_etahat ~ ", prior_sigmaSq_etahat, ";
  ",
        paste0(ifelse2(is.null(prior_dispersion),
           paste0('tau ~ gamma(',1.5,',',(fit$super_fit$data$final_fit$tau ^ 2),')'),
           paste0('dispersion ~ ',prior_dispersion))),";
  etahat ~ normal(mu_1,sqrt(sigmaSq_etahat+sigmaSq_1));
}
")
    stan.data <- list(
      N = length(Y),
      Y = Y,
      etahat = etahat,
      mu_eta = ifelse2(!is.null(mu_eta),rep(mu_eta,length(Y)),as.numeric(Y)),#mean(Y),
      sigmaSq_eta = ifelse2(!is.null(sigma_eta),sigma_eta^2,var(Y)*5)
    )
    print('starting stan')
    system.time({
      stan.result <- try({
        stan(
        model_code = stan.model,
        data = stan.data,
        chain = chain,
        iter = iter,
        warmup = warmup,
        pars = c("dispersion", "sigmaSq_etahat"),
        verbose = verbose
      )},silent=T)
    print('finished stan')
    if(class(stan.result) == 'try-error'){
      warning("Stan Error: Try a different prior?")
      print(stan.result)
    }
    })
  } else if (l == 'softmax') {
    stan.model <- paste0(
      "
data{
  int N;     // Total number of Observations
  int M;     // Total number of Categories - 1
  int K;     // Total number of Categories
  int Y[N]; // Numeric Outcome
  real etahat[N,M]; // Predicted etahat
  real mu_eta[N,M]; // Mean of eta(s)
  real sigma_eta; // SD of eta
}
parameters{
  matrix[N,M] eta;
  real<lower=0> sigmaSq_etahat;
}
transformed parameters{
  matrix[M+1,N] theta;
  matrix[N,M+1] eta2 = append_col(rep_vector(0,N),eta);
  for(nn in 1:N){  // Begin Workaround for Softmaxing a K-1 Simplex
    theta[,nn] = softmax(eta2[nn,]'); // exp(z)/(1+sum(exp(z))) equivalent
    theta[1,nn] = 1;
    for(mm in 1:M){
      theta[1,nn] += -theta[1+mm,nn];
    }
  }
}
model {
    sigmaSq_etahat ~ ", prior_sigmaSq_etahat, ";
    for(mm in 1:K){
      if(mm < M){
        etahat[,mm] ~ normal(eta[,mm],sqrt(sigmaSq_etahat));
        eta[,mm] ~ normal(mu_eta[,mm],sigma_eta);
      }
      for(nn in 1:N){
        if(Y[nn] == mm){
          target += Y[nn]*log(theta[mm,nn]);
        }
      } // Y[nn] ~ categorical(theta[,nn]);
    }
}
")
    stan.data <- list(
      N = length(Y),
      M = length(unique(Y))  -  1,
      K = length(unique(Y)),
      Y = Y,
      ## WATCH THIS! INSTEAD OF TRUE OUTCOME, THIS IS CENTERED ABOUT THE PREDICTED ONE LEADING TO OVER CONF
      ## WE SHOULD FIX THIS ONE DAY..........
      mu_eta = ifelse2(!is.null(mu_eta),mu_eta,etahat[,-1]),#colMeans(etahat)[-1],
      etahat = etahat[,-1],
      sigma_eta = sd(etahat)  *  2
    )
    system.time({
      stan.result <- try({stan(
        model_code = stan.model,
        data = stan.data,
        chain = chain,
        iter = iter,
        warmup = warmup,
        pars = c("sigmaSq_etahat"),
        verbose = verbose
      )},silent=T)
      if(class(stan.result) == 'try-error'){
        warning("Stan Error: Try a different prior?")
      }
    })
  } else if (l != 'weibull') {
    stan.model <- paste0(
      "
data{
  int N;     // Total number of observations
  ",paste0(ifelse2(l %in% c('logit','probit'),"int Y[N]","real Y[N]")),"; // Numeric Outcome
  real etahat[N]; // Predicted etahat
  real mu_eta[N]; // Mean of eta
  real sigma_eta; // Fixed sd of eta
}
parameters{
  vector[N] eta;
  real<lower=0> sigma_etahat;
}
model {
  sigma_etahat ~ inv_gamma(2,25);
  target += normal_lpdf(etahat | eta, sigma_etahat);
  target += normal_lpdf(eta | mu_eta, sigma_eta);
  ", link_likelihood, "
}
")
    stan.data <- list(
      N = length(Y),
      Y = as.numeric(Y),
      etahat = etahat,
      mu_eta = ifelse2(!is.null(mu_eta),rep(mu_eta,length(Y)),eta_hat),#mean(etahat),
      sigma_eta = ifelse2(!is.null(sigma_eta),sigma_eta,sd(etahat)*5)
    )
    system.time({
      stan.result <- try({stan(
        model_code = stan.model,
        data = stan.data,
        chain = chain,
        iter = iter,
        warmup = warmup,
        pars = c("sigma_etahat"),
        verbose = verbose
      )},silent=T)
      if(class(stan.result) == 'try-error'){
        warning("Stan Error: Try a different prior?")
      }
    })
  } else if (l == 'weibull') {
    stan.model <- paste0(
      "
data{
  int N;     // Total number of Observations
  real Y[N]; // Numeric for observed deaths
  int censored[N]; // Indicators for who is and isn't censored (1, 0 respectively)
  real etahat[N]; // Predicted etahat
  real mu_eta[N]; // Mean of eta
  real sigma_eta; // SD of eta
}
parameters{
  vector[N] eta;
  real<lower=0> sigmaSq_etahat;
  real<lower=0> dispersion;
}
model {
    sigmaSq_etahat ~ ", prior_sigmaSq_etahat, ";
    dispersion ~ ",
        paste0(ifelse2(is.null(prior_dispersion),
        paste0('gamma(',fit$super_fit$data$final_fit$tau ^ 2,',1)'),
        prior_dispersion)),";
    etahat ~ normal(eta,sqrt(sigmaSq_etahat));
    for(nn in 1:N){
      eta ~ normal(mu_eta[nn],sigma_eta);
      if(censored[nn] < 1){
        target += weibull_lpdf(Y[nn] | 1/dispersion, exp(eta[nn]/dispersion));
      }
      if(censored[nn] > 0){
        target += weibull_lccdf(Y[nn] | 1/dispersion, exp(eta[nn]/dispersion));
      }
    }
}
")
    stan.data <- list(
      N = length(Y),
      Y = Y,
      censored = censored,
      mu_eta = ifelse2(!is.null(mu_eta),rep(mu_eta,length(Y)),Y),#mean(Y),
      etahat = etahat,
      sigma_eta = ifelse2(!is.null(sigma_eta),sigma_eta,sd(etahat)  *  5)
    )
    system.time({
      stan.result <- try({stan(
        model_code = stan.model,
        data = stan.data,
        chain = chain,
        iter = iter,
        warmup = warmup,
        pars = c("sigmaSq_etahat", "dispersion"),
        verbose = verbose
      )},silent = TRUE)
      if(class(stan.result) == 'try-error'){
        warning("Stan Error: Try a different prior?")
      }
    })
  }
if(is.null(filename)){
  save(stan.result, file = paste0('stan_result_default.RData'))
} else{
  save(stan.result, file = filename)
}
if (!return_fit) {
  return(stan.result)
} else{
  return(list('stan.result' = stan.result,
              'fit' = fit))
}
}


