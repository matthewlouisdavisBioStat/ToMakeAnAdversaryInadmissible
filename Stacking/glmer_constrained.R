#' Fit generalized linear models and conditional mixed-effects models using constrained maximum likelihood estimation.
#' @param link_function character, what link function is being used to model the GLM. Options include: linear, logit, log, probit, weibull, softmax, or inverse
#' @param X a list of matrices for softmax regression, a matrix for all other link functions, corresponding to fixed effects (the effect subject to constraint)
#' @param Y a matrix for softmax regression, a vector for all other link functions, corresponding to the outcome of interest
#' @param Z a list of matrices for softmax regression, a matrix for all other link functions, corresponding to random effects to be estimated
#' @param tau exponential dispersion parameter, fixed if est_tau == F
#' @param est_tau T or F, should the exponential dispersion parameter be estimated?
#' @param sigma vector of numeric elements corresponding to intial values for the variance components associated with random effects.
#' @param beta_update a vector corresponding to the initial values of the fixed effects of interest.
#' @param u_update a vector corresponding to the intial values of the random effects of interest
#' @param lambda manually-specified l2-norm penalty: this appears in the score and information, but is excluded when computing the log-likelihood. Defaults to 0, but set to 0.000001 for make_glmerStackedModel().
#' @param sigma_list a list named by each random-effect variance parameter, each element of which is a vector containing the column indices of the Z matrix that correspond to each random-effect variance component.
#' @param use_quadprog T or F, should sequential quadratic programming using the quadprog package (T) or a general non-linear programmming algorithm using the Rsolnp package (F) be used to find MLEs under constraint. Defaults to F.
#' @param sum_constraint real number referring to the sum-constraint on fixed effect coefficients.
#' @param lower_bound_constraint real number (or vector with length equal to the length beta_update AND the number of columns of X) defining the lower bound constraint on fixed effect coefficients.
#' @param step_penalty_function  function with consecutive arguments for number of base learners and number of observations, for computing the stepwise base learner selection penalty. function(k,n){2*k} yields traditional AIC. Defaults to 0.
#'
#' @details First, generalized linear mixed effects models are fit using unconstrained maximum likelihood estimation via a damped Newton-Raphson algorithm. Coefficients are then updated to reflect the desired constraints, with initial values supplied to the functions based off of the unconstrained MLEs.
#'
#' @return A list of elements including constrained and unconstrained estimates of MLE weights.
#'
#' @importFrom quadprog solve.QP
#' @importFrom Matrix nearPD
#'
#' @seealso \code{\link[quadprog]{solve.QP}}
#'
#'
#' @examples
#' \donttest{
#'
#' ## Simulate Data
#' library(magrittr)
#' N <- 1000
#' Xt <- cbind((runif(N)),(runif(N)),(runif(N)),(runif(N)))
#' Bt <- 2*c(-.25,0,0.25,0.75)
#' Zt <- kronk(lapply(1:10,function(i){
#'   cbind(rep(1,N/10))
#' }))
#' Ut <- rnorm(10,0,2)
#' ## sse estimate of sigma^2 for observed error term for intializing (N-qr(Xt)$rank))
#' Mut <- Xt %*% Bt + Zt %*% Ut
#' Yt <- sapply(Mut,function(mu)rnorm(1,mu))
#' tau <- sd(Yt) ## for intializing only.....
#'
#'
#' ## setup for random efx
#' Z <- Zt
#' sigma_list <- list("sigma_1" = 1:(ncol(Z)))
#' names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
#' for(i in 1:length(sigma_list)){
#'   names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
#'                                 length(sigma_list[[i]]))
#' }
#' sigma <- 1 ## for initializing only
#' names(sigma) <- names(sigma_list)
#'
#' ## Constrained Maximum Likelihood-Based stacking
#' stacked_output_solnp <- glmer_constrained(
#'   link_function = 'linear',
#'   X = Xt,
#'   Y = Yt,
#'   Z = Z,
#'   tau = 1,
#'   est_tau = T,
#'   sigma = sigma,
#'   beta_update = rep(1/ncol(Xt),ncol(Xt)),
#'   u_update = rep(0,ncol(Z)),
#'   lambda = 0,
#'   sigma_list = sigma_list,
#'   use_quadprog = F,
#'   sum_constraint = sum(Bt),
#'   lower_bound_constraint = c(-.5,0,0,0)
#' )
#' stacked_output_qp <- glmer_constrained(
#'   link_function = 'linear',
#'   X = Xt,
#'   Y = Yt,
#'   Z = Z,
#'   tau = 1,
#'   est_tau = T,
#'   sigma = sigma,
#'   beta_update = rep(1/ncol(Xt),ncol(Xt)),
#'   u_update = rep(0,ncol(Z)),
#'   lambda = 0,
#'   sigma_list = sigma_list,
#'   use_quadprog = T,
#'   sum_constraint = sum(Bt),
#'   lower_bound_constraint = c(-.5,0,0,0)
#' )
#'
#' ## GLM (requires installation of lme4 for testing purposes)
#' Z2 <- apply(Z,1,function(z){
#'   sum(sapply(1:10,function(i){
#'     i*(z[i]==1)
#'   }))
#' })
#' temp_data <- as.data.frame(cbind(Yt,Xt,Z2))
#' colnames(temp_data) <- c(
#'   "Y",
#'   paste0("X",1:ncol(Xt)),
#'   "Z"
#' )
#' fitLMERraw <- lme4::lmer(as.formula(
#'   "Y ~ 0 + X1 + X2 + X3 + X4 + (1|Z)"
#' ),data = temp_data)
#' fitLMER <- fitLMERraw %>%
#'   summary
#' u_hat <- (fitLMERraw %>% coef)[[1]][,1]
#' mle <- c(fitLMER$coefficients[,1])
#'
#'
#' res <- data.frame(c(Bt,Ut),
#'                   round(c(stacked_output_solnp$beta_update,stacked_output_solnp$u_update),4),
#'                   round(c(stacked_output_qp$beta_update,stacked_output_qp$u_update),4),
#'                   round(c(stacked_output_solnp$beta_mle,stacked_output_solnp$u_update),4),
#'                   round(c(mle,u_hat),4))
#' colnames(res) <- c("True Betas",
#'                    "Constrained MLE Rsolnp",
#'                    "Constrained MLE quadprog",
#'                    "Unconstrained MLE glmer_constrained",
#'                    "Unconstrained MLE lmer")
#' rownames(res) <- c("Beta1", "Beta2" ,"Beta3","Beta4",paste0("RandInt",1:10))
#' print(res)
#' }
#' @export
glmer_constrained <- function(link_function,
                              X,
                              Y,
                              Z,
                              tau,
                              est_tau = F,
                              sigmas = 1,
                              beta_update,
                              u_update,
                              lambda = 50*sqrt(.Machine$double.eps),
                              sigma_list,
                              use_quadprog = F,
                              sum_constraint = 1,
                              lower_bound_constraint = 0,
                              step_penalty_function = function(k, n) {
                                0 * k + 0 * n
                              }) {
  
  ## establish link function components
  if (link_function == "linear") {
    ## cumulant generating function
    cumgenfunc <- function(eta)
      0.5 * eta ^ 2
    
    ## the first deriv of cum gen func, expectation of suff. stat, should be inverse of link function
    mu_of_eta <- function(eta)
      eta
    
    ## the second deriv of cum gen func, the negative hessian
    v_of_mu <- function(mu)
      1
    
  } else if (link_function == "logit") {
    ## cumulant generating function
    cumgenfunc <- function(eta)
      log(1 + exp(eta))
    
    ## the first deriv of cum gen func, expectation of suff. stat, should be inverse of link function
    mu_of_eta <- function(eta)
      1 / (1 + exp(-eta))
    
    ## the second deriv of cum gen func, the negative hessian
    v_of_mu <- function(mu)
      mu * (1 - mu)
    
  } else if (link_function == "log") {
    ## cumulant generating function
    cumgenfunc <- function(eta)
      exp(eta)
    
    ## the first deriv of cum gen func, expectation of suff. stat, should be inverse of link function
    mu_of_eta <- function(eta)
      exp(eta)
    
    ## the second deriv of cum gen func, the negative hessian
    v_of_mu <- function(mu)
      mu
    
  }
  
  ## observed information (negative hessian)
  info <- function(B) {
    eta <- X %*% B
    info <- t(X * c(v_of_mu(mu_of_eta(eta)))) %*% X
    diag(info) <- c(diag(info)) +
      rep(lambda, nrow(info))
    info
  }
  
  ## observed score (gradient)
  score <- function(B) {
    eta <- X %*% B
    t(X) %*% (Y - mu_of_eta(eta))
  }
  
  ## ## Step 1: Find optimal weights using quadprog
  ## initialize components
  count <- 0
  eps <- Inf
  beta <- c(rep(1 / ncol(X), ncol(X)))
  gamma <- rep(0, ncol(Z))
  eta <- X %*% beta
  mu <- mu_of_eta(eta)
  tau <- sqrt(ifelse(est_tau, 
                     mean((Y - mu) ^ 2 / v_of_mu(mu)),
                     1))
  info_update <- info(beta)
  score_update <- score(beta)
  
  ## for linear only - we can compute the lagrangian constraints explicitly
  if (link_function == 'linear') {
    A <- cbind(rep(1,ncol(X)))
    G <- quick_inv(t(X) %*% X + diag(rep(lambda,ncol(X))))
    mle <- c(G %*% t(X) %*% Y)
    mle_c <-
      mle - G %*% A %*% solve(t(A) %*% G %*% A) %*% (t(A) %*% mle - 1)
    
    ## if all betas are positive, we're done. No need for SQP with ineq. constr.
    if (min(mle_c) >= 0) {
      beta <- c(mle_c/sum(mle_c))
      count <- 1001
      eps <- 0
      eta <- X %*% beta
      mu <- mu_of_eta(eta)
      tau <- sqrt(ifelse(est_tau, mean((Y - mu) ^ 2 / v_of_mu(mu)) *
                           (nrow(X) / (
                             nrow(X) - ncol(X) + 1
                           )),
                         1))
      info_update <- info(beta)
      score_update <- score(beta)
    }
  }
  
  ## run sqp for stacked weights, ignoring random efx for now
  while (count < 1000 & eps > 1e-6) {
    prev_beta <- beta
    info_pd <- as(nearPD(info_update,
                         ensureSymmetry = T)[[1]], 'matrix')
    sc <- c(sqrt(norm(info_pd, '2')))
    beta <- 0.9 * beta + 0.1 * solve.QP(
      info_pd / sc,
      dvec =
        (score_update + info_pd %*% beta) / sc,
      Amat =
        cbind(cbind(c(rep(
          1, length(beta)
        ))),
        diag(c(rep(
          1, length(beta)
        )))),
      bvec = c(sum_constraint,
               rep(
                 lower_bound_constraint, length(beta)
               )),
      meq = 1
    )$solution
    
    prev_eps <- eps
    eps <- mean(abs(prev_beta - beta))
    count <- count + 1
    
    ## update components
    eta <- X %*% beta
    mu <- mu_of_eta(eta)
    tau <- tau * 0.5 + 0.5 * sqrt(ifelse(est_tau,
                                         mean((Y - mu) ^ 2 / v_of_mu(mu)) *
                                           (nrow(X) / (
                                             nrow(X) - ncol(X) + 1
                                           )),
                                         1))
    info_update <- info(beta) * ifelse(link_function == 'linear',
                                       1 / tau ^ 2,
                                       1)
    score_update <- score(beta)
  }
  
  ## ## Step 2: optimize for random efx
  gamma <- rep(0, ncol(Z))
  
  ## first, get the penalty term that offers best cross-val fit
  nn <- nrow(cbind(Y))
  XB <- c(X %*% beta)
  log_l2 <- optim(
    rep(-2.5,
        length(sigma_list)),
    f = function(log_l2) {
      
      ## assign penalties to corresponding indices
      penalty <- rep(0,length(gamma))
      for(i in 1:length(sigma_list)){
        penalty[sigma_list[[i]]] <- rep(exp(log_l2[i]),
                                        length(sigma_list[[i]])) + lambda
      }
      
          ## wrap around gamma being optimized
          gamma <- optim(
            rep(0, ncol(Z)),
            fn = function(gamma) {
              eta <- XB + Z %*% gamma
              - sum(Y * eta / nn - cumgenfunc(eta) / nn) +
                   0.5 * sum(penalty*gamma^2 / nn)
            },
            gr = function(gamma) {
              eta <- XB + Z %*% gamma
              mu <- mu_of_eta(eta)
              -c(t(Z) %*% (Y  - mu)) / nn +
                penalty * gamma / nn
            },
            method = 'BFGS'
          )$par
          eta <- XB + Z %*% gamma
          
          
          ## maximize loglik
          mean(Y * eta / nn - cumgenfunc(eta) / nn)
          
      },
    ## optimizers for 1-D vs >1-D problems
    lower = ifelse(length(sigma_list) == 1,
                   -20,
                   -Inf),
    upper = ifelse(length(sigma_list) == 1,
                   12.5,
                   Inf),
    method = ifelse(length(sigma_list) == 1,
                    'Brent',
                    'BFGS')
  )$par
  
  ## next, given penalty term, optimize for the random effects themselves
  penalty <- rep(0,length(gamma))
  for(i in 1:length(sigma_list)){
    penalty[sigma_list[[i]]] <- rep(exp(log_l2[i]),
                                    length(sigma_list[[i]])) + lambda
  }
  gamma <- optim(
    rep(0, ncol(Z)),
    fn = function(gamma) {
      eta <- X %*% beta + Z %*% gamma
      - sum(Y * eta / nn - cumgenfunc(eta) / nn) +
           0.5 * sum(penalty*gamma^2 / nn)
    },
    gr = function(gamma) {
      eta <- X %*% beta + Z %*% gamma
      mu <- mu_of_eta(eta)
      -c(t(Z) %*% (Y  - mu)) / nn +
        penalty * gamma / nn
    },
    method = 'BFGS'
  )$par
  
  ## ## Step 3: final estimated components
  eta <- X %*% beta + Z %*% gamma
  mu <- mu_of_eta(eta)
  tau <- sqrt(ifelse(est_tau,
                     mean((Y - mu) ^ 2 / v_of_mu(mu)) *
                       (nrow(X) / (
                         nrow(X) - ncol(X) - ncol(Z) + 1
                       )),
                     1))
  sigmas <-
    sapply(sigma_list, function(inds) {
      sd(c(gamma[inds]))
    })
  
  sigmas <- ifelse(is.nan(sigmas), 0, sigmas)
  names(sigmas) <- names(sigma_list)
  beta <- c(beta)
  names(beta) <- names(beta_update)
  names(gamma) <- names(u_update)
  loglik <- sum(Y * eta - cumgenfunc(eta))
  
  ## return final components
  return(
    list(
      "weights" = beta,
      "beta_update" = beta,
      "u_update" = gamma,
      'loglik' = loglik,
      'AIC' = -2 * loglik + step_penalty_function(ncol(X) + ncol(Z) - 1, nrow(X)),
      'quadprog_iterations' = count,
      'vcov' = try({quick_inv(info_update)},
                   silent = T),
      'sigmas' = sigmas,
      'tau' = tau,
      'newton_iter' = 0,
      'beta_mle' = rep(1 / ncol(X), ncol(X)),
      'damp_step_attempts' = 0,
      'loglik_function' = NULL,
      'cumgenfunc' = cumgenfunc,
      'C' = NULL,
      'probit' = F,
      'softmax' = F,
      'step_penalty_function' = step_penalty_function
    )
  )
}

