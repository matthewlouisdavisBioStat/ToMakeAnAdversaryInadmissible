#' Convert the log-likelkihood for common REF models into MLMetrics()
#' @param link_function the link function corresponding to the log-likelihood desired.
#' @param tau the fixed exponential dispersion parameter for computing the log-likelihood (i.e., this would be sigma for a normal distribution).
#' @param double whether to double the log-likelihood when computing the metric, which suggests a Wilks-like statistic.
#'
#' @details Make MachineShop-compatible MLMetrics reflecting certain log-likelihoods for predicted and observed responses.
#'
#' @importFrom MachineShop MLMetric
#' @return an MLMetric that can be used with other MachineShop compatible functions.
#'
#' @seealso \code{\link[MLMetric]{MLMetric}}
#'
#' @examples
#' \donttest{
#'
#'  make_loglikelihood_MLMetric(link_function = 'linear', tau = 1, double = F)
#' }
#' @export
make_loglikelihood_MLMetric <- function(link_function,tau,double){
  loglikelihood <- function(observed,predicted = NULL,
                            weights = NULL,
                            cutoff = MachineShop::settings("cutoff"), ...){

    computed_loglik <- F
    probit <- F
    softmax <- F
    survival <- F
    d_censored <- NULL


    if(double){
      mult <- 2
    } else{
      mult <- 1
    }

    ## establish link function components
    if (link_function == "linear"){

      ## link function
      link <- function(mu)mu

      ## cumulant generating function
      cumgenfunc <- function(eta)0.5*eta^2

      ## estimating tau is the same as the mse
      tau <- ifelse(tau == 1,MachineShop::rmse(observed,predicted),tau)

      # ## offset function
      C <- function(observed, tau)sum(-0.5*log(2*pi) + -0.5*log(tau^2) + -0.5/(tau^2) * observed^2,
                                      na.rm = T)

    } else if (link_function == "logit") {

      predicted <- ifelse(sapply(predicted,class) == 'factor',
                          as.numeric(predicted)-1,
                          as.numeric(predicted))
      observed <- ifelse(sapply(observed,class)  == 'factor',
                         as.numeric(observed)-1,
                         as.numeric(observed))

      ## link function
      link <- function(mu){
        mu <- sapply(mu,function(z)ifelse(z >= 1, 0.9999,ifelse(z <= 0, 0.00001, z)))
        log( mu / (1 - mu) )
      }

      ## cumulant generating function
      cumgenfunc <- function(eta)log(1 + exp(eta))

      ## offset function
      C <- function(observed, tau)log(1)

    } else if (link_function == "inverse") {

      ## link function
      link <- function(mu)-(1/mu)

      ## cumulant generating function
      cumgenfunc <- function(eta)log(-1/eta)

      ## offset function (describes the Weibull distribution)
      C <- function(observed, tau)sum(log(tau / observed))

    } else if (link_function == "log") {

      ## link function
      link <- function(mu)log(mu)

      ## cumulant generating function
      cumgenfunc <- function(eta)exp(eta)

      ## offset function
      C <- function(observed, tau) log(1) # sum(log(sapply(observed, factorial)))

    } else if (link_function == "probit") {
      predicted <- ifelse(sapply(predicted,class) == 'factor',
                          as.numeric(predicted)-1,
                          as.numeric(predicted))
      observed <- ifelse(sapply(observed,class)  == 'factor',
                         as.numeric(observed)-1,
                         as.numeric(observed))
      link <- function(mu)qnorm(mu)
      computed_loglik <- T
      Eta <- cbind(link(predicted))
      return(mult*as.numeric(sum(c(observed) * pnorm(Eta))) + sum(c(1-observed) * pnorm(1-Eta)))

    } else if (link_function == "softmax") {
      observed <- sapply(unique(observed)[-1],function(x){
        as.numeric(observed == x)
      })
      computed_loglik <- T
      return(mult*sum(cbind(observed,1-rowSums(observed)) * cbind(log(predicted[,-1]),log(1-rowSums(predicted[,-1])))))

    } else if(link_function == "weibull"){
      survival <- T

      ## censorship
      d_censored <- as.numeric(!grepl("[+]",as.character(observed)))

      ## log-transformed time
      Y <- log(sapply(observed,function(y){
        y <- as.character(y)
        if(grepl("[+]",y)) y <- substr(y,1,nchar(y)-1)
        as.numeric(y)
      }))[1:length(observed)]

      ## get from the linear-link function to the estimate of the mean
      mu_of_eta <- function(eta,t)exp((Y-as.numeric(eta))/t)
      Mu <- mu_of_eta(log(predicted),tau)
      computed_loglik <- T
      return(mult*(
        sum(d_censored * (log(1/tau) + log(Mu))) +
          sum(-Mu)
      )
      )

    }
    if(!computed_loglik){
      ## Canonical GLM Form
      Eta <- cbind(link(predicted))
      return(mult*as.numeric( (sum(observed*Eta) -
                                 sum(sapply(Eta,cumgenfunc)))/tau +
                                C(observed,tau)))
    }
  }
  MLMetric(loglikelihood) <-
    list(name = "log-likelihood",
         label = "Log-Likelihood Loss Function",
         maximize = TRUE)
  return(loglikelihood)
}

