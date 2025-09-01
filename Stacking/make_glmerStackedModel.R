#' Wrapper for generating an meta-learner to be paired with SuperModel for stacking with stepwise selection.
#' @param link the link function for modelling, which implies the GLM to-be-used.
#' @param incl_random_efx T or F, should random effects be included? Requires a supplied Z matrix if T.
#' @param tau_0 positive real number, the initial (or fixed, if est_dispersion = F) exponential family dispersion parameter.
#' @param est_dispersion T or F, should the exponential family dispersion parameter be estimated as well in the stacked model?
#' @param min_learners positive real number at least 2 or greater, the minimum number of base learners until the the backwards selection procedure will stop running (no more base learners will be excluded).
#' @param Z design matrix for random effects.
#' @param sigma_list list of entries with names equal to "sigma_1....X" and elements containing indices of the columns of Z that correspond to each random variance component.
#' @param use_qp T or F, should quadratic programming using the quadprog package or non-linear programming using the Rsolnp package be used to find constrained MLEs?
#' @param fixed_weights T or F, rather than fit the stacked model, should the weights be custom and fixed? If true, weights must be provided.
#' @param weights A vector of weights with length EXACTLY equal to the number of base learners being stacked, required if fixed_weights is T.
#' @param trace T or F, should the program print out stepwise base-learner selection output?
#' @param AIC_penalty function with consecutive arguments for number of base learners and number of observations, for computing the stepwise base learner selection penalty. function(k,n){2*k} yields traditional AIC. Defaults to 0.
#' @details Make an MLModel to be inserted into the SuperModel function as the meta (aka super) learner. This allows one to stack ML models for a variety of different outcomes based on the GLM framework, and allows the user to perform backwards variable selection via AIC to choose an optimal combination of base learners.
#'
#' @return A MachineShop custom MLModel that can be passed into the SuperModel function as the meta (aka super) learner.
#'
#' @importFrom MachineShop MLModel
#'
#' @seealso \code{\link[MLModel]{MLModel}}, \code{\link[SuperModel]{SuperModel}}
#'
#' @examples
#' \donttest{
#'
#'  make_glmerStackedModel(link = 'linear',est_dispersion = T)
#' }
#' @export
make_glmerStackedModel <- function(
                               link,
                               incl_random_efx = F,
                               tau_0 = 1,
                               est_dispersion = F,
                               min_learners= 2,
                               Z= NULL,
                               sigma_list = NULL,
                               use_qp = F,
                               fixed_weights = F,
                               weights = NULL,
                               trace = T,
                               AIC_penalty = function(k,n){0*k+0*n}){
  ## min learners stop
  if(min_learners < 2){
    stop("Minimum number of base learners is 2: choosing only one base learner will break things!")
  }
  ## link function stop
  if(!(link %in% c('weibull',
                   'softmax',
                   'linear',
                   'log',
                   'inverse',
                   'probit',
                   'logit'))){
    stop(paste0("Link function must be one of: ",paste0(c(
                                                   'weibull',
                                                   'softmax',
                                                   'linear',
                                                   'log',
                                                   'inverse',
                                                   'probit',
                                                   'logit'),
                collapse = ", ")))
  }
  if(incl_random_efx & (is.null(Z) | is.null(sigma_list))){
    stop(paste0("If random effects are desired, a Z matrix AND associated sigma list are required"))
  }
  if(fixed_weights & is.null(weights)){
    stop(paste0("You must specify fixed weights if fixed_weights = T"))
  }
  if(fixed_weights & !is.null(weights)){
    warning("Fixed weights are specified: please make sure the length of the vector of weights is EXACTLY equal to the number of base learners in the super model or things will break!")
  }
  if(est_dispersion & (link %in% c('softmax','inverse','probit','logit'))){
    warning(paste0("Exponential dispersion estimation is desired, but it is not implemented for the following link functions: softmax, logit, inverse, or probit"))
  }
  ## Fitting Ensemble Models for GLM Outcomes
  ## Uses Backwards Variable Selection via AIC
  MLModel(
    name = "glmerEnsemble",
    label = paste0("Fits generalized linear mixed effecs models for stacking, with coefficients constrained to be positive and sum to 1 and backwards variable selection via AIC for choosing base learners"),
    response_types = c("numeric","factor","Surv","PoissonVariate"),
    fit = function(formula = as.formula(y ~ 1),
                   data,
                   ...) {

      ## when no random effects are specified.....
      if(!incl_random_efx | is.null(Z) | is.null(sigma_list)){

        ## setup for random efx (arbitrarily set all entries to 0 to avoid including random efx)
        Z <- matrix(0,nrow = nrow(data),ncol = 2)
        sigma_list <- list("sigma_1" = 1:(ncol(Z)))
        names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
        for(i in 1:length(sigma_list)){
          names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
                                        length(sigma_list[[i]]))
        }
        sigmas <- 1
        names(sigmas) <- names(sigma_list)
      }
      Zog <- Z
      if(!incl_random_efx){
        Z <- Z*0
        Z <- Z[1:nrow(cbind(data$y)),]
      }
      if(link == 'softmax'){
        ## dummy lm to save data to for convenience
        model <- lm(rnorm(100) ~ 1)
        model$model <- data
        data <- model$model

        ## K-1 outcomes
        unique_outcomes <- sort(unique(data$y))[-1]
        num_cats <- length(unique_outcomes)
        bs_lrnrs <- (sapply(paste0("X",1:100),paste0,".",unique_outcomes))
        nlearners <- sum(bs_lrnrs %in% colnames(as.data.frame(data)))/num_cats
        if(nlearners == 0){
          stop("Check Your Model Setup: No Base Learners Found")
        }
        bs_lrnrs <- bs_lrnrs[bs_lrnrs %in%
                               colnames(data)]

        ## logit link function for softmax
        ## note, this is fixed so only binary-response base learners can be link-transformed
        ## predicted probabilities of 0 become -10 on log-odds scale,
        ## predicted probabilities of 1s become 10s on the log-odds scale
        link_func <- function(K) {
                                   K[K < 0.00001] <- 0.00001/(num_cats)
                                   K[K > 0.99999] <- 0.99999/(num_cats)
                                   K[is.na(K)] <- 1/num_cats
                                   x <- c(log(K/(1 - sum(K, na.rm = T))))
                                   x <- ifelse(is.na(x), 0, x)
                                   x <- ifelse(x <= -10, -10, x)
                                   x <- ifelse(x >= 10, 10, x)
                                   unlist(x)
                                 }
                                 X <- lapply(paste0("X", 1:nlearners, "."), 
                                             function(xx) {
                                               d <- data[, paste0(xx, as.character(unique_outcomes))]
                                               d <- t(apply(as(d[, paste0(xx, unique_outcomes)], 
                                                               "matrix"), 1, link_func))
                                               d
                                             })
        names(X) <- paste0("X",1:nlearners)
        bs_lrnrs <- names(X)
        Y <- sapply(unique_outcomes,function(x){
          as.numeric(data$y == x)
        })
        Z <- lapply(1:ncol(Z),function(j)cbind(c(Z[,j]),c(Z[,j])))

        ## note that Y, X, and Z are abstractions of matrices,
        ## now stored as lists
        data <- list(y = Y, X = X, Z = Z)


      } else {

        ## How many learners are being considered (100 max)?
        data <- data[,c(grepl("y",colnames(data)) |
                          grepl("X",colnames(data)))]
        bs_lrnrs <- paste0("X",1:100)
        nlearners <- sum(bs_lrnrs %in% colnames(as.data.frame(data)))
        bs_lrnrs <- bs_lrnrs[bs_lrnrs %in%
                               colnames(data)]
        if(nlearners == 0){
          stop("Check Your Model Setup: No Base Learners Found")
        }
        if(nlearners == 100){
          warning("Only the first 100 base learners will be used")
        }

        ## link functions
        if (link == "linear"){link_func <- function(K)K}
        if (link == "log"){link_func <- function(K){
          K[is.na(K) | !is.finite(K)] <- mean(K,na.rm = T)
          K[K <= 0] <- min(
              min(K[K > 0],na.rm=T),
              1,
              na.rm = T
            )
          log(K)
        }}
        if (link == "logit") {
                                   link_func <- function(K){
                                     x <- log(K/(1 - K))
                                     x <- ifelse(is.na(x), 0, x)
                                     x <- ifelse(x < -10, -10, x)
                                     x <- ifelse(x > 10, 10, x)
                                     x
                                   } 
                                   inv_link_func <- function(K)1/(1+exp(-K))
                                 }
                                 if (link == "inverse") {
                                   link_func <- function(K) 1/K
                                   inv_link_func <- function(K)1/K
                                 }
                                 if (link == "probit") {
                                   link_func <- function(K){
                                     x <- qnorm(K)
                                     x <- ifelse(is.na(x), 0, x)
                                     x <- ifelse(x < -5, -5, x)
                                     x <- ifelse(x > 5, 5, x)
                                     x
                                   } 
                                   inv_link_func <- function(K)pnorm(K)
                                 }
        #if (link == "logit"){link_func <- function(K)log(K/(1-K))}
        #if (link == "inverse"){link_func <- function(K)1/K}
        #if (link == "probit"){link_func <- function(K)qnorm(K)}
        if(link == "softmax"){link_func <- function(K)rbind(log(K/(1-sum(K))))}
        if (link  == "weibull") link_func <- function(K)log(max(K,0.01))

        ## Change the predictions to be on link-function scale
        data[,-1] <- apply(data[,-1],2,function(x){
          x <- as.numeric(as.character(x))

          ## Fix so only binary-response base learners can be link-transformed
          ## ie, log(1/(1-1)) is underfined, but log(0.9999/(1-.9999)) is just a big number
          ## ie for svms that only take on binary outcome values
          if(link == "logit"){
            if(!(any(x != 0) | any(x != 1))){
              x <- x*10 + (1-x)*(-10)
            }
          }

          if(link == "probit"){
            if(!(any(x != 0) | any(x != 1))){
              x <- x*2.5 + (1-x)*(-2.5)
            }
          }
          sapply(x,link_func)
        })
        if(link != "weibull") data[,1] <- as.numeric(as.character(data[,1]))
      }



      ## set the initial weights of beta to be all equal to 1/p
      beta_update <- rep(1/length(bs_lrnrs),length(bs_lrnrs))
      names(beta_update) <- bs_lrnrs

      ## set random intercepts to 0
      if(link == "softmax"){
        u_update <- matrix(0,ncol = num_cats,nrow = length(unlist(sigma_list)))
      } else {
        u_update <- rep(0,ncol(Z))
      }

      ## if linear, use a generalized least-squares approach to initialize variance component tau
      if(link == "linear"){

        Xstar <- cbind(data[,bs_lrnrs] %>% as('matrix'),
                       Z
        )
        XX <- t(Xstar) %*% Xstar
        G <- quick_inv(XX)
        Y <- data[,1]
        XY <- t(Xstar) %*% Y
        sse_r <- t(Y) %*% Y - t(XY) %*% G %*% XY # Y'(I-XGX')Y / (n-rank(X'X))
        tau <- sqrt(as.numeric(c(sse_r/(nrow(Xstar) - qr(XX)$rank))))


        ## else set to 1
      } else {
        tau <- tau_0
      }
      ## initialize random efx variance component to 1s
      ## will be updated via N.R. anyways
      sigmas <- rep(1,length(sigma_list))
      names(sigmas) <- names(sigma_list)

      ## run backwards AIC variable selection
      if(link != 'softmax'){
        data <- as.data.frame(data)

      }
      start_time <- Sys.time()
      if(trace){
        print("Starting Backwards Selection for Base Learners")
        print(start_time)
      }

      ## fit a stacked model using backwards variable selection via AIC
      final_fit <- stepAIC_stacked(vars = bs_lrnrs,
                           cv_data = data,
                           Zmodel = Z,
                           link_func = link,
                           est_tau_model = est_dispersion,
                           t_init = tau,
                           s_init = sigmas,
                           b_init = cbind(beta_update)[bs_lrnrs,],
                           u_init = cbind(u_update),
                           list_of_sigmas = sigma_list,
                           qp = use_qp,
                           min_num_learners = min_learners,
                           use_fixed_weights = fixed_weights,
                           preset_weights = weights,
                           tr = trace,
                                  spf = AIC_penalty)
      end_time <- Sys.time()
      if(trace){
        print(paste0("Finished Selecting Base Learners, Time Elapsed: ",end_time - start_time))
      }

      ## model output
      best_model_vars <- final_fit[[1]]
      beta_update <- final_fit$final_fit$beta_update
      u_update <- final_fit$final_fit$u_update
      tau <- final_fit[[4]]
      sigma_u <- final_fit[[5]]

      ## clean data
      if(!incl_random_efx & est_dispersion & (link!='softmax')){
        u_update <- 0*u_update
        sigma_u <- 0*sigma_u
        final_fit$final_fit$u_update <- 0*u_update
        final_fit$final_fit$sigmas <- 0* final_fit$final_fit$sigmas
      }

      ## select non-null,non-0 betas only

      names(beta_update) <- best_model_vars
      if(link != 'softmax'){
        best_model_vars <- names(beta_update[beta_update > 0.00000000001])
        beta_update <- cbind(beta_update)[best_model_vars,]
      } else{
        bu <- sapply(1:length(beta_update),function(i)unique(diag(beta_update[[i]])))
        best_model_vars <- best_model_vars[which(bu > 0.0000000001)]
        beta_update <- beta_update[which(bu > 0.0000000001)]
      }

      ## Labelling the weights
      final_weights <- c(c(beta_update),u_update)
      names(final_weights) <- c(best_model_vars,paste0("RandInt",1:length(u_update)))

      ## GLM object for Saving Data
      if(link == "weibull"){
        data$y <- (sapply(data$y,function(y){
          y <- as.character(y)
          if(grepl("[+]",y)) y <- substr(y,1,nchar(y)-1)
          as.numeric(y)
        }))[1:length(data$y)]
      }
      if(link != 'softmax'){
        fo <- as.formula(paste0("y ~ 1"))
      } else{
        fo <- as.formula(paste0("y.1 ~ 1"))
      }
      glm_fit <- glm(fo,data = as.data.frame(data))
      glm_fit$coefficients <- final_weights
      vcov <- try(as(final_fit$final_fit$vcov,'matrix'),silent = T)
      #if(class(vcov) != 'try-error') glm_fit$vcov <- NULL
      glm_fit$data <- list(
        #data = data,
        final_fit = final_fit$final_fit,
        Zmat = Zog,
        link_function = link,
        select_time = end_time - start_time
      )
      return(
        glm_fit
      )
    },

    ## for making predictions on new data
    predict = function(object,
                       newdata,
                       Zmat = NULL,
                       ...) {


      ## load the fitted stacked model object
      data <- newdata
      super_fit <- object
      tau <- super_fit$data$final_fit$tau
      link_function <- super_fit$data$link_function
      incl_random_efx <- ifelse2(mean(abs(super_fit$data$Zmat)) == 0,
                                 F,
                                 T)
      bs_lrnrs_selected <- names(super_fit$coefficients[grepl('X',names(super_fit$coefficients))])

      ## link functions
      if (link_function  == "linear") {
        link_func <- function(K) K
        inv_link_func <- function(K) K
      }
      if (link_function  == "log") {
        link_func <- function(K){
          K[is.na(K) | !is.finite(K)] <- mean(K,na.rm = T)
          K[K <= 0] <- min(
              min(K[K > 0],na.rm=T),
              1,
              na.rm = T
            )
          log(K)
        }
        inv_link_func <- function(K) exp(K)
      }
      if (link == "logit") {
                                   link_func <- function(K){
                                     x <- log(K/(1 - K))
                                     x <- ifelse(is.na(x), 0, x)
                                     x <- ifelse(x < -10, -10, x)
                                     x <- ifelse(x > 10, 10, x)
                                     x
                                   } 
                                   inv_link_func <- function(K)1/(1+exp(-K))
                                 }
                                 if (link == "inverse") {
                                   link_func <- function(K) 1/K
                                   inv_link_func <- function(K)1/K
                                 }
                                 if (link == "probit") {
                                   link_func <- function(K){
                                     x <- qnorm(K)
                                     x <- ifelse(is.na(x), 0, x)
                                     x <- ifelse(x < -5, -5, x)
                                     x <- ifelse(x > 5, 5, x)
                                     x
                                   } 
                                   inv_link_func <- function(K)pnorm(K)
                                 }
      if (link_function  == "weibull") {
        link_func <- function(K)log(max(ifelse(is.nan(K) | is.na(K),
                                               mean(K,na.rm = T),
                                               K),0.01))
        inv_link_func <- function(K)exp(ifelse(is.na(K) | is.nan(K),
                                               mean(K,na.rm=T),
                                               K
        ))
      }

      ## make predictions on new data
      if(link_function != "softmax"){
        coef_names <- names(coef(super_fit))
        baselearner_names <- coef_names[grepl('X', coef_names)]
        data <- apply(data, 2, function(x)sapply(x,link_func))
        colnames(data) <- colnames(newdata)
        X <- as(data[, baselearner_names], 'matrix')
        Z <- ifelse2(!is.null(Zmat),Zmat,super_fit$data$Zmat)
        eta_hat <- (X %*% coef(object)[baselearner_names] +
                      ifelse2(incl_random_efx,
                              (Z %*% cbind(super_fit$coefficients[ncol(X) + 1:ncol(Z)])),
                              0)
        )
        preds <- sapply(eta_hat, inv_link_func)
        return(preds)
      } else {

        ## generalizes X %*% B but for multinomial outcomes
        `%**%` <- function(A,b){
          list <- (lapply(1:length(A),function(j){
            A[[j]] %*% b[[j]]
          }))
          sapply(1:ncol(list[[1]]),function(j){
            rowSums(sapply(list,function(l)l[,j]))
          })
        }

        ## adapted for softmax regression
        inv_link_func <- function(K){
          exp(K) / (1 + sum(exp(K)))
        }
        unique_outcomes <- unique(sapply(colnames(newdata),
                                         function(n){
                                           (strsplit(n,
                                                     "[.]") %>% unlist)[[2]]
                                         }))[-1]
        num_cats <- length(unique_outcomes)
        bs_lrnrs <- (sapply(paste0("X",1:100),paste0,".",unique_outcomes))
        nlearners <- sum(bs_lrnrs %in% colnames(as.data.frame(newdata)))/num_cats
        if(nlearners == 0){
          stop("Check Your Model Setup: No Base Learners Found")
        }
        bs_lrnrs <- bs_lrnrs[bs_lrnrs %in%
                               colnames(newdata)] %>%
          sapply(function(n){
            (strsplit(n,
                      "[.]") %>% unlist)[[1]]
          }) %>% unique
        link_func <- function(K) {
                                   K[K < 0.00001] <- 0.00001/(num_cats)
                                   K[K > 0.99999] <- 0.99999/(num_cats)
                                   K[is.na(K)] <- 1/num_cats
                                   x <- c(log(K/(1 - sum(K, na.rm = T))))
                                   x <- ifelse(is.na(x), 0, x)
                                   x <- ifelse(x <= -10, -10, x)
                                   x <- ifelse(x >= 10, 10, x)
                                   unlist(x)
                                 }
                                 X <- lapply(paste0(bs_lrnrs, "."), 
                                             function(xx) {
                                               d <- data[, paste0(xx, as.character(unique_outcomes))]
                                               d <- t(apply(as(d[, paste0(xx, unique_outcomes)], 
                                                               "matrix"), 1, link_func))
                                               d
                                             })
        names(X) <- bs_lrnrs
        B <- super_fit$coefficients[bs_lrnrs_selected]
        X <- X[bs_lrnrs_selected]
        Z <- ifelse2(!is.null(Zmat),
                     lapply(1:ncol(Zmat),function(j)do.call('cbind',lapply(1:num_cats,function(kk)c(Zmat[,j])))),
                     lapply(1:ncol(super_fit$data$Zmat),function(j)do.call('cbind',lapply(1:num_cats,function(kk)c(super_fit$data$Zmat[,j])))))
        eta_hat <- X %**% B +
          ifelse2(incl_random_efx,(Z %**% super_fit$data$final_fit$u_update),0)

        ## add the Kth component to the K-1 simplex of predictions
        preds <- t(apply(eta_hat, 1, inv_link_func))
        preds <- cbind(1-rowSums(preds),preds)
        return(preds)

      }
    },
    varimp = function(object, ...) {

      ## the weights are THE direct measures of "variable importance" for base learners
      return(object$data$final_fit$weights)
    }
  )
}


