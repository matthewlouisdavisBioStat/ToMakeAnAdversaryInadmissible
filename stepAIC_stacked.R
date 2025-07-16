#' Perform Backwards Variable Selection via AIC when stacking.
#' @param vars a character vector of names of variables (i.e. base-learners) to perform selection on.
#' @param cv_data a dataframe containing the cross-validated out-of-fold predictions of base learners.
#' @param Zmodel the random effects design matrix.
#' @param link_func the link function specified.
#' @param est_tau_model T or F, is the exponential dispersion parameter to be estimated?
#' @param t_init real number greater than 0, the initial (or fixed, if not being estimated) value of the exponential dispersion parameter.
#' @param s_init vector of real numbers greater than 0, the initial (or fixed, if not being estimated) values of the random-effect variance parameters.
#' @param b_init vector of real numbers, the initial values of weights for estimating unconstrained MLEs.
#' @param u_init vector of real numbers, the initial values of random effects.
#' @param list_of_sigmas the sigma list, containing the names and corresponding column-indices of the Z matrix corresponding to each random effect variance component.
#' @param qp T or F, should sequential quadratic programming be used over a more general non-linear optimizer?
#' @param min_num_learners integer > 1, minimum number of base learners to include in stacked model before halting the stepwise selection procedure.
#' @param use_fixed_weights T or F, whether stacked model should be fit or fixed weights assigned.
#' @param preset_weights If use_fixed_weights == T,then this must be a vector of weights with length exactly equal to the number of base learners considered.
#' @param tr T or F, should the program print out stepwise base-learner selection output?
#' @param spf  function with consecutive arguments for number of base learners and number of observations, for computing the stepwise base learner selection penalty. function(k,n){2*k} yields traditional AIC. Defaults to 0.
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
#'  print("Not yet finished")
#' }
#' @export
stepAIC_stacked <- function(vars,cv_data, Zmodel,link_func,est_tau_model = F,t_init,s_init,b_init,u_init,list_of_sigmas,qp = F,
                            min_num_learners,use_fixed_weights = F,preset_weights = NULL,tr = T,spf = function(k,n){0*k+0*n}){
  softmax <- link_func == 'softmax'
  surv <- link_func == 'weibull'
  keep_going <- TRUE
  Ymodel <- cv_data$y

  iter <- 0
  b_init_post <- b_init
  u_init_post <- u_init
  s_init_post <- s_init
  t_init_post <- t_init
  og_names <- names(b_init)
  while (keep_going) {

      (b_init_temp <- rep(1/length(vars),length(vars)))
      names(b_init_temp) <- vars
    if(tr){
      print(paste0("Remaining Base Learners: ",paste0(vars,collapse = ", ")))
    }
    if(!softmax){
      Xmodel <- as(cv_data[,c(t(c(na.omit(unlist(sapply(vars,function(tv)which(colnames(cv_data) == tv)))))))],'matrix')
    } else{
      Xmodel <- cv_data$X[vars]
    }



    ## save these for (potentially) returning later
    if(!use_fixed_weights){
      full_fit <- (glmer_constrained(link_function = link_func,
                                X = Xmodel,
                                Y = Ymodel,
                                Z = Zmodel,
                                tau = t_init,
                                est_tau = est_tau_model,
                                sigmas = s_init,
                                beta_update = ifelse2(!softmax,
                                                      cbind(b_init_temp)[vars,],
                                                      c(b_init_temp)[vars]),
                                u_update = u_init,
                                sigma_list = list_of_sigmas,
                                use_quadprog = qp,
                                lambda = 0.000001,
                                 step_penalty_function = spf))
      #print("Finished the outer-round of selection")
    b_init_post <- full_fit$beta_update
    names(b_init_post) <- vars
    u_init_post <- full_fit$u_update
    names(u_init_post) <- paste0("RandInt",1:length(u_init_post))
    s_init_post <- full_fit$sigmas
    names(s_init_post) <- paste0("sigma_",1:length(s_init_post))
    t_init_post <- full_fit$tau
    names(t_init_post) <- 'tau'

    ## AIC of full model to compare to
    full_AIC <- full_fit[['AIC']]
    AIC_vec <- rep(NA, length(vars))
    names(AIC_vec) <- vars
    #print("Finished Renaming Things")
    } else {
      b_init_post <- preset_weights
      names(b_init_post) <- vars
      u_init_post <- u_init
      names(u_init_post) <- paste0("RandInt",1:length(u_init_post))
      s_init_post <- s_init
      names(s_init_post) <- paste0("sigma_",1:length(s_init_post))
      t_init_post <- t_init
      names(t_init_post) <- 'tau'

      ## AIC of full model to compare to
      full_AIC <- NA
      AIC_vec <- rep(NA, length(vars))
      names(AIC_vec) <- vars
      full_fit <- list('weights' = b_init_post,#[-length(weights)],
                       'beta_update' = preset_weights,
                       'u_update' = u_init,
                       'loglik' = NA,
                       'AIC' = NA,
                       'niter' = NA,
                       'vcov' = NA,
                       'sigmas' = s_init,
                       'sigma_list'  = list_of_sigmas,
                       'tau' = t_init,
                       'fisher_iter' = NA,
                       'beta_mle' = weights,
                       'final_newt_steps' = NA)
      return(list(vars,b_init_post,u_init_post,t_init_post,s_init_post,'final_fit' = full_fit))
      keep_going <- FALSE
    }
    ## if only min_num_learners or less learners, return both
    if(length(vars) <= min_num_learners){ ## return last predictor(s) standing
      return(list(vars,b_init_post,u_init_post,t_init_post,s_init_post,'final_fit' = full_fit))
      keep_going <- FALSE
    }
    #print("Finished the if-elses")
    ## Iterate over each variable to compute AIC
    ## Remove any 0'd variables from previous iter
    names(b_init) <- vars
    #keep_vars <- vars[which(round(full_fit$weights,7) > 0)]
    for (var in vars) {

      print(paste0("    Removing Base Learner: ",var))
      temp_vars <- vars[vars != var]
      if(!softmax){
        Xmodel <- as(cv_data[,c(t(c(na.omit(unlist(sapply(temp_vars,function(tv)which(colnames(cv_data) == tv)))))))],'matrix')
      } else{
        Xmodel <- cv_data$X[vars[vars != var]]
      }

        (b_init_temp <- rep(1/length(temp_vars),length(temp_vars)))
        names(b_init_temp) <- temp_vars
      ## we then use backwards variable selection amongst the non-0 weights
        #print("About to start second round")
        AIC_vec[var] <- (glmer_constrained(link_function = link_func,
                                      X = Xmodel,
                                      Y = Ymodel,
                                      Z = Zmodel,
                                      tau = t_init,
                                      est_tau = est_tau_model,
                                      sigmas = s_init,
                                      beta_update = ifelse2(!softmax,
                                                            cbind(b_init_temp)[temp_vars,],
                                                            c(b_init_temp)[temp_vars]),
                                      u_update = u_init,
                                      sigma_list  = list_of_sigmas,
                                      use_quadprog = qp,
                                      lambda = 0.000001,
                                          step_penalty_function = spf))[['AIC']]
      }
    #}
    full_AIC <- full_fit[['AIC']]
    if(tr){
      print(paste0("AIC full model vs. excluding each Xth base learner"))
      v <- c(full_AIC,AIC_vec)
      names(v)[1] <- "Full AIC"
      print(v)
    }
    AIC_vec <- full_AIC - AIC_vec
    worst_var <- (names(AIC_vec)[AIC_vec == max(AIC_vec,na.rm = T)])[1]
    iter <- iter + 1
    if(length(AIC_vec) <= 2){ ## return last two predictor(s) standing
      return(list(vars,b_init_post,u_init_post,t_init_post,s_init_post,final_fit = full_fit))
      keep_going <- FALSE
    } else if (iter > 100) { # maximum 100 rounds
      keep_going <- FALSE
      return(list(vars,b_init,u_init,t_init,s_init,final_fit = full_fit))
    } else if (AIC_vec[worst_var] >= -2) { # if AIC does not improve by 2 units
      vars <- vars[!(vars == worst_var)] # take first if tie
      b_init <- cbind(b_init)[vars,]
    } else { # AIC of all has improved by 2 units, stop going
        return(list(vars,b_init_post,u_init_post,t_init_post,s_init_post,final_fit = full_fit,other_aics = AIC_vec))
    }
  }
}

