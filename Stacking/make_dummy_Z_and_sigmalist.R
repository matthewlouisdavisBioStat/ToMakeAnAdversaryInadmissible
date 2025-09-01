#' Create dummy random-effects components for ensuring compatibility with glmer_constrained() when only fixed-effects are desired.
#' @param X design matrix of fixed effects for the model, or a list for 3+ categorical outcomes
#'
#' @return a list of elements Z, sigma_list, sigmas, and initial random effect values that when inputted to the eponymous arguments of glmer_constrained() function, will not affect estimation of fixed effects.
#'
#' @examples
#' \donttest{
#'
#'    ## ordinary vector-valued outcomes
#'  make_dummy_Z_and_sigmalist(sapply(1:100,function(i)rnorm(100)))
#'
#'    ## matrix-valued outcomes, i.e., categorical regression
#'  make_dummy_Z_and_sigmalist(lapply(1:100,function(i)cbind(rnorm(100),rnorm(100))))
#'
#' }
#' @export
make_dummy_Z_and_sigmalist <- function(X){
  if(any(class(X) %in% 'matrix')){
    N <- nrow(X)
  } else{
    N <- nrow(X[[1]])
  }
  Z <- matrix(0,nrow = N,ncol = 2)
  U <- rep(0,ncol(Z))
  sigma_list <- list("sigma_1" = 1:2)
  names(sigma_list) <- paste0("sigma_",1:length(sigma_list))
  for(i in 1:length(sigma_list)){
    names(sigma_list[[i]]) <- rep(names(sigma_list)[i],
                                  length(sigma_list[[i]]))
  }
  sigmas <- 0
  names(sigmas) <- names(sigma_list)
  if(!(any(class(X) %in% 'matrix'))){
     Z <- lapply(1:2,function(i)X[[i]]*0)
     U <- lapply(1:2,function(i)diag(0,ncol(X[[i]])))
  }
  return(list("Z" = Z,
              "sigma_list" = sigma_list,
              "sigmas" = sigmas,
              "u_update" = U))
}


