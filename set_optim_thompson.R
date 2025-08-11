## only library needed, assuming MachineShop is loaded up
library(foreach)
################################a################################################

## function for performing latin-grid sampling
sample_between_intervals <- function(x){
  intervals <- sort(unique(x))
  if(length(intervals) >= length(x)/2){
    x
  } else {
    sapply(x,function(y){
      ## if on the max, generate a uniform between a randomly-selected interval
      if (y == max(intervals)){
        s <- sample(1:(length(intervals)-1),1)
        runif(1,intervals[s],intervals[s+1])
        ## otherwise, generate uniform between obs and next interval
      } else {
        runif(1,y,min(intervals[intervals > y]))
      }
    })
  }
}


## for centering and scaling and extracting components
center_and_scale <- function(x){
  xx <- na.omit(x)
  xx <- xx[is.finite(xx)]
  xx <- xx[!is.nan(xx)]
  if(length(xx) > 1){
    meann <- mean(xx[is.finite(xx)],na.rm = T)
    sdd <- sd(xx[is.finite(xx)],na.rm = T)
    if(sdd > 0){
      return(list((x-meann)/sdd,
                  meann,
                  sdd
      ))
    } else {
      return(list((x-meann),
                  meann,
                  1))
    }
  } else {
    return(list(x,
                0,
                1))
  }
}

## knot expansion
knot_expand2 <- function(mat,Q){
  mat[,rep(1:ncol(mat),ncol(cbind(Q)))] * 
    cbind(Q)[,rep(1:ncol(cbind(Q)),each = ncol(mat))]
}

## variance stabilizing/symmetry-inducing transformations
transf <- function(x,type,pwr_lmbda = NULL,...){
  #pwr_lmbda <- 0
  if(type == 'gengamma'){
    ## use box-cox
    if(pwr_lmbda != 0){
      (x^pwr_lmbda+1)/pwr_lmbda
      # x^pwr_lmbda
    } else {
      log(x)
    }
  } else if(type == 'binom'){
    ## use arcsin
    asin(sqrt(x))
  } else if(type == 'beta'){
    ## use hyperbolic-tangent transformation
    x <- ifelse(x <= -1,-0.99999,x)
    x <- ifelse(x >= 1,0.99999,x)
    x <- ifelse(is.nan(x),0,x)
    atanh(x)
  } else {
    return(...)
  }
}

## variance stablizing/symmetry-inducing back-transformations
un_transf <- function(x,type,pwr_lmbda = NULL,...){
  if(type == 'gengamma'){
    if(pwr_lmbda != 0){
      (x * pwr_lmbda - 1)^(1 / pwr_lmbda)
    } else {
      exp(x)
    }
  } else if(type == 'binom'){
    (sin(x))^2
  } else if(type == 'beta'){
    tanh(x)
  } else {
    return(...)
  }
}

gengamma_like_metrics <- c(
  "brier",
  "cross_entropy",
  "mae",
  "mse",
  "msle",
  "rmse",
  "rmsle"
)
binom_like_metrics <- c(
  "accuracy",
  "cindex",
  "fnr",
  "fpr",
  "npv",
  "ppv",
  "ppr",
  "precision",
  "recall",
  "roc_auc",
  "sensitivity",
  "specificity",
  "tnr",
  "tpr",
  "gini"
)
interval_like_metrics <- c(
  "kappa2",
  "r2"
)

## assign a knot group (for quantiles q) to a vector x
get_group <- function(x,q,rev = F){
  if(length(q) > 1){
    rowsums <- rowSums(sapply(2:length(q),function(qq){
      qq*(x <= q[qq] &
            x > q[qq-1])
    }))
  } else {
    rowsums <- 0
  }
  rowsums + 
    ifelse(x <= q[1],
           1,
           0) +
    ifelse(x > q[length(q)],
           length(q)+1,
           0)
}

## generate inverse-gamma variables, with error handling
my_rinvgamma <- function(n, a, b, s){
  try <- rgamma(n, 
                a, 
                rate = b)
  t <- try({
    if(is.na(try)){
      return(s)
    } else {
      if(is.nan(try)){
        return(s)
      } else {
        if(!is.finite(try) | try < 1e-16){
          return(s)
        } else {
          return(1/try)
        }
      }
    }},silent = T)
  if(class(t) == 'try-error'){
    return(s)
  }
}


## fix na
fixna <- function(x){
  if(any(na.omit(c(is.na(x),
                   is.nan(x),
                   !is.finite(x))))){
    rep(0,length(x))
  } else {
    x
  }
}

## first attempting a normal inversion, before resorting to the M.P.G.I.
solvetry <- function(x, 
                     tol = sqrt(.Machine$double.eps)){
  t <- try(solve(x),silent = T)
  if(any(class(t) == 'try-error')){
    svd <- svd(x)
    d_inv <- ifelse(svd$d <= tol,
                    0,
                    1/svd$d)
    (svd$v) %*% 
      (d_inv * 
         t(svd$u))
  } else {
    t
  }
}
optimStats <- function(...){
  stats::optim(...)
}
is.nanf <- function(x){
  sapply(x,function(xx)!is.finite(xx) | is.nan(xx) | is.na(xx))
}

## take the l2norm of a function
l2norm2 <- function(x,other_vec = 0)sqrt(mean(x-other_vec)^2)

## take a first derivative, with respect to a variable: 
first_deriv <- function(dat,var,stdevs){
  cols <- grep(paste0(var,"_degree="),colnames(dat))
  which_sing <- which(colnames(dat) == paste0(var,"_degree=1"))
  cols <- cols[cols != which_sing]
  dat_deriv <- dat
  dat_deriv[,-cols] <- 0
  substrings <- sapply(colnames(dat)[c(cols,which_sing)],function(cn)as.numeric(substr(cn,nchar(cn),nchar(cn))))
  names(substrings) <- colnames(dat)[c(cols,which_sing)]
  denom <- (dat[,which_sing]*stdevs[which_sing])
  tmp <- sapply(c(cols,which_sing),function(j){
    colname <- colnames(dat_deriv)[j]
    ## if interaction, simply divide by x = axy / x = ay
    if(grepl("xXx",colname)){
      ## = observed value divided by the variable 
      dat[,j]/denom
      ## if polynomial term, multiply by degree, divide by variable = ax^(a-1) = a * (x^a / x)
    } else {
      dgr <- substrings[colname]
      dgr*dat[,j]/denom
    }
  })
  dat_deriv[,c(cols,which_sing)] <- tmp
  dat_deriv[is.nan(dat_deriv)] <- 0
  dat_deriv[is.na(dat_deriv)] <- 0
  dat_deriv[!is.finite(dat_deriv)] <- 0
  dat_deriv
}

## take a second derivative, no interactions

## interaction derivatives are easy, recall, would just be the beta coefficient divided by the observed sd 
# = c(beta_new / sds)[j] ! 
## these are only taken for respective numeric numeric interactions, 
## ignores cat-numeric interactions (because we don't take derivatives w/ respect to cats)
interact2_deriv <- function(dat,var,stdevs){
  cols <- grep(paste0(var,"_degree="),colnames(dat))
  which_sing <- which(colnames(dat) == paste0(var,"_degree=1"))
  cols <- cols[cols != which_sing]
  dat_deriv <- dat
  dat_deriv[,-cols] <- 0
  substrings <- sapply(colnames(dat)[c(cols,which_sing)],function(cn)as.numeric(substr(cn,nchar(cn),nchar(cn))))
  names(substrings) <- colnames(dat)[c(cols,which_sing)]
  #### print('got thru first part')
  cols <- cols[grepl("xXx",cols)]
  tmp <- sapply(c(cols,which_sing),function(j){
    colname <- colnames(dat_deriv)[j]
    if(grepl('xxYYzz',colname)){
      catvar <- unlist(strsplit(colname,'xxYYzz'))[1]
      dat[,catvar] / stdevs[j]
    } else {
      rep(1 / stdevs[j],nrow(dat_deriv))
    }
  })
  dat_deriv[,c(cols,which_sing)] <- tmp
  dat_deriv[is.nan(dat_deriv)] <- 0
  dat_deriv[is.na(dat_deriv)] <- 0
  dat_deriv[!is.finite(dat_deriv)] <- 0
  dat_deriv
}

## full hessian = diag(second_deriv) + beta_mat
second_deriv <- function(dat,var,stdevs){
  cols <- grep(paste0(var,"_degree="),colnames(dat))
  which_sing <- which(colnames(dat) == paste0(var,"_degree=1"))
  cols <- cols[cols != which_sing]
  dat_deriv <- dat
  dat_deriv[,-cols] <- 0
  cols <- cols[!grepl("xXx",cols)]
  substrings <- sapply(colnames(dat)[c(cols,which_sing)],function(cn)as.numeric(substr(cn,nchar(cn),nchar(cn))))
  names(substrings) <- colnames(dat)[c(cols,which_sing)]
  denom <- (dat[,which_sing]*stdevs[which_sing])^2
  tmp <- sapply(c(cols,which_sing),function(j){
    dgr <- substrings[colnames(dat_deriv)[j]]
    if(dgr <= 1){
      rep(0,nrow(dat))
    } else {
      dgr*(dgr-1)*dat[,j]/denom
    }
  })
  dat_deriv[,c(cols,which_sing)] <- tmp
  dat_deriv[is.nan(dat_deriv)] <- 0
  dat_deriv[is.na(dat_deriv)] <- 0
  dat_deriv[!is.finite(dat_deriv)] <- 0
  dat_deriv
}

## mogul optimization
set_optim_thompson <- function(object, 
                               times = 10, # how many total grid points will be evaluated (including initial points evaluated)?
                               initial_points = 10, # what is the number of randomly sampled points to evaluate initially?
                               degree = 3, # what is the maximum polynomial degree for numeric and integer parameters? First order interactions will be included regardless.
                               plot_predicted_performance = T, # should the predicted performance of unevaluated grid points be plotted?
                               max_grid_rows = 2500000, # what is the maximum design-matrix number of rows? This prevents computational explosiveness for massive tuning grids; if the tuning grid is over this many rows, this many of them will randomly sampled.
                               max_knots = Inf, # what is the maximum number of knots to include (for fitting splines)?
                               obs_per_spline_coefficient = NULL,#function(num_knots)exp(sum(sapply(1:(num_knots+1),function(k)1/k^2))), # converges to ~ 5.181, starts at ~ 2.7
                               random = TRUE, # when algorithm fails, should all grid points or a single randomly sampled one (default) be evaluated?
                               use_transf = TRUE, # should a default variance-stabilizing transformation be used?
                               log_outcome = NULL, # should the metric being modeled be log-transformed as an outcome?
                               logit_outcome = NULL, # should the metric being modeled be logit-transformed as an outcome?
                               custom_vst = NULL, # custom variance-stabilizing transformation
                               custom_unvst = NULL, # custom inverse of variance-stabilizing transformation
                               save_model_fit = FALSE, # should the design matrix and beta coefficients be saved to an RData file? 
                               include_numeric_interactions = T, # should numeric-numeric interactions be included?
                               include_high_order_interactions = F, # should degree 3-and-higher numeric polynomial terms and categorical interactions be included?
                               include_quadratic_interactions = F, # should degree 2 numeric polynomial terms and categorical interactions be included?
                               qualify_numeric_numeric_interactions = F, # should three-way interactions between categorical and first-order numeric pairings be included?
                               save_model_fit_lable = 'thompson_model_fit.RData', # label for the data file saving the beta coefficients and design matrix,
                               generic_tolerance = sqrt(.Machine$double.eps), # tolerance for convergence on a variety of tasks
                               cardinal_knots = T, # should the knots be spaced evenly?
                               latin_grid = T, # should numerics (not integers) be latin-grid'd?
                               enforce_positive_definiteness = F, # should we ensure that each polynomial partition unconstrained is full rank?
                               print_colnames = T, # should the column names of the design matrix be printed?
                               acquisition_function = NULL, # should a different acquisition function other than Thompson be used?
                               # otherwise, insert a function with the following 6 arguments, in order
                               #' sampled predictions conditional upon the coefficients (beta) sampled 
                               #' the square root of the  variance (sigma^2) that was sampled 
                               #' the best observed performance thus far
                               #' the number of grid points evaluated thus far
                               #' the posterior predictive predictions using the posterior expectation of beta, not the sampled beta
                               #' elipses for additional arguments
                               stopping_rule_total = NULL, # should a stopping rule be used after total steps haven't improved?
                               ... ) { # additional MachineShop arguments to be passed to the fun = (...) argument of set_optim_method
  
  
  
  prob_random = 1 # what is the probability that the algo will try a random grid point or will it be "greedy" and simply try its optimally predicted value? This is the probability of a Bernoulli distribution.
  
  ## for making new knots
  make_1knot <- F
  
  ## initialize with no knots
  num_knots <- 0
  
  ## initialize ridge penalty
  initial_ridge_penalty = 1e-64
  
  # ## initialize
  # beta_meta <- NULL
  
  ## check for presence of input/model grids, and combine 
  ## we need to save the information about the input grid/model grid objects with respect to 
  ## 1) column names
  ## 2) whether or not they are null objects
  input_grid <- NULL
  model_grid <- NULL
  for(i in 1:length(object@grid)){
    grid <- object@grid[[i]]
    grid_name <- names(object@grid)[i]
    if(grid_name == object@model@id){
      model_grid <- grid
    }
    if(grid_name == object@input@id){
      input_grid <- grid
    }
  }
  if(is.null(input_grid) & is.null(model_grid)){
    stop("No tuning grids found: Please supply one")
  } else if(is.null(input_grid)){
    
    grid <- model_grid
    
    ## edit model grid so it isn't a null object, retains it's column names, but is of small size
    model_grid <- model_grid[1:2,]
  } else if(is.null(model_grid)){
    
    grid <- input_grid
    
    ## edit input grid so it isn't a null object, retains it's column names, but is of small size
    input_grid <- input_grid[1:2,]
  } else{
    
    grid <- expand_params(
      c(lapply(input_grid,unique),
        lapply(model_grid,unique))
    )
    
    ## edit both model and input grids so they aren't null objects, retain column names, but are of small size
    model_grid <- model_grid[1:2,]
    input_grid <- input_grid[1:2,]
  }
  
  ## first, make sure the grid isn't stupidly large
  grid_inds_to_keep <- 1:nrow(grid)
  if(nrow(grid) > max_grid_rows){
    grid_inds_to_keep <- sort(sample(1:nrow(grid),
                                     max_grid_rows))
    warning(paste0("The number of rows of the tuning grid, ",nrow(grid),", exceeds ", 
                   max_grid_rows, 
                   ". Only this many rows will be randomly sampled from the grid to be evaluated. This can be changed by increasing the max_grid_rows argument."))
    grid <- grid[grid_inds_to_keep,
    ]
  }
  
  ## fix list error
  #colnames <- colnames(grid)
  grid_temp <- grid
  excl <- which(sapply(1:ncol(grid),function(i)(!(class(grid[[i]])[1] %in% c("numeric","integer","character","factor")))))
  t <- try({
    if(length(excl) > 0){
      grid_temp <- cbind(grid[[excl]],grid[,-excl])
      grid <- grid_temp
    } else{
      NULL
    }
  },silent = T)
  ## this happens when we tune over multiple/solely over categoricals
  if(class(t) == 'try-error'){
    t <- try({
      for(i in 1:ncol(grid)){
        if(!(class(grid[[i]])[1] %in% c("numeric","integer","character","factor"))){
          grid[[i]] <- unlist(grid[[i]])
        }
      }
    },silent = T)
  } else {
    gridname <- NULL
  }
  
  ## try making integers
  for(i in 1:ncol(grid)){
    if(class(unlist(grid[,i])[1]) == "numeric"){
      ## if the rounded values modulo observed values = 0, then we treat as integer
      if(!(any(round(grid[[i]]) - grid[[i]] != 0))){
        grid[[i]] <- as.integer(grid[[i]])
      }
    }
  }
  
  
  
  ## save for later
  og_grid <- grid
  og_normcats <- as.character(1:(num_knots+1))
  unq_g_1 <- apply(og_grid,2,function(x)length(unique(x)) > 1)
  integers <- which(sapply(og_grid[1, ], class) == "integer" & unq_g_1)
  numerics <- which(sapply(og_grid[1, ], class) == "numeric" & unq_g_1)
  ## time to latin grid ## 
  ## sample between intervals so every alpha and every lambda appears once
  if(latin_grid){
    for(num in c(numerics)){
      if(length(unique(unlist(grid[[num]]))) <= length(unlist(grid[[num]]))/4){
        grid[[num]] <- sample_between_intervals(as.numeric(unlist(grid[[num]])))
      }
    }
  }
  ##                    ##
  cats <- which((sapply(og_grid[1, ], class) %in% c("factor", "character")) & 
                  unq_g_1)
  names(cats) <- names(og_grid[1, ])[cats]
  
  ## partition for knots, if numerics and/or integers available
  if(length(c(numerics,integers) > 0)){
    #### print('partinioning predictor space for knots')
    gridnumtemp <- as(grid[,c(numerics,integers)],'matrix')
    sdtemp <- apply(gridnumtemp,2,sd)
    degree1mat <- t(t(gridnumtemp) / sdtemp)
    prednorms <- sort(apply(cbind(degree1mat),
                            1,
                            l2norm2))
    norms <- apply(degree1mat,1,l2norm2)#,colM)
    quants <- quantile(norms,seq(0,
                                 1,
                                 length.out = max(num_knots,1) + # ensures knots of 0 get treated as '1s'
                                   2)[-c(1,max(num_knots,1) + # ensures knots of 0 get treated as '1s'
                                           2)]
    )
    
    ## knot indices 
    knots <- sapply(quants,function(q){
      which(abs(norms-q) == min(abs(norms-q)))[1]
    })
    
    ## knot groups
    Q <- cbind(rep(1,length(norms)))
    
  }
  
  make_design_matrix <- function(grid,drop_cols = T, 
                                 quantiles = quants) {
    ## create design matrix of numeric/integer components for polynomial regre
    ## [degree] columns will be included for each parameter "x" : x, x^2, x^3,....x^degree
    if (length(c(integers, numerics)) > 0) {
      design_mat_num <- foreach(var = (c(integers,
                                         numerics)), .combine = 'cbind') %do% {
                                           if (degree > 1) {
                                             sapply(1:round(degree), function(d) {
                                               as.numeric(unlist(grid[, var])) ^ d
                                             })
                                           } else{
                                             as.numeric(c(unlist(grid[, var])))
                                           }
                                         }
      colnames(design_mat_num) <-
        rep(paste0("degree=", 1:degree), length(c(integers, numerics)))
      colnames(design_mat_num) <-
        paste(rep(
          names(c(integers, numerics
          )),
          each = degree), 
          colnames(design_mat_num), 
          sep = "_")
      if (ncol(design_mat_num) == 0) {
        design_mat_num <- NULL
      }
    } else{
      design_mat_num <- NULL
    }
    
    ## create design matrix of categorical indicators:
    if (length(cats) > 0) {
      design_mat_cat <- foreach(cat = cats, .combine = 'cbind') %do% {
        # if the category has more than one unique value, include it in the design matrix, else exclude it
        if (length(unique(og_grid[[cat]])) > 1) {
          foreach(var = cat, .combine = 'cbind') %do% {
            f <-
              foreach(category = unique(unlist(og_grid[, var])), .combine = 'cbind') %do% {
                as.numeric(grid[, var] == category)
              }
            colnames(f) <-
              paste(colnames(grid)[var], unique(unlist(og_grid[, var])), sep = "=")
            f
          }
        } else{
          cbind(rep(0, nrow(grid)))
        }
      }
      if(drop_cols){
        design_mat_cat <-
          design_mat_cat[, apply(design_mat_cat, 2, function(x)
            any(x != 0))]
      } else {
        design_mat_cat <-
          design_mat_cat[, apply(design_mat_cat, 2, function(x)
            any(x != -Inf))]
      }
      
    } else {
      ## without any categorical variables, we include an intercept into the design matrix
      design_mat_cat <- cbind(rep(1, nrow(grid)))
      colnames(design_mat_cat) <- c("intercept")
    }
    
    
    
    ## add in interactions
    interactions <- NULL
    if (!is.null(design_mat_cat) & !is.null(design_mat_num)) {
      ## if there are both categorical and numeric predictors,
      if (ncol(design_mat_cat) > 1) {
        #### print('catnum')
        first_ind <- ifelse(drop_cols,2,1)
        cat_num_interactions <-
          foreach(i = first_ind:ncol(design_mat_cat), # drop first category
                  .combine = 'cbind') %do% {
                    foreach(j = seq(1, ncol(design_mat_num), by = 1),
                            .combine = 'cbind') %do% {
                              design_mat_cat[, i] * 
                                design_mat_num[, j]
                            }
                  }
        sq <- seq(1, ncol(design_mat_num), by = degree)
        colnames(cat_num_interactions) <-
          foreach(i = first_ind:ncol(design_mat_cat),
                  .combine = 'c') %do% {
                    foreach(j = seq(1, ncol(design_mat_num), by = 1),
                            .combine = 'c') %do% {
                              paste0(colnames(design_mat_cat)[i],
                                     "xZx",
                                     colnames(design_mat_num)[j])
                              
                            }
                  }
        
        if(drop_cols & 
           degree >= 3 & 
           !include_high_order_interactions){
          cat_num_interactions <- 
            cat_num_interactions[,-which(sapply(colnames(cat_num_interactions),function(col){
              any(sapply(paste0("degree=",3:degree),function(nam)grepl(nam,col)))
            }))]
        }
        if(drop_cols & 
           degree >= 3 & 
           !include_quadratic_interactions){
          cat_num_interactions <- 
            cat_num_interactions[,-which(sapply(colnames(cat_num_interactions),function(col){
              any(sapply(paste0("degree=",2),function(nam)grepl(nam,col)))
            }))]
        }
        ##### print('normcat cols')
        cat_interactions <-
          foreach(i = 1:ncol(design_mat_cat),
                  .combine = 'cbind') %do% {
                    foreach(j = c(1:ncol(design_mat_cat))[(c(1:ncol(design_mat_cat)) != i)],
                            .combine = 'cbind') %do% {
                              (design_mat_cat[, j]) * 
                              (design_mat_cat[, i])
                            }
                  }
        colnames(cat_interactions) <-
          foreach(i = 1:ncol(design_mat_cat),
                  .combine = 'c') %do% {
                    foreach(j = c(1:ncol(design_mat_cat))[(c(1:ncol(design_mat_cat)) != i)],
                            .combine = 'c') %do% {
                              paste0(colnames(design_mat_cat)[j],
                                     "xXx",
                                     colnames(design_mat_cat[i]))
                            }
                  }
        # remove redundant interactions between linearly-independent categories
        if(drop_cols){
          design_mat_cat <-
            design_mat_cat[, apply(design_mat_cat, 2, function(x)
              any(x != 0))]
        } else {
          design_mat_cat <-
            design_mat_cat[, apply(design_mat_cat, 2, function(x)
              any(x != -Inf))]
        }
        #### print('num interactions')
        ## numeric interactions
        num_interactions <-
          foreach(i = seq(1, ncol(design_mat_num), by = degree), .combine = 'cbind') %do% {
            foreach(j = c(seq(1, ncol(
              design_mat_num
            ), by = degree))[c(seq(1, ncol(design_mat_num), by = degree)) > i],
            .combine = 'cbind') %do% {
              design_mat_num[, j] * design_mat_num[, i]
            }
          }
        
        colnames(num_interactions) <- foreach(i = seq(1, ncol(design_mat_num), by = degree), .combine = 'c') %do% {
          foreach(j = c(seq(1, ncol(
            design_mat_num
          ), by = degree))[c(seq(1, ncol(design_mat_num), by = degree)) > i],
          .combine = 'c') %do% {
            paste0(colnames(design_mat_num)[j],
                   "xXx",
                   colnames(design_mat_num)[i])
          }
        }
        
        ## now, adjust the numeric interactions so they are conditional on category
        if(!is.null(ncol(num_interactions))){
          cat_num_num_interactions <-
            foreach(i = first_ind:ncol(design_mat_cat), # drop first category
                    .combine = 'cbind') %do% {
                      foreach(j = seq(1, ncol(num_interactions), by = 1),
                              .combine = 'cbind') %do% {
                                design_mat_cat[, i] * 
                                  num_interactions[, j]
                              }
                    }
          sq <- seq(1, ncol(num_interactions), by = degree)
          cat_num_num_interactions <- cbind(cat_num_num_interactions)
          #print(head(cat_num_num_interactions))
          colnames(cat_num_num_interactions) <-
            foreach(i = first_ind:ncol(design_mat_cat),
                    .combine = 'c') %do% {
                      foreach(j = seq(1, ncol(num_interactions), by = 1),
                              .combine = 'c') %do% {
                                paste0(colnames(design_mat_cat)[i],
                                       "xxYYzz",
                                       colnames(num_interactions)[j])
                                
                              }
                    }
        }
        
        #### print('binding')
        if (ncol(cat_interactions) > 0 & qualify_numeric_numeric_interactions) {
          interactions <-
            cbind(cbind(cat_num_interactions, cat_interactions),
                  num_interactions,
                  cat_num_num_interactions)
        } else if (ncol(cat_interactions) > 0 & !qualify_numeric_numeric_interactions) {
          interactions <-
            cbind(cbind(cat_num_interactions, cat_interactions),
                  num_interactions)
        } else{
          interactions <- cbind(cat_num_interactions, num_interactions)
        }
      } else {
        ## when only numeric variables are being tuned
        interactions <-
          foreach(i = seq(1, ncol(design_mat_num), by = degree), .combine = 'cbind') %do% {
            foreach(j = c(seq(1, ncol(
              design_mat_num
            ), by = degree))[c(seq(1, ncol(design_mat_num), by = degree)) > i],
            .combine = 'cbind') %do% {
              design_mat_num[, j] * design_mat_num[, i]
            }
          }
        
        colnames(interactions) <- foreach(i = seq(1, ncol(design_mat_num), by = degree), .combine = 'c') %do% {
          foreach(j = c(seq(1, ncol(
            design_mat_num
          ), by = degree))[c(seq(1, ncol(design_mat_num), by = degree)) > i],
          .combine = 'c') %do% {
            paste0(colnames(design_mat_num)[j],
                   "xXx",
                   colnames(design_mat_num)[i])
          }
        }
      }
      
      ## if there are just categorical, try to make interactions
    } else {
      if (ncol(design_mat_cat) > 1) {
        interactions <-
          foreach(i = 1:ncol(design_mat_cat),
                  .combine = 'cbind') %do% {
                    foreach(j = c(1:ncol(design_mat_cat))[c(1:ncol(design_mat_cat)) != i], .combine = 'cbind') %do% {
                      design_mat_cat[, j] * design_mat_cat[, i]
                    }
                  }
        colnames(interactions) <-
          foreach(i = 1:ncol(design_mat_cat),
                  .combine = 'c') %do% {
                    foreach(j = c(1:ncol(design_mat_cat))[c(1:ncol(design_mat_cat)) != i],
                            .combine = 'c') %do% {
                              paste0(colnames(design_mat_cat)[j],
                                     "xXx",
                                     colnames(design_mat_cat)[i])
                            }
                  }
        ## remove redundant interactions between linearly-independent categories
        if(drop_cols){
          interactions <-
            interactions[, apply(interactions, 2, function(x)
              any(x != 0))]
        } else {
          interactions <-
            interactions[, apply(interactions, 2, function(x)
              any(x != -Inf))]
        }
        if (ncol(interactions) == 0) {
          interactions <- NULL
        }
      }
    }
    
    ## combine into one design matrix
    #### print('merging design matrices')
    if (is.null(design_mat_cat)) {
      if(drop_cols){
        design_mat_num <-
          design_mat_num[, which(apply(design_mat_num, 2, function(x)
            length(unique(x)) > 1))]
      } else {
        design_mat_num <-
          design_mat_num[, which(apply(design_mat_num, 2, function(x)
            length(unique(x)) > -Inf))]
      }
      design_mat <- cbind(design_mat_num)
    } else if (is.null(design_mat_num)) {
      design_mat <- cbind(design_mat_cat)
    } else {
      if(drop_cols){
        design_mat_num <-
          design_mat_num[, which(apply(design_mat_num, 2, function(x)
            length(unique(x)) > 1))]
      } else {
        design_mat_num <-
          design_mat_num[, which(apply(design_mat_num, 2, function(x)
            length(unique(x)) > -Inf))]
      }
      design_mat <- as(cbind(design_mat_num, design_mat_cat),
                       'matrix')
    }
    if (!is.null(interactions)) {
      colnames(interactions) <-
        ## interactions get penalized equal to the highest degree being fit + 1 + 1(3-way interaction)
        paste(colnames(interactions), paste0("",
                                             ifelse(grepl('xZx',colnames(interactions)), # keep degree of numeric for the catnum quadratic interactions right!!
                                                    "",
                                                    paste0("_degree=",#degree# + 
                                                           2 + # pairwise interactions
                                                             1*grepl('xxYYzz',colnames(interactions)) # 3-way interactions
                                                    ))), 
              sep = "")
      design_mat <- cbind(design_mat, interactions)
    }
    
    ## exclude columns with only 1 unique value, unless of course, the column is the intercept column
    if(drop_cols){
      design_mat <- design_mat[,
                               which(sapply(1:ncol(design_mat),
                                            function(x)
                                              (length(unique(design_mat[, x])) > 1)|
                                              (colnames(design_mat)[x])=="intercept"))]
    } else {
      design_mat <- design_mat[,
                               which(sapply(1:ncol(design_mat),
                                            function(x)
                                              (length(unique(design_mat[, x])) > -Inf)|
                                              (colnames(design_mat)[x])=="intercept"))]
    }
    #### print('finished making design matrix')
    return(design_mat)
  }
  
  ## make first, cubic regression design matrix
  design_mat <- make_design_matrix(grid,drop_cols = T)
  ## scale so all variables have equal sd
  sds <- apply(design_mat,2,function(x)if(any(!(unique(x) %in% c(0,1)))) sd(x) else 1)
  for(j in 1:length(sds)){
    design_mat[,j] <- 
    design_mat[,j]/sds[j]
  }
  
  ## save results to outcome vector
  y <- c()
  if(!include_numeric_interactions){
    design_mat <- design_mat[,!grepl('xXx',colnames(design_mat))]
  }
  
  ## print design matrix column names
  if(print_colnames){
    print(cbind(colnames(design_mat)))
  }
  
  ## for how many obs we need before adding a new spline
  p0 <- ncol(design_mat)
  if(is.null(obs_per_spline_coefficient)){
    obs_per_spline_coefficient <- function(num_knots)
      ## change 1: 1/9/2023
      ceiling(1/(length(c(numerics,
                          integers,
                          cats))^(1.5) + 1) * (p0 *(num_knots + 1))^(1.5))
  }
  
  ## center the knot quantiles 
  colz <- colnames(design_mat)
  ov <- colMeans(cbind(design_mat[,grepl('_degree=1',colz) &
                                    substr(colz,1,nchar(colz) - 9) %in% 
                                    names(c(numerics,integers))]))
  ##############################################################################
  set_optim_method(
    object,
    fun = function(optim, grid, ...) {
      grid <- grid[grid_inds_to_keep,]
      
      
      ## how many points are we initially going to evaluate?
      initial_iterations <- initial_points
      old_times <- times
      times <- max(0, times - initial_iterations)
      ## no more than this amount will be initially randomly sampled
      if(initial_iterations > initial_points){
        initial_iterations <- initial_points
      }
      print(paste0(c('initial iterations: ', initial_iterations),collapse = ""))
      ## if there are 3 or less rows, just evaluate them all and be done with it
      if(initial_iterations >= nrow(grid)){
        for(i in 1:nrow(grid)){
          optim(grid[i,])
        }
      } else{
        
        grid_points <- sample(1:nrow(grid),initial_iterations)
        
        ## for each grid point randomly sampled
        count <- 1
        for (i in grid_points){
          opt <- optim(grid[i, ])
          if(count == 1 & any(!is.na(opt))){
            count <- 2
            metrics <- tolower(unique(names(na.omit(opt)))[1])
            if(metrics == 'c-index' | metrics == 'cindex'){
              metric <- metricinfo('cindex')[[1]]
            } else {
              metric <- metricinfo(metrics[[1]])[[1]]
            }
          }
          y[paste0(i)] <- opt
        }
        
        if((metrics == 'c-index' | metrics == 'cindex')){
          metrics <- 'cindex'
        } 
        raw_y <- y
        best_y <- max(y,na.rm = T)
        steps_since_best <- 0
        transf_sign <- ifelse(metric$maximize,1,-1)
        if(!use_transf){
          if(is.null(log_outcome)){
            log_outcome <- F
          }
          if(log_outcome){
            y <- transf_sign*log(transf_sign*y + sqrt(1e-16))
          }
          if(is.null(logit_outcome)){
            logit_outcome <- F
          }
          if(logit_outcome){
            y <- transf_sign*(log(transf_sign*y+sqrt(1e-16)) - log(1-transf_sign*y+sqrt(1e-16)))
          }
        } else {
          #### print("starting to transform")
          #print(y)
          if(metrics %in% gengamma_like_metrics){
            ##### print("doing gamma like")
            names_y <- names(y)
            y <- transf_sign*y
            which <- which(!is.nan(y) &
                             is.finite(y) &
                             !is.na(y))
            shap_l <- min(length(y[which]),5000)
            if(length(unique(y)) > 1){
              #print(length(y))
              pwr_lmbda_opt <- try({optimize(
                f = function(l){
                  z <- (transf(y[which],'gengamma',pwr_lmbda = l))
                  -log(shapiro.test(sample(z,shap_l)
                  )$p.value
                  )
                },
                lower = -1,
                upper = 1
              )[[1]]},silent = TRUE)
              if(any(class(pwr_lmbda_opt) == 'try-error')){
                print(pwr_lmbda_opt)
                pwr_lmbda_opt <- 1
              }
              y <- transf_sign*c(transf(y, 
                                        'gengamma',
                                        pwr_lmbda = pwr_lmbda_opt))
              names(y) <- names_y
            } else {
              pwr_lmbda_opt <- 1
            }
            
            #print('finished')
          } else if(metrics %in% binom_like_metrics){
            #### print("doing binom like")
            y <- transf_sign*y
            y <- transf_sign*transf(y,'binom')
          }  else if(metrics %in% interval_like_metrics){
            #### print("doing beta like")
            y <- transf_sign*y
            y <- transf_sign*transf(y,'beta')
          } else {
            #### print("doing nonelike")
            y <- transf_sign*y
          }
          #### print("finished transform")
          #print(y)
        }
        
        
        ## start cleaning out potential outliers when modelling 
        y[!is.finite(y)] <- NA
        lm <- lm(y[!is.na(y)] ~ 1)
        outliers <- which(abs(resid(lm)-mean(resid(lm)))/sd(resid(lm)) > 10)
        y[!is.na(y)][outliers] <- NA
        cs <- center_and_scale(y)
        #### print("center and scale")
        y <- cs[[1]]
        mu <- cs[[2]]
        sig <- cs[[3]]
        
        inds_to_rem <- which(is.na(y))
        if(length(inds_to_rem) > 0){
          nas <- as.numeric(names(y[inds_to_rem]))
        } else{
          nas <- NULL
        }
        nas <- as.numeric(names(y[inds_to_rem]))
        
        #### print("Starting to Construct Linear Algebra Components")
        ## fitted values for randomly-sampled (non-weighted) grid points
        X <- design_mat[grid_points[!(grid_points %in% nas)],]
        Y <- y[!(as.numeric(names(y)) %in% nas)]
        max_transf_Y <- max(Y)
        XX <- t(X) %*% X
        
        
        ## make R ##
        #### print(" making R matrix ")
        secderiv <- 
          Reduce("+",lapply(names(c(numerics,integers)),
                            function(v){
                              second_deriv(design_mat,
                                           v,
                                           sds) +
                                interact2_deriv(design_mat,v,sds)
                            }))
        secderiv_og <- secderiv
        R <- ((t(secderiv) / sqrt(nrow(design_mat))) %*% 
                (secderiv  / sqrt(nrow(design_mat))))
        
        
        
        ## else, L.O.O. Cross-Validation for Choosing Ridge Penalty And Slope Fxn
        #### print("starting L.O.O. cross-val")
        tXy <- t(X) %*% Y
        opt_ridge <- try({optimStats(
          par = c(generic_tolerance, 1,generic_tolerance),
          function(par){
            m <- par[1]
            g <- par[2]
            lr <- par[3]
            rp <- diag(sapply(colnames(design_mat),function(x){
              nchar_x <- nchar(x)
              if(substr(x,nchar_x - 7,nchar_x - 1)=='degree='){
                m^(1/(as.numeric(substr(x,nchar_x,nchar_x))^g))
              } else {
                m
              }
            }))
            info <- XX + rp + lr*R
            sc <- sqrt(as.numeric(norm(info,'2')))
            V <- solvetry(info / sc) / sc
            XV <- X %*% V
            diagH <- diag(XV %*% t(X))
            fitted_resids <- Y - XV %*% (tXy )
            mean(abs(fitted_resids/(1-diagH)))
          },
          lower = c(generic_tolerance^4,0,generic_tolerance^4),
          upper = c(1,10,1),
          method = "L-BFGS-B")$par},silent= T)
        if(any(class(opt_ridge) == 'try-error')){
          ##print(opt_ridge)
          opt_ridge <- c(generic_tolerance,1,generic_tolerance)
        }
        #print(opt_ridge)
        #### print("finished L.O.O. cross-val")
        
        ridge_penalty_adjusted_upd <- sapply(colnames(design_mat),function(x){
          if(substr(x,nchar(x)-7,nchar(x)-1)=='degree='){
            (opt_ridge[1])^(1/(as.numeric(substr(x,nchar(x),nchar(x)))^opt_ridge[2]))
          } else {
            (opt_ridge[1])
          }
        })
        G <- solvetry(XX + diag(ridge_penalty_adjusted_upd) + opt_ridge[3]*R)
        diagH <- diag(X %*% G %*% t(X))
        
        #### print("fitting beta new now")
        beta_new <- G %*% tXy
        mu_beta <- beta_new
        
        #### print("making inds2keep")
        betaknot_new <- rep(0,ncol(X)*(num_knots + 1))
        G_knot <- diag(rep(0,ncol(X)*(num_knots+1)))
        Amat2 <- G_knot[,1:2]
        sigmaSq_knot <- 0
        
        preds <- c(design_mat[-as.numeric(grid_points),] %*% beta_new)
        preds[is.na(preds)] <- min(preds,na.rm = T)
        preds[!is.finite(preds)] <- min(preds,na.rm = T)
        
        #### print("estimate sigmaSq") # - note this is leave one out
        ## jackknife bias-corrected estimate of variance
        residsq <- (Y - X %*% beta_new)^2
        sigmaSq <- length(Y)*(mean(residsq) - (1-1/length(Y))*mean(residsq/diagH))
        if(sigmaSq <= 0){
          sigmaSq <- mean(residsq/diagH)
        }
        
        if(!is.finite(sigmaSq) | (sigmaSq <=0) | is.nan(sigmaSq) | is.na(sigmaSq)){
          kernel <- rep(0,length(preds))
          phi <- Inf
          weights <- exp(kernel - log(sum(exp(kernel))))
        } else if(prob_random > 0){
          
          #### print("mvnorming 2")
          n <- nrow(X)
          r <- qr(XX)$rank
          
          ## if posterior-distr degenerate, 50/50 chance of plugging in
          ## jackknife estimate as-if fixed, or randomly sampling a grid point
          if(((n - r) <= 2)){
            ## change 2: 1/9/2023
            ## there is now a 1/3 of picking the best one as well
            sample_random <- as.logical(rbinom(1,1,0.5))
            prob_random <- 2/3
          } else {
            sample_random <- F
            #sample_random <- as.logical(rbinom(1,1,0.5))
            prob_random <- 1
          }
          sigSq <- my_rinvgamma(1, 
                                -1 + max(n - r,3)/2, 
                                0 + (sum(Y*Y) - 
                                       sum(c(tXy)*c(solvetry(XX)%*%tXy)))/2,
                                sigmaSq)
          svd <- svd(G)
          #print('finished svd and mult')
          thompson_sample <- sqrt(sigSq) * 
            (t(t(svd$u) *
                 (sqrt(ifelse(svd$d > 0, svd$d,0)))) %*%
               (rnorm(length(beta_new)))) + 
            beta_new
          #### print("just making predictions")
          thompson_preds <- design_mat[-as.numeric(grid_points),] %*% 
            thompson_sample
          #### print("finished making predictions")
          #}
          
          #### print("assigning weights")
          ## fix, if we want to randomly sample a point
          if(sample_random){
            weights <- sample(c(1,rep(0,length(thompson_preds)-1)))
          } else if(!is.null(acquisition_function)){
            weights <- acquisition_function(thompson_preds,
                                            sqrt(sigSq),
                                            max(Y),
                                            length(Y),
                                            preds,
                                            sqrt(jackknife_sigSq_est),
                                            ...)
          } else {
            weights <- ifelse(fixna(thompson_preds) == max(fixna(thompson_preds)),
                              1,
                              0)
          }
          
          #### print("finished weights (other one)")
        } else {
          #kernel <- (preds - mean(Y)) / sigmaSq
          weights <- 1*(preds == max(preds))
        }
        
        
        if(length(preds) == 1){
          
          ## if only one grid point left, evaluate it
          new_grid_point <- c(1:nrow(grid))[-grid_points]
          
          #### print('sampled new point')
          ####
          
        } else if (length(preds) > 1){
          
          # initialize
          steps <- 0
          adjust <- (substr(colnames(design_mat),
                            nchar(colnames(design_mat))-7,
                            nchar(colnames(design_mat))-1)=='degree=')
          x <- colnames(design_mat)
          names(x) <- x
          nch <- nchar(x)
          names(nch) <- x
          names(adjust) <- x
          betaknot_new <- beta_new
          colsum0 <- c()
          while((length(preds) > 1) & (steps < (times))){
           
            #### print("starting iteration")
            
            ## are we going to randomly sample a grid point according to the computed weights?
            ## or are we going to be greedy use what our current model predicts as "best"
            rand <- rbinom(1,
                           1,
                           ifelse(steps == (times - 1), ## unless probability of random is specified
                                  0, ## the last Number of Unique Tuning Parameter Combinations Evaluated is the one predicted to be best no matter what, since beta will no longer be updated
                                  prob_random))
            ## candidate grid points for this round
            #### print("sampling")
            candidates <- c(c(1:nrow(grid))[-grid_points])
            if(length(colsum0) > 0 & enforce_positive_definiteness){
              ## evaluate random grid point of the partition that doesn't have p0 observations yet
              missing_candidates <- c(c(1:nrow(Q))[Q[,colsum0[1]]  == 1])
              new_grid_point <- sample(missing_candidates[!(missing_candidates %in% grid_points)],
                                       1)
            } else if(rand == 1){
              new_grid_point <- sample(candidates,
                                       1,
                                       prob = weights)
            } else {
              
              ## sample the best grid point deterministically 
              new_grid_point <- candidates[
                sample(which(preds == max(preds,na.rm = T)),1)]
              
              ## if this is the last iteration and we're optimizing
              if(steps == (times - 1)){
                ## use L-BFGS-B for optimizing over numerical predictors
                #### print("starting final optimization")
                if(length(c(numerics,integers)) >= 1){
                  optimal_ints <- as.numeric(unlist(grid[as.numeric(new_grid_point),])[c(numerics,integers)])
                  ## get min value above, and max value below, each numeric tuning parameter
                  #### print("starting ranges")
                  
                  ## match the columns 
                  
                  ranges <- t(foreach(j = 1:length(c(numerics,integers)),
                                      .combine = 'cbind') %do% {
                                        ## all unique values for the variable, excluding grid points sampled (thus, they can be included in any interval)
                                        unq <- unique(as.numeric(unlist(grid[candidates,c(numerics,integers)[j]])))
                                        ## the differences between unique values and variable
                                        diff <- unq - optimal_ints[j]
                                        ## the max is either the variable observed in the grid
                                        if(optimal_ints[j] == max(unq)){
                                          upper_bd <- optimal_ints[j]
                                        } else {
                                          ## or the smallest one larger than it
                                          upper_bd <- unq[diff == min(diff[diff > 0])]
                                        }
                                        ## same with lower bound
                                        if(optimal_ints[j] == min(unq)){
                                          lower_bd <- optimal_ints[j]
                                        } else {
                                          lower_bd <- unq[diff == max(diff[diff < 0])]
                                        }
                                        # return unique bounds
                                        c(lower_bd,upper_bd)
                                      })
                  # rownames(ranges) <- c(numerics,integers)
                  # print(ranges)
                  #### print("starting numeric optimization")
                  #print(as.numeric(unlist(grid[as.numeric(new_grid_point),])[numerics]))
                  opt <- try({optimStats(as.numeric(unlist(grid[as.numeric(new_grid_point),])[c(numerics,integers)]),
                                         function(par){
                                           grid_temp <- grid[as.numeric(new_grid_point),]
                                           if(length(numerics) > 0){
                                             for(j in 1:length(numerics)){
                                               grid_temp[,numerics[j]] <- par[j]
                                             }
                                           }
                                           if(length(integers) > 0){
                                             for(j in 1:length(integers)){
                                               grid_temp[,integers[j]] <- round(par[j])
                                             }
                                           }
                                           dm <- make_design_matrix(bind_rows(grid_temp,
                                                                              grid_temp),
                                                                    drop_cols = F)
                                           miss_cols <- colnames(design_mat)[!(colnames(design_mat) %in% colnames(dm))]
                                           if(length(miss_cols) > 0){
                                             for(col in miss_cols){
                                               names <- colnames(dm)
                                               dm <- cbind(dm,0)
                                               colnames(dm) <- c(names,col)
                                             }
                                           }
                                           newmat <- t(t(dm[,colnames(design_mat)])/sds)
                                           if(num_knots > 0){
                                             Qtemp <- sapply(1:(length(quants)+1),function(k){
                                               tempnorms <- apply(cbind(newmat),1,l2norm2)
                                               if(k == 1){
                                                 as.numeric(tempnorms >= min(norms) & tempnorms <= quants[k])
                                               } else if(k == (length(quants) + 1)){
                                                 as.numeric(tempnorms > quants[k-1] & tempnorms <= max(norms))
                                               } else{
                                                 as.numeric(tempnorms > quants[k-1] & tempnorms <= quants[k])
                                               }
                                             })
                                           } else {
                                             Qtemp <- cbind(rep(1,nrow(newmat)))
                                           }
                                           tmp <- knot_expand2(newmat,Qtemp)
                                           -sum(tmp[1,] * betaknot_new)
                                         },
                                         method = 'L-BFGS-B',
                                         lower = ranges[,1],
                                         upper = ranges[,2]
                  )},silent = T)
                  #### print("finished optimization")
                  if(class(opt) != 'try-error'){
                    #print(opt)
                    if(length(numerics) > 0){
                      for(j in 1:length(numerics)){
                        if(!any(is.nanf(c(opt$par)[j]))){
                          grid[as.numeric(new_grid_point),numerics[j]] <- c(opt$par)[j]
                        }
                      }
                    }
                    if(length(integers) > 0){
                      for(j in 1:length(integers)){
                        if(!any(is.nanf(c(opt$par)[j+length(numerics)]))){
                          grid[as.numeric(new_grid_point),
                               integers[j]] <- 
                            round(c(opt$par)[j + length(numerics)])
                        }
                      }
                    }
                  } else {
                    invisible()
                    print(opt)
                  }
                  #### print("finished editing")
                }
              }
            }
            
            ## plot predictions if desired
            if(plot_predicted_performance){
              plot_preds <- preds * sig + mu
              if(use_transf){
                if(metrics %in% gengamma_like_metrics){
                  plot_preds <- transf_sign*un_transf(transf_sign*plot_preds,'gengamma',pwr_lmbda = pwr_lmbda_opt)
                } else if(metrics %in% binom_like_metrics){
                  plot_preds <- transf_sign*un_transf(transf_sign*plot_preds,'binom')
                } else if(metrics %in% interval_like_metrics){
                  plot_preds <- transf_sign*un_transf(transf_sign*plot_preds,'beta')
                }
              }
              plot(design_mat[candidates,1]*sds[1],plot_preds,
                   xlab = paste0(substr(colnames(design_mat)[1],
                                        1,
                                        nchar(colnames(design_mat)[1]) - 
                                          nchar("_degree=1")),collapse = " "), 
                   ylab = paste0("Predicted Metric (",ifelse(metric$maximize,"","-"),
                                 ifelse(log_outcome,paste0("log-"),
                                        ifelse(logit_outcome,paste0('logit-'),
                                               paste0(""))),
                                 toupper(metrics[[1]]),
                                 ")"),
                   main = paste0("Predicted Performance of Unevaluated Grid Points: ", 
                                 steps+initial_iterations, 
                                 " Grid Points Evaluated so Far"))
              print(paste0(c(paste0(c("Best Observed: ", 
                                      best_y),
                                    collapse = ""),
                             paste0(c("Best Prediction: ", 
                                      max(plot_preds,na.rm = T)),
                                    collapse = "")),
                           collapse = " || "))
              
            }
            
            #### print('repeat cross val')
            ## repeat cross-validation
            for(gp in as.numeric(new_grid_point)){
              ## reset ys 
              #### print("optiming")
              y <- y * sig + mu
              opt <- optim(grid[gp,])
              if(!is.na(opt) & !is.nan(opt) & is.finite(opt)){
                #### print("maybe this will work")
                #print(c(opt,best_y))
                prev_best <- best_y
                best_y <- max(opt,best_y)
                if(prev_best != best_y){
                  steps_since_best <- 0
                }
              }
              #### print("finished optim")
              if(!use_transf){
                if(log_outcome){
                  y[paste0(gp)] <- transf_sign*log(transf_sign*opt+sqrt(1e-16))
                } else if(logit_outcome){
                  y[paste0(gp)] <- transf_sign*log(transf_sign*opt+sqrt(1e-16)) - 
                    transf_sign*log(1-transf_sign*opt+sqrt(1e-16))
                } else {
                  y[paste0(gp)] <- transf_sign*opt
                } 
              } else {
                if(metrics %in% gengamma_like_metrics){ 
                  #### print("doing it gengamma like")
                  raw_y <- un_transf(transf_sign*y,
                                     'gengamma',
                                     pwr_lmbda = pwr_lmbda_opt)
                  names(raw_y) <- names(y)
                  opt <- transf_sign*opt
                  which <- which(
                    !is.nan(c(raw_y,opt)) &
                      is.finite(c(raw_y,opt)) &
                      !is.na(c(raw_y,opt)))
                  #print(c(raw_y,opt))
                  shap_l <- min(length(c(raw_y,opt)[which]),5000)
                  if(length(unique(y)) > 1){
                    #print(length(y))
                    pwr_lmbda_opt <- try({optimize(
                      f = function(l){
                        z <- (transf(c(raw_y,opt)[which],
                                     'gengamma',
                                     pwr_lmbda = l))
                        -log(shapiro.test(sample(z,shap_l)
                        )$p.value
                        )
                      },
                      lower = -1,
                      upper = 1
                    )[[1]]},silent = TRUE)
                    if(any(class(pwr_lmbda_opt) == 'try-error')){
                      print(pwr_lmbda_opt)
                      pwr_lmbda_opt <- 1
                    }
                    y <- transf_sign*transf(c(raw_y,opt),
                                            'gengamma',
                                            pwr_lmbda = pwr_lmbda_opt)
                    names(y) <- c(names(raw_y),
                                  paste0(gp))
                    #### print("finished this part")
                  } #else { leave out, because we don't want to mess up what's there
                  #pwr_lmbda_opt <- 1
                  #}
                  #print('finished')
                } else if(metrics %in% binom_like_metrics){
                  #### print("doing it binom like")
                  opt <- transf_sign*opt
                  y[paste0(gp)] <- transf_sign*
                    transf(opt,'binom')
                } else if(metrics %in% interval_like_metrics){
                  #### print("doing it beta like")
                  opt <- transf_sign*opt
                  y[paste0(gp)] <- transf_sign*
                    transf(opt,'beta')
                } else {
                  #### print("doing it not at all")
                  y[paste0(gp)] <- transf_sign*opt
                }
              }
            }
            
            #### print("rem outliers")
            ## remove outliers
            y[!is.finite(y)] <- NA
            lm <- lm(y[!is.na(y)] ~ 1)
            outliers <- which(abs(resid(lm)-mean(resid(lm)))/sd(resid(lm)) > 10)
            y[!is.na(y)][outliers] <- NA
            cs <- center_and_scale(y)
            #### print("center and scale")
            #print(cs[-1])
            y <- cs[[1]]
            mu <- cs[[2]]
            sig <- cs[[3]]
            
            inds_to_rem <- which(is.na(y))
            if(length(inds_to_rem) > 0){
              nas <- as.numeric(names(y[inds_to_rem]))
            } else{
              nas <- NULL
            }
            
            ## include the new grid point into the previous grid
            grid_points <- unique(c(grid_points,new_grid_point))
            #### print("added grid point")
            
            if(new_grid_point %in% nas){
              #### print("na grid point")
            }
            
            #### print("re-fitting values")
            X <- design_mat[grid_points[!(grid_points %in% nas)],]
            Y <- y[!(as.numeric(names(y)) %in% nas)]
            max_transf_Y <- max(Y)
            if(!is.na(y[paste0(new_grid_point)])){
              if(num_knots == 0){
                
                
                X_knot <- X
                XX_knot <- t(X_knot) %*% X_knot
                tXy <- t(X_knot) %*% Y
                
                #### print("starting no-spline residual fit")
                log_init_penalt <- log(initial_ridge_penalty)
                opt_ridge_knot <- try({optimStats(
                  par = c(generic_tolerance, 1,generic_tolerance),
                  function(par){
                    m <- par[1]
                    g <- par[2]
                    lr <- par[3]
                    rp <- diag(
                      sapply(colnames(XX_knot),function(xx){
                        if(adjust[xx]){
                          m^
                            (1/(as.numeric(substr(x[xx],nch[xx],nch[xx]))^g))
                        } else {
                          m
                        }
                      })
                    )
                    info <- XX_knot + rp + lr*R
                    
                    sc <- sqrt(as.numeric(norm(info,'2')))
                    V <- solvetry(info / sc) / sc
                    XV <- X_knot %*% V
                    diagH <- diag(XV %*% t(X_knot))
                    fitted_resids <- Y - XV %*% (tXy )
                    mean(abs(fitted_resids/(1-diagH)))
                  },
                  lower = c(generic_tolerance^4,0,generic_tolerance^4),
                  upper = c(1,10,1),
                  method = "L-BFGS-B")$par},silent= T)
                #### print("finished opt ridge knot")
                if(any(class(opt_ridge_knot) == 'try-error')){
                  ##print(opt_ridge_knot)
                  opt_ridge_knot <- c(generic_tolerance,1,generic_tolerance)
                } else {
                  invisible()
                }
                
                ## find optimal ridge/prior precision matrix
                #print(opt_ridge_knot)
                knot_ridge <- diag(
                  sapply(colnames(XX_knot),function(xx){
                    if(adjust[xx]){
                      (opt_ridge_knot[1])^
                        (1/(as.numeric(substr(x[xx],nch[xx],nch[xx]))^opt_ridge_knot[2]))
                    } else {
                      (opt_ridge_knot[1])
                    }
                  })
                )
                infraw_knot <- XX_knot + knot_ridge + opt_ridge_knot[3]*R
                sc <- norm(infraw_knot,'2')
                G_knot <- solvetry(infraw_knot / sc) / sc
                betaknot_new <- G_knot %*% (tXy)
                mu_beta <- betaknot_new
                sigmaSq_knot <- mean((Y - X_knot %*% betaknot_new)^2)
                Amat2 <- G_knot[,1:2]
                KC <- 0*Amat2
                tGAmat2 <- t(G_knot) %*% KC
                U <- diag(1,nrow(G_knot))
                
                
                ##############################
                ## compute jackknife residuals
                #### print("starting meta model")
                VU <- G_knot
                denom <- 1 - diag(X_knot %*% VU %*% t(X_knot))
                YminusYhat <- Y-X_knot %*% betaknot_new
                loo_resids <- (YminusYhat)/denom
                loo_resids_og <- loo_resids
                jackknife_sigSq_est <- 
                  length(Y)*(mean(YminusYhat^2) - (1-1/length(Y))*mean(loo_resids_og^2))
                if(jackknife_sigSq_est <= 0){
                  jackknife_sigSq_est <- mean(loo_resids_og^2)
                }
                
                ###############################
                
                #### print("ending no-spline residual fit")
                if ((nrow(X_knot) > obs_per_spline_coefficient(num_knots+1)) & # at least 2 observations per coefficient about2b fit
                    (mean(abs(loo_resids)) > generic_tolerance)){ # the l1-norm of the residuals without the spline are not small
                  ## we have enough observations for a new spline
                  make_1knot <- T 
                  made_R <- F
                } 
                #### print("finished this part")
                
              } else {
                
                #########
                if(!made_R) {
                  secderiv <- knot_expand2(secderiv_og,
                                           Q)
                  R <- ((t(secderiv) / sqrt(nrow(design_mat))) %*% 
                          (secderiv  / sqrt(nrow(design_mat))))
                  made_R <- T
                }
                #########
                
                #### print("setting up temp norms")
                temp_gridnumtemp <- as(grid[grid_points,c(numerics,integers)],'matrix')
                temp_degree1mat <- t(t(temp_gridnumtemp) / sdtemp)
                temp_norms <- apply(cbind(temp_degree1mat),1,l2norm2)
                
                #### print("setting expanded X")
                X_knot <- knot_expand2(X,
                                       Q[grid_points[!(grid_points %in% nas)],])
                #print(X_knot)
                #print(colSums(X_knot))
                
                ## want at least p0 obs per polynomial partition
                ## if the sum of the intercept column of the design matrix
                ## within some polynomial partition is <= 2,
                ## we want to sample a bit more in that region
                ## in fact, we sample p0 points in that region
                if(enforce_positive_definiteness){
                  colsum0 <- which(colSums(X_knot[,which(colnames(X_knot) ==
                                                           'intercept')]) < p0)
                }
                
                #### print("indexing dun")
                ## make a knot constraint (KC) constraint matrix 
                #### print("making KC")
                knotmat <- design_mat[knots,]
                grid_temp <- grid[rep(knots,2), ]
                #### print("makin design mat")
                dm <- make_design_matrix(grid_temp,
                                         drop_cols = F) 
                miss_cols <- colnames(design_mat)[!(colnames(design_mat) %in% 
                                                      colnames(dm))]
                if(length(miss_cols) > 0){
                  for(col in miss_cols){
                    names <- colnames(dm)
                    dm <- cbind(dm,0)
                    colnames(dm) <- c(names,col)
                  }
                }
                knotmat_temp <- t(t(dm[,colnames(design_mat)])/
                                    sds)
                
                ## make a checkered quantile matrix for expansion, automatically does (1,-1,1,-1,...)
                Qcheckered <- cbind(Q[,1], Q[,-1]  - Q[,-ncol(Q)])
                obs_constr <- knot_expand2(knotmat_temp,
                                           Qcheckered[rep(knots,2),])
                
                ## compute derivatives
                #### print("first grad")
                first_gradient <- foreach(v = names(c(numerics,integers)),
                                          .combine = 'rbind') %do% {
                  fd <- first_deriv(knotmat_temp,v,sds)
                  knot_expand2(fd,
                               Qcheckered[rep(knots,2),])
                }
                #### print("second grad")
                second_gradient <- foreach(v = 
                                             names(c(numerics,integers)),.combine = 'rbind') %do% {
                                               secd <- (second_deriv(knotmat_temp,v,sds))+
                                                 interact2_deriv(knotmat_temp,v,sds)
                                               knot_expand2(secd,
                                                            Qcheckered[rep(knots,2),])
                                             }
                
                #### print('starting 2 bind')
                KC <- cbind(t(obs_constr),
                            t(first_gradient),
                            t(second_gradient))
                #print(KC)
                KC <- KC[,!apply(KC,2,function(x)sum(abs(x)) == 0)]
                KC <- KC[,apply(KC,2,function(x)sum((x)) == 0)]
                KC <- t(unique(t(KC)))
                #print(KC)
                #### print("finished making knot constraints")
                
                
                #### print("making new knot ridge")
                XX_knot <- t(X_knot) %*% X_knot
                Amat2 <- cbind(KC)
                tXy <- t(X_knot) %*% Y
                #### print("start PRESS optimization")
                opt_ridge_knot <- try({optimStats(
                  par = c(generic_tolerance, 1,generic_tolerance),
                  function(par){
                    #m <- exp(par[1]+log_init_penalt)
                    m <- par[1]
                    g <- par[2]
                    lr <- par[3]
                    rp <- diag(sapply(
                      colnames(X_knot),
                      function(x){
                        if(substr(x,nchar(x)-7,nchar(x)-1)=='degree='){
                          (m)^
                            (1/(as.numeric(substr(x,nchar(x),nchar(x)))^
                                  g))
                        } else {
                          m
                        }
                      }))
                    info <- XX_knot + rp + lr*R
                    sc <- sqrt(as.numeric(norm(infraw_knot,'2')))
                    V <- solvetry(info / sc) / sc
                    tGAmat2 <- V %*% Amat2
                    proj <- tGAmat2 %*% 
                      solvetry(t(Amat2) %*% 
                                 tGAmat2) %*% 
                      t(Amat2)
                    U <- diag(1,nrow(proj)) - proj
                    XUV <- X_knot %*% U %*% V
                    denom <- 1-diag(XUV %*% t(X_knot))
                    std_fitted_resids <- 
                      (Y - XUV %*% tXy)/
                      denom
                    mean(abs(std_fitted_resids))
                  },
                  lower = c(generic_tolerance^4,0,generic_tolerance^4),
                  upper = c(1,10,1),
                  method = "L-BFGS-B")$par},silent= T)
                if(any(class(opt_ridge_knot) == 'try-error')){
                  ##print(opt_ridge_knot)
                  opt_ridge_knot <- c(generic_tolerance,1,generic_tolerance)
                } else {
                  invisible()
                }
                #print(opt_ridge_knot)
                #### print("finished PRESS optimization")
                
                knot_ridge <- sapply(
                  colnames(X_knot),
                  function(x){
                    if(substr(x,nchar(x)-7,nchar(x)-1)=='degree='){
                      ((opt_ridge_knot[1]))^
                        (1/(as.numeric(substr(x,nchar(x),nchar(x)))^
                              opt_ridge_knot[2]))
                    } else {
                      (opt_ridge_knot[1])
                    }
                  })
                infraw_knot <- XX_knot + diag(knot_ridge) + opt_ridge_knot[3]*R
                sc <- sqrt(as.numeric(norm(infraw_knot,'2')))
                G_knot <- solvetry(infraw_knot / sc) / sc
                ##### print("finished block inversion")
                #### print("Use the method of Lagrange for solving constrained optimization")
                #print(c(nrow(X_knot),length(resids)))
                # tXy <- t(X_knot) %*% Y
                #### print("step 1 done")
                tGAmat2 <- G_knot %*% Amat2
                #### print("step 2 done")
                betaknot_new <- G_knot %*% (tXy)
                mu_beta <- betaknot_new
                #### print("raw betaknotnew")
                #print(round(betaknot_new,4))
                #### print("step 3 done")
                tGAmat2 <- cbind(tGAmat2)
                #print(tGAmat2)
                #print(dim(tGAmat2))
                #print(Amat2)
                #print(G_knot)
                if(any(dim(tGAmat2) <= 0)){
                  proj <- 0
                } else {
                  proj <- tGAmat2 %*% 
                    solvetry(t(Amat2) %*% 
                               tGAmat2) %*% 
                    t(Amat2)
                }
                U <- diag(1,length(betaknot_new)) - proj
                betaknot_new <- U %*% 
                  betaknot_new
                
                ##############################
                ## jackknife residuals
                #### print("starting jackknife estimation")
                VU <- G_knot %*% U
                denom <- 1-diag(X_knot %*% VU %*% t(X_knot))
                YminusYhat <- Y-X_knot %*% betaknot_new
                loo_resids <- (YminusYhat)/denom
                loo_resids_og <- loo_resids
                jackknife_sigSq_est <- 
                  length(Y)*(mean(YminusYhat^2) - (1-1/length(Y))*mean(loo_resids_og^2))
                if(jackknife_sigSq_est <= 0){
                  jackknife_sigSq_est <- mean(loo_resids_og^2)
                }
                
                
                ###############################
                #### print("finished lagrange")
                ## check for nas, compute the second variance component
                if(any(na.omit(c(is.na(betaknot_new),
                                 is.nan(betaknot_new),
                                 !is.finite(betaknot_new))))){
                  betaknot_new <- cbind(rep(0,length(c(betaknot_new))))
                  #print('no knot betas')
                  sigmaSq_knot <- 0
                } else {
                  new_resids <- Y - 
                    X_knot %*% 
                    betaknot_new
                  sigmaSq_knot <- mean(new_resids^2)
                  
                  ## check to see if we can add more knots
                  if((nrow(X_knot) > obs_per_spline_coefficient(num_knots+1)) & # at least 3 observations per coefficient
                     
                     (num_knots < max_knots) & # stop adding knots after some point
                     
                     (steps < (times - 2)) & # no adding knots prior to final iteration
                     
                     ((mean(abs(loo_resids))) > generic_tolerance) # residuals suggest a better fit is plausible
                  ){
                    #### print("expanded X has enough rows to fit a new spline; making MORE knots starting....... now!")
                    make_more_knots <- TRUE
                    made_R <- F ## mu0 temp needs to be reconstructed to match dimensions of 
                  }
                }
              }
            }
            
            ## get new predicted values and weights
            #### print("update predictions")
            expanded <- knot_expand2(design_mat[-as.numeric(grid_points),],
                                     cbind(cbind(Q)[-as.numeric(grid_points),]))
            preds <- 
              fixna(c(expanded %*% 
                        betaknot_new))
            
            # compute weights
            #### print("re-compute weights")
            if(!is.finite(sigmaSq_knot) | (sigmaSq_knot <=0) | is.nan(sigmaSq_knot) | is.na(sigmaSq_knot)){
              kernel <- rep(0,length(preds))
              phi <- Inf
              weights <- exp(kernel - log(sum(exp(kernel))))
            } else if(rand > 0){
              if(sum(abs(betaknot_new)) > 0){
                oog_expand <- expanded
              }
              
              
              
              svd <-  svd(G_knot)
              n <- nrow(X_knot)
              r <- qr(XX_knot)$rank
              ##### print("n - r")
              #print(n-r)
              q <- ncol(KC)
              half_varcov <- t(t(svd$u) * (sqrt(svd$d)))
              ginv_XX <- solvetry(XX_knot)
              reml <- (sum(Y^2) - sum(c(tXy)*c(ginv_XX%*%tXy)))
              ## if we have a rank-degenerate posterior,
              ## 1/3 chance of plugging in jackknife estimate of variance as fixed,
              ## or simply ignoring our model and choosing a random grid point,
              ## or choosing best predicted
              if(((n - r) <= 2)){
                ## change 2: 1/9/2023
                ## there is now a 1/3 of picking the best one as well
                sample_random <- as.logical(rbinom(1,1,0.5))
                prob_random <- 2/3
              } else {
                sample_random <- F
                #sample_random <- as.logical(rbinom(1,1,0.5))
                prob_random <- 1
              }
              sigSq <- ifelse(sum(abs(betaknot_new)) > 0,
                                   my_rinvgamma(1, 
                                                -1 + max(n - r,3)/2, 
                                                0 + reml/2,
                                                jackknife_sigSq_est),
                                   0)
              thompson_sample_knot <- 
                U %*% 
                (sqrt(sigSq) * 
                   (half_varcov %*%
                      (rnorm(length(betaknot_new)))) +
                   mu_beta)
              
              thompson_preds <- 
                oog_expand %*% 
                thompson_sample_knot
              
              ## fix, if we want to randomly sample a point
              if(sample_random){
                weights <- sample(c(1,rep(0,length(thompson_preds)-1)))
              } else if(!is.null(acquisition_function)){
                weights <- acquisition_function(thompson_preds,
                                                sqrt(sigSq),
                                                max(Y),
                                                steps,
                                                preds,
                                                jackknife_sigSq_est,
                                                ...)
              } else {
                weights <- ifelse(fixna(thompson_preds) == max(fixna(thompson_preds)),
                                  1,
                                  0)
              }
            } else {
              weights <- 1*(preds == max(preds,na.rm = T))
            }
            
            #### print("finished weights")
            
            ## update quantiles used for the design matrix/knots
            #### print("update quantile time")
            if(length(c(numerics,integers)) > 0 & (num_knots > 0 | make_1knot)){
              #print(num_knots)
              #### print("knot rules")
              if(!cardinal_knots){
                old_num_knots <- max(num_knots,1)
              }
              if(make_1knot){
                #### print("make 1 knot1")
                num_knots <- 1
                make_1knot <- F
                make_more_knots <- F
              } 
              if(make_more_knots){
                num_knots <- num_knots + 1
                make_more_knots <- F
              }
              
              ## the l2 norms of grid points evaluated
              norms <- c(cbind(prednorms)[as.numeric(grid_points),])
            
              
              ## if using cardinal knots, make the knots evenly spaced
              ## with respect to all grid points in the grid
              if(cardinal_knots){
                norm_quants <- sort(prednorms)[seq(1,
                                                   length(prednorms),
                                                   length.out = num_knots + 2)[
                                                     -c(1,num_knots + 2)]
                ]
              ## if using adaptive knots, make the knots evenly spaced 
              ## with respect to only the grid points thus far sampled
              } else if(old_num_knots < num_knots | num_knots == 1){
                norm_quants <- sort(norms)[seq(1,
                                               length(norms),
                                               length.out = num_knots + 2)[
                                                 -c(1,num_knots + 2)
                                               ]
                ]
              }
               
              
              old_quants <- quants
              quants <- norm_quants
              old_knots <- knots
              
              #### print("b")
              knots <- (sapply(quants,function(q){
                which(abs(prednorms-q) == min(abs(prednorms-q)))[1]
              }))
              
              
              #### print("c")
              ## knot groups
              if((num_knots + 1) > ncol(Q)){
                Q <- sapply(1:(length(quants)+1),function(k){
                  if(k == 1){
                    (prednorms >= min(prednorms))*(prednorms <= quants[k])
                  } else if(k == (length(quants) + 1)){
                    (prednorms > quants[k-1])*(prednorms <= max(prednorms))
                  } else {
                    (prednorms > quants[k-1])*(prednorms <= quants[k])
                  }
                })
              }
              
              #### print("d")
              #print(knots)
              if(plot_predicted_performance){
                  for(col in 1:length(old_quants)){
                    abline(v = cbind(grid)[old_knots[col],1],
                           col = "grey69")
                    }
                  }
            }
            #### print("updated quantiles completed")
            
            
            ## record steps, 
            steps_since_best <- steps_since_best + 1
            ## stopping rule
            if(!is.null(stopping_rule_total)){
              if((steps_since_best >= stopping_rule_total) & 
                 (num_knots >= 2) &
                 # mean of geometric is 1/p
                 # this is 1/probability of observing a value greater than current best
                 # meaning, this many expected steps until seeing something better
                 # if this is less than the number of steps we have left,
                 # we're done
                 (1/pnorm(
                  (
                    max_transf_Y-
                    max(preds))/sqrt(jackknife_sigSq_est),lower.tail = F) < 
                  (
                    times - length(y)))){
                ## stop early
                steps <- times - 1
                print("Stopping Early")
              }
            }
            steps <- steps + 1
            #print(c(steps,times,length(preds)))
            #### print("finished iteration")
            #print(c(num_knots,length(betaknot_new),sum(abs(betaknot_new))))
          }
          ## if desired, save the fitted coefficients, the design matrix, and the indices of grid points evaluated
          if(save_model_fit){
            save(betaknot_new,design_mat,X_knot,grid_points,file = save_model_fit_lable)
          }
        } else {
          NULL
        }
      }
      return(NULL)
    },
    label = "Custom Grid Search",
    random = random
  )
}
