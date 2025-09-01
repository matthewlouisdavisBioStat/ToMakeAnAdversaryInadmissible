#' A straightforward non-vectorized way of extending the flexibility of ifelse
#' @param cond condition to evaluate boolean for.
#' @param x number of variables randomly sampled as candidates at each split.
#' @param y should sampling of cases be done with or without replacement?
#'
#' @details This increases the flexibility of ifelse when potential inputs and outputs are of different form and/or length.
#'
#' @return Object of arbitrary class.
#'
#' @examples
#' \donttest{
#'
#'   ifelse2(2 > 3, "2 > 3", "3 > 2")
#' }
#' @export
ifelse2 <- function(cond,x,y){
  if(cond) x else y
}

#' Operation that makes 0 times infinity = 0, useful for modelling purposes.
#' @param a a real number possibly equivalent to 0.
#' @param b an integer, numeric, vector, matrix, or other data type that can be scaled.
#'
#' @details This function ensures that when multiplying something going to infinity by something equivalent to 0, 0 wins out (the entire expression becomes 0).
#'
#' @return The object b scaled by a.
#'
#' @examples
#' \donttest{
#'
#' fix_inf(1/0,0)
#' }
#' @export
fix_inf <- function(a,b){
  if (a == 0) {
    return(0*b)
  } else{
    c <- a * b
    if (is.nan(c)) {
      return(0*b)
    } else {
      c
    }
  }
}

#' quick way to invert (or alternatively use ginv to find the generalized inverse of) a non-negative definite symmetric matrix utilizing the singular value decomposition
#' @param mat a symmetric square matrix to be inverted (or find generalized inverse of).
#'
#' @details This function ensures that when multiplying something going to infinity by something equivalent to 0, 0 wins out (the entire expression becomes 0).
#'
#' @importFrom MASS ginv
#' @return A matrix, the inverse (or Moore-Penrose generalized inverse of) the matrix inputted.
#'
#' @examples
#' \donttest{
#'
#' quick_inv(sapply(1:100,function(i)rnorm(100)))
#' }
#' @export
quick_inv <- function(mat){
  svd_comp <- svd(mat)
  D <- diag(svd_comp$d)

  ## when not p.d., we have to take the ginv.....
  if(min(round(diag(D),6)) <= 0.0000001){
    return(ginv(mat))

    ## else, the svd decomp works faster than solve() for large p.d. matrices
    ## time saved by sapply/Reduce/vectorization was not worth the potential memory issues
    ## a for loop is fast enough
  } else {
    P <- svd_comp$v
    G <- matrix(c(0),
                nrow = unique(dim(mat)),
                ncol = unique(dim(mat)))
    for(i in 1:unique(dim(mat))){
      G <- G + t(1/D[i,i] * P[,i] %*% t(P[,i])) # have to transpose when inverting
    }
    return(G)
  }
}

#' kronecker product a list of matrices into one block-diagonal
#' @param matlist a list of matrices to Kronecker Product.
#'
#' @details This function is helpful for creating large, diagonal matrices given some list of individual, possibly non-conformable matrices.
#'
#' @import foreach
#' @return A matrix, the inverse (or Moore-Penrose generalized inverse of) the matrix inputted.
#'
#' @examples
#' \donttest{
#'
#' kronk(lapply(1:2,function(j)(sapply(1:10,function(i)rnorm(10)))))
#' }
#' @export
kronk <- function(matlist){
  nrows <-     unlist(lapply(matlist,nrow))
  ncols <-     unlist(lapply(matlist,ncol))
  foreach(m = 1:length(matlist),.combine = "rbind") %do% {
    matt <- matrix(0,nrow = nrows[m],
                   ncol = sum(ncols))
    matt[,sum(ncols[-c(m:length(nrows))]) +
           1:ncols[m]] <- matlist[[m]]
    matt
  }
}
