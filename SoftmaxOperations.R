# generalizes matrix multiplication for A %*% b when the elements of matrix A and vector b are themselves conformable matrices.
`%**%` <- function(A,b){
  list <- (lapply(1:length(A),function(j){
    A[[j]] %*% b[[j]]
  }))
  sapply(1:ncol(list[[1]]),function(j){
    rowSums(sapply(list,function(l)l[,j]))
  })
}

# generalizes t(A) %*% B when the elements of matrix A and matrix B are themselves comformable matrices. For use in the glmer_constrained() function.
`%*t1*%` <- function(A,B){
  sapply(1:length(B),function(j){
    cbind(sapply(1:length(A),function(i){
      sum(rowSums(A[[i]]*B[[j]]))
    }))
  })
}

# generalizes t(A) %*% B %*% C first step ( t(A) %*% B^1/2 ) when the elements of matrix A, B, C are comformable matrices. For use in the glmer_constrained() function.
`%*t2*%` <- function(A,B){
  lapply(1:length(B),function(j){
    sapply(1:length(A),function(i){
      ((A[[i]][j,]%*%B[[j]]))
    })
  })
}


# generalizes t(A) %*% B %*% C second step ( (t(A) %*% B^1/2) %*% (B^1/2 %*% C) ) when the elements of matrix A, B, C are comformable matrices. For use in the glmer_constrained() function.
`%*t3*%` <- function(A,C){
  dummy <- matrix(0,nrow = ncol(A[[1]]),
                  ncol = ncol(C[[1]]))

  ## break into batches to balance speed and memory
  splits <- seq(1,length(A),by = 10)
  splits <- lapply(splits,function(s)c(s+c(0:9)))
  for(b in 1:length(splits)){
    inds <- c(splits[[b]])
    inds <- inds[inds %in% c(1:length(A))]
    dummy <- dummy  +
      Reduce("+",lapply(inds,function(bb){
        t(A[[bb]]) %*% C[[bb]]
      }))
  }
  return(dummy)
}
