


# RndPairs <- function(n, r, rdist1 = rnorm(n=n, mean = 0, sd = 1), rdist2 = rnorm(n=n, mean = 0, sd = 1)){
# 
#   # create correlated random pairs
#   data.frame(matrix(nrow=n, ncol=2, data=cbind(rdist1, rdist2)) %*%
#                 chol(matrix(nrow=2, ncol=2, data=c(1, r, r, 1))))
# }


RndPairs <- function(n, r, rdist1 = rnorm(n=n, mean = 0, sd = 1), 
                     rdist2 = rnorm(n=n, mean = 0, sd = 1), prop=NULL) {
  
  # create correlated random pairs
  res <- data.frame(matrix(nrow=n, ncol=2, data=cbind(rdist1, rdist2)) %*%
                      chol(matrix(nrow=2, ncol=2, data=c(1, r, r, 1))))
  colnames(res) <- c("x","y")
  
  if(!is.null(prop)){
    
    if(is.list(prop)){
      propx <- cumsum(c(0, prop[[1]])) 
      propy <- cumsum(c(0, prop[[2]]))
      
    } else {
      propx <- propy <- cumsum(c(0, prop))
    }

    res$x <- cutQ(res$x, breaks = quantile(res$x, probs = propx))
    res$y <- cutQ(res$y, breaks = quantile(res$y, probs = propy))

  }
  
  return(res)  
  
}




TwoSamp <- function(n, colnames, glevels=NULL, DIST, long=TRUE){
  
  n <- rep(n, length.out=2)
  DIST <- rep(DIST, length.out=2)
  
  if(long) {
    res <- data.frame(do.call(c, sapply(seq(n), function(i) DIST[[i]](n[i]))),
                      rep(glevels, n))
  } else {
    res <- as.data.frame(sapply(seq(n), function(i) DIST[[i]](n[i])))
  }
  
  return(setNamesX(res, colnames=colnames))
  
}



