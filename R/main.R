

## pascal triangle as a matrix

ptmatrix <- function(ptfn = 5){
  cat("default parameters: ptfn = 5", '\n', '\n')
  ptfmat <- matrix(0, ptfn+1, (ptfn*2)+1)
  ptfmat[1, ptfn+1] <- 1
  for (i in 1:(ptfn-1)) {
    for (j in 1:((ptfn*2)-1)) {
      ptfmat[i+1, j+1] <- ptfmat[i, j] + ptfmat[i, j+2]
    }
  }
  ptfmat[ptfmat==0] <-''
  ptfmnoqut <- noquote(ptfmat)
  return(noquote(prmatrix(ptfmat, collab = rep('', (ptfn*2)+1),
                          rowlab = rep('', ptfn+1))))
}


## Short R code for pascal triangle
short_code_pascal_triangle <- function(pt = 5){
  cat("default parameters: pt = 5", '\n', '\n')
  lapply(0:pt, FUN = function(x)choose(x, 0:x))
}


fibonacci <- function(fa = 1, fb = 8){
  cat("default parameters: fa = 1, fb = 8", '\n', '\n')
  x <- c(1,1)
  for (i in 1:fb) {
    x[length(x)+1] <- sum(x[length(x)-1], x[length(x)])
  }
  x[fa:fb]
}


fibonacci_2D <- function(rf2D = 4, cf2D = 5){
  cat("default parameters: rf2D = 4, cf2D = 5", '\n', '\n')
  a <- 1
  b <- cf2D
  c <- rf2D
  d <- (b-a)+c
  mapply(a:b, c:d, FUN = function(x,y)fibonacci(x,y))
}

