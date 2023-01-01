

## pascal triangle as a matrix

ptmatrix <- function(ptfn){
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
short_code_pascal_triangle <- function(pt){
  sapply(0:pt, FUN = function(x)choose(x, 0:x))
}

