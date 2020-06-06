#' Largest odd, Density of primes
#'
#' This function is calculate n factorial and define density of primes
#'
#' nfact(3) prime(4)
nfact <- function(n) {
  if (n==1) {
    cat(" nfact2(1)\n")
    return(1)
  } else {
    cat(" nfact2(", n, ")\n", sep="")
    return(n*nfact(n-1))
  }
}

prime <- function(n) {
  if ( n==1 ) {
    is.prime <- FALSE
  } else if ( n==2) {
    is.prime <- TRUE
  } else {
    is.prime <- TRUE
    for ( m in 2:(n/2) ) {
      if ( n%%m == 0 ) is.prime <- FALSE
    }
  }
  return(is.prime)
}
