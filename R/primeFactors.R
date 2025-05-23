#' @title Retrieve prime decomposition
#' @author Thomas Bryce Kelly
#' @param x a number to test for prime factors
#' @keywords prime
primeFactors = function(x) {
  x = round(x)
  f = c()
  k = 2
  while(x > 1) {
    if (x %% k == 0) {
      f = c(f, k)
      x = x / k
    } else {
      k = k + 1
    }
  }
  f
}
