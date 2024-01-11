#' @title Boolean Vector Split
#' A helper function that takes a boolean vector and returns a list of indicies for each sequence of consecutive T/F values.
#' @author Thomas Bryce Kelly
#' @param vec a boolean vector to be split
splitBool = function(vec) {
  
  ## If all are bad, return empty
  if (all(!vec)) {
    return(list())
  }
  
  ret = list()
  breaks = unique(c(1, which(diff(vec) != 0)+1, length(vec)))
  
  for (i in 2:length(breaks)) {
    start = breaks[i - 1]
    end = breaks[i] - 1
    if (vec[start]) {
      ret[[paste(start)]] = c(start:end)
    }
  }
  
  ret
}