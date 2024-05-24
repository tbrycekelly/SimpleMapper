#' @title Utility Func
#' @author Thomas Bryce Kelly
#' @importFrom stats approx
#' @param x x coordinates of a line
#' @param y y coordinates of a line
#' @export
getIntersection = function(x, y) {
  usr = par()$usr
  k = !is.na(x) & !is.na(y)
  x = x[k]
  y = y[k]
  
  res = list(
    side1 = c(NA,NA),
    side2 = c(NA,NA),
    side3 = c(NA,NA),
    side4 = c(NA,NA)
  )
  
  if (sum(!is.na(x)) < 3 | sum(!is.na(y)) < 3) {
    return(res)
  }
  
  ## Side 1 and 3 (calculation in plotting space)
  # Order points in increasing y axis, determine intersection point:
  tmpx = x[order(y, na.last = T)]
  tmpy = y[order(y, na.last = T)]
  int = approx(tmpy, tmpx, xout = usr[3], method = 'constant')$y
  if (!is.na(int) & any(tmpy >= usr[3]) & any(tmpy <= usr[3])) {
    res$side1 = c(int, usr[3])
  }
  
  int = approx(tmpy, tmpx, xout = usr[4], method = 'constant')$y
  if (!is.na(int) & any(tmpy > usr[4]) & any(tmpy < usr[4])) {
    res$side3 = c(int, usr[4])
  }
  
  ## Side 2 and 4 (calculation in plotting space)
  # Order points in increasing x axis, determine intersection point:
  tmpx = x[order(x, na.last = T)]
  tmpy = y[order(x, na.last = T)]
  int = approx(tmpx, tmpy, xout = usr[1], method = 'constant')$y
  if (!is.na(int) & any(tmpx > usr[1]) & any(tmpx < usr[1])) {
    res$side2 = c(usr[1], int)
  }
  
  int = approx(tmpx, tmpy, xout = usr[2], method = 'constant')$y
  if (!is.na(int) & any(tmpx > usr[2]) & any(tmpx < usr[2])) {
    res$side4 = c(usr[2], int)
  }
  
  res
}