#' @title Add Axis to Map
#' @author Thomas Bryce Kelly
#' @importFrom graphics axis lines par
#' @param basemap basemap such as that from plotBasemap()
#' @param lons vector of longitude values (in degree easting) to overlayt
#' @param lats vector of latitude values (in degree northing) to overlay
#' @param sides integer or vector of integers for which sides to add axes (if any).
#' @param col color value for the graticule lines
#' @param ... optional arguments passed on to lines()
#' @export
addAxis = function(basemap,
                   lons,
                   lats,
                   sides = c(1:4),
                   col = '#00000030',
                   ...) {
  
  par(las=1)
  usr = par()$usr
  lons = standardize.longitude(lons - basemap$lon)
  
  
  ## Invert projection of borders
  field = fieldOfView(basemap, 100)
  N = 100
  
  for (ll in lons) {
    tmp = basemap$projection(lon = rep(ll, N),
                             lat = seq(field$lat[1], field$lat[2], length.out = N),
                             lat0 = basemap$lat)
    
    tmp$x = tmp$x * 1.01 # Add 1% margin to FOV
    tmp$y = tmp$y * 1.01 # Add 1% margin to FOV
    
    lines(tmp$x, tmp$y, col = col)
    intersect = getIntersection(tmp$x, tmp$y)
    
    for (k in c(1:4)[1:4 %in% sides]) {
      if (!is.na(intersect[[k]][1])) {
        axis(k, at = intersect[[k]][((k+1) %% 2) + 1], labels = paste0(ll, 'E'))
      }
    }
  }
  
  for (ll in lats) {
    tmp = basemap$projection(lon = seq(field$lon[1], field$lon[2], length.out = N),
                             lat = rep(ll, N),
                             lat0 = basemap$lat)
    lines(tmp$x, tmp$y, col = col)
    intersect = getIntersection(tmp$x, tmp$y)
    
    for (k in c(1:4)[1:4 %in% sides]) {
      if (!is.na(intersect[[k]][1])) {
        axis(k, at = intersect[[k]][((k+1) %% 2) + 1], labels = paste0(ll, 'N'))
      }
    }
  }
  
  par(las = 0)
  
  basemap
}



#' @title Utility Func
#' @author Thomas Bryce Kelly
#' @importFrom stats approx
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
