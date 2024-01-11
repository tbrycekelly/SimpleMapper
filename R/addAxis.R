#' @title Add Axis to Map
#' @author Thomas Bryce Kelly
#' @importFrom graphics axis box lines par points polygon text
#' @importFrom stats approx
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
  N = 5000 * 4e3 / diff(usr[1:2])
  dx = diff(usr[1:2]) / N * 1e3
  dy = diff(usr[3:4]) / N * 1e3
  
  ## Add lon axes to sides 1 & 2
  for (ll in lons) {
    tmp = basemap$projection(lon = rep(ll, N),
                             lat = seq(min(lats), max(lats), length.out = N),
                             lon0 = basemap$lon,
                             lat0 = basemap$lat)
    k = !is.na(tmp$x) & !is.na(tmp$y) & tmp$x >= usr[1] - dx & tmp$x <= usr[2] + dx & tmp$y >= usr[3] - dy & tmp$y <= usr[4] + dy
    
    if (sum(k) > 0) {
      tmp$x[!k] = NA
      tmp$y[!k] = NA
  
      ## split at breaks
      split = splitBool(k)
      if (length(split) > 0) {
        for (i in 1:length(split)) {
          ## Add lon lines
          lines(x = tmp$x[split[[i]]], y = tmp$y[split[[i]]], col = col, ...)
        
          ## Attempt to Label
          s = 'E'
          if (ll < 0) { 
            s = 'W'; ll = -ll 
          }
          
          if (diff(range(sign(tmp$y[split[[i]]] - usr[3]))) > 0 & 1 %in% sides) { # TODO add angle
            axis(1, at = approx(tmp$y[split[[i]]], tmp$x[split[[i]]], xout = usr[3], rule = 2)$y, labels = paste(ll, s))
          }
          if (diff(range(sign(tmp$y[split[[i]]] - usr[4]))) > 0 & 3 %in% sides) {
            axis(3, at = approx(tmp$y[split[[i]]], tmp$x[split[[i]]], xout = usr[4], rule = 2)$y, labels = paste(ll, s))
          }
        }
      }
    }
  }
  
  for (ll in lats) {
    tmp = basemap$projection(lon = seq(min(lons), max(lons), length.out = N),
                             lat = rep(ll, N),
                             lon0 = basemap$lon,
                             lat0 = basemap$lat)
    
    k = !is.na(tmp$x) & !is.na(tmp$y) & tmp$x >= usr[1] - dx & tmp$x <= usr[2] + dx & tmp$y >= usr[3] - dy & tmp$y <= usr[4] + dy
    
    if (sum(k) > 0) {
      tmp$x[!k] = NA
      tmp$y[!k] = NA
      
      ## split at breaks
      split = splitBool(k)
      
      if (length(split) > 0) {
        for (i in 1:length(split)) {
          ## Add lon lines
          lines(x = tmp$x[split[[i]]], y = tmp$y[split[[i]]], col = col, ...)
          
          ## Attempt to Label
          s = 'N'
          if (ll < 0) { 
            s = 'S'; ll = -ll 
          }
          
          if (diff(range(sign(tmp$x[split[[i]]] - usr[1]))) > 0 & 2 %in% sides) {
            axis(2, at = approx(tmp$x[split[[i]]], tmp$y[split[[i]]], xout = usr[2], rule = 2)$y, labels = paste(ll, s))
          }
          if (diff(range(sign(tmp$x[split[[i]]] - usr[2]))) > 0 & 4 %in% sides) {
            axis(4, at = approx(tmp$x[split[[i]]], tmp$y[split[[i]]], xout = usr[4], rule = 2)$y, labels = paste(ll, s))
          }
        }
      }
    }
  }
  par(las = 0)
  
  basemap
}