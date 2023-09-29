#' @title Add Axis to Map
#' @author Thomas Bryce Kelly
#' @export
addAxis = function(basemap,
                   lons,
                   lats,
                   label.sides = c(1:4),
                   col = '#00000030',
                   ...) {
  
  
  ## Determine domain
  #field = expand.grid(x = seq(-1, 1, length.out = 25),
  #                    y = seq(-basemap$aspect.ratio, basemap$aspect.ratio, length.out = 25)) * basemap$scale
  
  #field = basemap$projection(field$x, field$y, lon0 = 0, lat0 = 0, inv = T) # assuming -180:180 and -90:90 as relative positions
  
  # TODO How to find out which entries to put, need an antimeridian flag!
  
  par(las=1)
  usr = par()$usr
  N = 5000 * 4e3 / diff(usr[1:2])
  
  ## Add lon axes to sides 1 & 2
  for (ll in lons) {
    tmp = basemap$projection(lon = rep(ll, N) - basemap$lon,
                             lat = seq(-90, 90, length.out = N),
                             lon0 = 0,
                             lat0 = basemap$lat)
    k = !is.na(tmp$x) & !is.na(tmp$y) & tmp$x >= usr[1] & tmp$x <= usr[2] & tmp$x >= usr[3] & tmp$y <= usr[4]
    
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
          
          if (diff(range(sign(tmp$y[split[[i]]] - usr[1]))) > 0 & 1 %in% label.sides) { # TODO add angle
            axis(1, at = approx(tmp$y[split[[i]]], tmp$x[split[[i]]], xout = usr[3], rule = 2)$y, labels = paste(ll, s))
          }
          if (diff(range(sign(tmp$y[split[[i]]] - usr[2]))) > 0 & 3 %in% label.sides) {
            axis(3, at = approx(tmp$y[split[[i]]], tmp$x[split[[i]]], xout = usr[4], rule = 2)$y, labels = paste(ll, s))
          }
        }
      }
    }
  }
  
  for (ll in lats) {
    tmp = basemap$projection(lon = seq(-180, 180, length.out = N) - basemap$lon,
                             lat = rep(ll, N),
                             lon0 = 0,
                             lat0 = basemap$lat)
    
    k = !is.na(tmp$x) & !is.na(tmp$y) & tmp$x >= usr[1] & tmp$x <= usr[2] & tmp$y >= usr[3] & tmp$y <= usr[4]
    
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
          
          if (diff(range(sign(tmp$x[split[[i]]] + usr[3]))) > 0 & 2 %in% label.sides) {
            axis(2, at = approx(tmp$x[split[[i]]], tmp$y[split[[i]]], xout = usr[2], rule = 2)$y, labels = paste(ll, s))
          }
          if (diff(range(sign(tmp$x[split[[i]]] - usr[4]))) > 0 & 4 %in% label.sides) {
            axis(4, at = approx(tmp$x[split[[i]]], tmp$y[split[[i]]], xout = usr[4], rule = 2)$y, labels = paste(ll, s))
          }
        }
      }
    }
  }
  par(las = 0)
  
  basemap
}

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

