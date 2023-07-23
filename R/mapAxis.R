#' @title Add Axis to Map
#' @author Thomas Bryce Kelly
#' @export
mapAxis = function(basemap,
                   lons,
                   lats,
                   label.sides = c(1:4),
                   col = '#00000030',
                   ...) {
  
  
  ## Determine domain
  field = expand.grid(x = seq(-1, 1, length.out = 25),
                      y = seq(-basemap$aspect.ratio, basemap$aspect.ratio, length.out = 25)) * basemap$scale
  
  field = basemap$projection(field$x, field$y, lon0 = 0, lat0 = 0, inv = T) # assuming -180:180 and -90:90 as relative positions
  
  # TODO How to find out which entries to put, need an antimeridian flag!
  
  par(las=1)
  
  ## Add lon axes to sides 1 & 2
  for (ll in lons) {
    tmp = basemap$projection(lon = rep(ll, 100), lat = seq(-90, 90, length.out = 100),
                             lon0 = basemap$lon,
                             lat0 = basemap$lat)
    k = !is.na(tmp$x) & !is.na(tmp$y)
    
    if (length(k) > 0) {
      tmp = tmp[k,]
  
      ## Add lon lines
      lines(x = tmp$x, y = tmp$y, col = col, ...)
      
      ## Attempt to Label?
      s = 'E'
      if (ll < 0 | ll %% 360 > 180) { s = 'W'; ll = -ll }
      
      if (diff(range(sign(tmp$y + basemap$scale * basemap$aspect.ratio))) > 0 & 1 %in% label.sides) {
        axis(1, at = approx(tmp$y, tmp$x, xout = par()$usr[3])$y, labels = paste(ll, s))
      }
      if (diff(range(sign(tmp$y - basemap$scale * basemap$aspect.ratio))) > 0 & 3 %in% label.sides) {
        axis(3, at = approx(tmp$y, tmp$x, xout = par()$usr[4])$y, labels = paste(ll, s))
      }
    }
  }
  
  for (ll in lats) {
    tmp = basemap$projection(lon = seq(0, 360, length.out = 100),
                             lat = rep(ll, 100),
                             lon0 = basemap$lon,
                             lat0 = basemap$lat)
    
    k = !is.na(tmp$x) & !is.na(tmp$y)
    if (length(k) > 0) {
      tmp = tmp[k,]
  
      ## Add lon lines
      lines(x = tmp$x, y= tmp$y, col = col, ...)
      
      ## Attempt to Label?
      s = 'N'
      if (ll < 0) { s = 'S'; ll = -ll }
      if (diff(range(sign(tmp$x + basemap$scale))) > 0 & 2 %in% label.sides) {
        axis(2, at = approx(tmp$x, tmp$y, xout = par()$usr[2])$y, labels = paste(ll, s))
      }
      if (diff(range(sign(tmp$x - basemap$scale))) > 0 & 4 %in% label.sides) {
        axis(4, at = approx(tmp$x, tmp$y, xout = par()$usr[4])$y, labels = paste(ll, s))
      }
    }
    
  }
  par(las = 0)
  
  invisible()
}
