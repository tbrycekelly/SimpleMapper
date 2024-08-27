#' @title Add Longitudinal Graticules to Map
#' @author Thomas Bryce Kelly
#' @importFrom graphics axis lines par
#' @param basemap basemap such as that from plotBasemap()
#' @param lons vector of longitude values (in degree easting) to overlayt
#' @param sides integer or vector of integers for which sides to add axes (if any).
#' @param col color value for the graticule lines
#' @param ... optional arguments passed on to lines()
#' @export
addLongitude = function(basemap,
                   lons = NULL,
                   sides = 1,
                   col = '#00000030',
                   ...) {
  
  par(las=1)
  usr = par()$usr
  
  if (is.null(lons)) {
    if (basemap$scale > 4e3) {
      lons = seq(-180, 180, by = 30)[-1]
    } else if (basemap$scale > 300) {
      lons = seq(-180, 180, by = 5)[-1]
    } else if (basemap$scale > 100){
      lons = seq(-180, 180, by = 1)[-1]
    } else if (basemap$scale > 50){
      lons = seq(-180, 180, by = 0.5)[-1]
    } else {
      lons = seq(-180, 180, by = 0.1)[-1]
    }
  }
  
  lons = standardize.longitude(lons)
  
  
  ## Invert projection of borders
  field = fieldOfView(basemap, 100)
  N = 300
  usr = par()$usr
  
  for (ll in lons) {
    tmp = basemap$projection(lon = rep(ll, N),
                             lat = seq(field$lat[1], field$lat[2], length.out = N),
                             lat0 = basemap$lat, lon0 = basemap$lon)
    
    k = tmp$x >= usr[1] & tmp$x <= usr[2] & tmp$y >= usr[3] & tmp$y <= usr[4]
    tmp = tmp[k,]
    tmp$x = tmp$x * 1.01 # Add 1% margin to FOV
    tmp$y = tmp$y * 1.01 # Add 1% margin to FOV
    
    lines(tmp$x, tmp$y, col = col)
    intersect = getIntersection(tmp$x, tmp$y)
    
    for (k in c(1:4)[1:4 %in% sides]) {
      if (!is.na(intersect[[k]][1])) {
        axis(k, at = intersect[[k]][((k+1) %% 2) + 1], labels = paste0(standardize.longitude(ll), 'E'))
      }
    }
  }
  
  par(las = 0)
  
  basemap
}

