#' @title Add Latitudinal Graticules to Map
#' @author Thomas Bryce Kelly
#' @importFrom graphics axis lines par
#' @param basemap basemap such as that from plotBasemap()
#' @param lats vector of latitude values (in degree northing) to overlay
#' @param sides integer or vector of integers for which sides to add axes (if any).
#' @param col color value for the graticule lines
#' @param ... optional arguments passed on to lines()
#' @export
addLatitude = function(basemap,
                       lats = NULL,
                       sides = 2,
                       col = '#00000030',
                       ...) {
  
  par(las=1)
  usr = par()$usr
  
  if (is.null(lats)) {
    if (basemap$scale > 4e3) {
      lats = seq(-90, 90, by = 30)
    } else if (basemap$scale > 300) {
      lats = seq(-90, 90, by = 5)
    } else if (basemap$scale > 100) {
      lats = seq(-90, 90, by = 1)
    } else if (basemap$scale > 50) {
      lats = seq(-90, 90, by = 0.5)
    } else {
      lats = seq(-90, 90, by = 0.1)
    }
  }
  
  ## Invert projection of borders
  field = fieldOfView(basemap, 100)
  N = 100
  
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
