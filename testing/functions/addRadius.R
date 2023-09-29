#' @title Add Radius's to map
#' @author Laura Whitmore
#' @export
addRadius = function(basemap,
                     lon,
                     lat,
                     distance,
                     col = 'black',
                     lwd = 1,
                     lty = 1,
                     ...) {
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  x = distance * sin(seq(0, pi, length.out = 500))
  y = distance * cos(seq(0, pi, length.out = 500))
  
  ## Plot
  for (i in 1:length(tmp$x)) {
    lines(tmp$x[i] + c(x, -x), tmp$y[i] + c(y, -y))
  }
  
  basemap
}
