#' @title Add Map Points
#' @author Laura Whitmore
#' @export
addPoints = function(basemap,
                   lon,
                   lat,
                   col = 'black',
                   pch = 1,
                   cex = 1,
                   ...) {
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  ## Plot
  points(tmp$x, tmp$y, col = col, pch = pch, cex = cex, ...)
}