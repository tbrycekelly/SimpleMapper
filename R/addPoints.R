#' @title Add Map Points
#' @author Laura Whitmore
#' @param basemap a basemap list object
#' @param lon a set of longitudes to draw a line between
#' @param lat a set of latitudes to draw a lien between
#' @param col the color of the line to be drawn
#' @param pch pch
#' @param cex size
#' @param ... passed onto points()
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
  
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addPoints',
                                                        arguments = list(
                                                          lon = lon, lat = lat, col = col, pch = pch, cex = cex, ... = ...
                                                        )
  )
  
  invisible(basemap)
}