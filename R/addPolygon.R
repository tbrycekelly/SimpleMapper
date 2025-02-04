#' @title Add Map Line
#' @author Thomas Bryce Kelly
#' @param basemap a basemap list object
#' @param lon a set of longitudes to draw a line between
#' @param lat a set of latitudes to draw a lien between
#' @param border boolean to turn on/off polygon border
#' @param col the color of the line to be drawn
#' @param lty the type of line to be drawn
#' @param lty the type of line to be drawn
#' @param lwd the width of the line to be drawn
#' @param ... optional arguments passed in lines()
#' @importFrom graphics lines
#' @export
addPolygon = function(basemap,
                   lon,
                   lat,
                   col = 'black',
                   border = T,
                   lty = 1,
                   lwd = 1,
                   ...) {
  
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addPolygon',
                                                        arguments = list(
                                                          lon = lon, lat = lat, col = col, border = border, lty = lty, lwd = lwd, ... = ...
                                                        )
  )
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  ## Plot
  graphics::polygon(tmp$x, tmp$y, col = col, border = border, lty = lty, lwd = lwd, ...)
  
  invisible(basemap)
}
