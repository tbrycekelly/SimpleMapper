#' @title Add Map Line
#' @author Thomas Bryce Kelly
#' @param basemap a basemap list object
#' @param lon a set of longitudes to draw a line between
#' @param lat a set of latitudes to draw a lien between
#' @param col the color of the line to be drawn
#' @param lty the type of line to be drawn
#' @param lty the type of line to be drawn
#' @param lwd the width of the line to be drawn
#' @param ... optional arguments passed in lines()
#' @importFrom graphics lines
#' @export
addLine = function(basemap,
                        lon,
                        lat,
                        col = 'black',
                        lty = 1,
                        lwd = 1,
                        ...) {
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  ## Plot
  graphics::lines(tmp$x, tmp$y, col = col, lty = lty, lwd = lwd, ...)
  
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addLine',
                                                        arguments = list(
                                                          lon = lon, lat = lat, col = col, lty = lty, lwd = lwd, ... = ...
                                                        )
  )
  
  invisible(basemap)
}
