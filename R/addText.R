#' @title Add Map Text
#' @author Laura Whitmore
#' @param basemap a basemap list object
#' @param lon a set of longitudes to draw a line between
#' @param lat a set of latitudes to draw a lien between
#' @param label text to be written
#' @param col the color of the line to be drawn
#' @param cex size
#' @param ... passed onto text.default()
#' @export
addText = function(basemap,
                     lon,
                     lat,
                     label,
                     col = 'black',
                     cex = 1,
                     ...) {
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  ## Plot
  text.default(tmp$x, tmp$y, labels = label, col = col, cex = cex, ...)
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addText',
                                                        arguments = list(
                                                          lon = lon, lat = lat, label = label, col = col, cex = cex, ... = ...
                                                        )
  )
  
  invisible(basemap)
}