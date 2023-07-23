#' @title Add Map Points
#' @author Laura Whitmore
#' @description Add station points to a map
#' @param lon the longitudes of the points to be drawn
#' @param lat the latitude of the points to be drawn
#' @param stn.lon (depreciated) the longitudes of the points to be drawn
#' @param stn.lat (depreciated) the latitude of the points to be drawn
#' @param col the color of the points
#' @param cex the size of point to be drawn
#' @param pch the point character to be used
#' @param ... optional arguments passed to points().
#' @import rgdal
#' @export
mapPoints = function(basemap,
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