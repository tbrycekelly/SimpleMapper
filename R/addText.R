#' @title Add Map Text
#' @author Laura Whitmore
#' @export
addText = function(basemap,
                     lon,
                     lat,
                     label,
                     col = 'black',
                     pch = 1,
                     cex = 1,
                     ...) {
  
  tmp = basemap$projection(lon = lon,
                           lat = lat,
                           lon0 = basemap$lon,
                           lat0 = basemap$lat)
  
  ## Plot
  text.default(tmp$x, tmp$y, labels = label, col = col, pch = pch, cex = cex, ...)
  
  return(basemap)
}