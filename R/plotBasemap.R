

#' @title Make Map
#' @param coast Should be the name of a coastline data object. A value of NULL sets the default cosatline to 'coastline2'.
#' @param land.col A color string or value for the land polygon
#' @author Laura Whitmore
#' @author Thomas Bryce Kelly
#' @export
plotBasemap = function (lon = 0,
                      lat = 0,
                      scale = 1000,
                      coastline = NULL,
                      projection = NULL,
                      land.col = 'lightgray',
                      frame = T,
                      verbose = T) {
  
  lon = standardize.longitude(lon)
  
  ## Apply Defaults
  if (is.null(coastline)) { 
    if (scale < 1000) {
      coastline = coastline4
    } else if (scale < 3000) {
      coastline = coastline3
    } else if (scale < 10000) {
      coastline = coastline2
    } else {
      coastline = coastline1
    }
  }
  
  if (is.null(projection)) {
    projection = projectionEqualarea
  }
  
  aspect.ratio = par()$pin[1] / par()$pin[2] # ratio of width:height of the plotting area
  
  ## make plot
  if (frame) {
    plot(NULL,
         NULL,
         xlim = c(-scale, scale),
         ylim = c(-scale, scale)/aspect.ratio,
         xaxt = 'n', yaxt = 'n',
         xlab = '', ylab = '',
         xaxs = 'i', yaxs = 'i')
  } else {
    plot(NULL,
         NULL,
         bty = 'n',
         xlim = c(-scale, scale),
         ylim = c(-scale, scale)/aspect.ratio,
         xaxt = 'n', yaxt = 'n',
         xlab = '', ylab = '',
         xaxs = 'i', yaxs = 'i')
  }
  
  ## Setup map object
  basemap = list(coastline = coastline,
             lon = lon,
             lat = lat,
             scale = scale,
             aspect.ratio = aspect.ratio,
             projection = projection,
             land.col = land.col)
  
  coast = addCoastline(
    basemap = basemap,
    coastline = coastline,
    land.col = land.col)
  
  if (frame) { box()}
  
  basemap
}
