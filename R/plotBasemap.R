

#' @title Make Map
#' @param land.col A color string or value for the land polygon
#' @param lon center longitude of map
#' @param lat center latitude of map
#' @param scale the visual scale of the map (in km)
#' @param coastline a coastline data list object (optional)
#' @param land.col color used for the fill of each land polygon
#' @param projection a function used to project (and invert the projection)
#' @param frame a boolean flag to redraw the graphic canvas' frame
#' @param verbose a flag to turn on/off pritning to screen
#' @author Laura Whitmore
#' @author Thomas Bryce Kelly
#' @export
plotBasemap = function (lon = 0,
                      lat = 0,
                      scale = 1000,
                      coastline = NULL,
                      projection = projectionEqualarea,
                      land.col = 'lightgray',
                      frame = T,
                      verbose = T) {
  
  lon = standardize.longitude(lon)
  
  ## Apply Defaults
  if (is.null(coastline)) { 
    if (scale < 1000) {
      coastline = SimpleMapper::coastline4
    } else if (scale < 3000) {
      coastline = SimpleMapper::coastline3
    } else if (scale < 10000) {
      coastline = SimpleMapper::coastline2
    } else {
      coastline = SimpleMapper::coastline1
    }
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
             land.col = land.col,
             frame = frame,
             history = list()
             )
  
  coast = addCoastline(
    basemap = basemap,
    coastline = coastline,
    land.col = land.col)
  
  if (frame) { box()}
  
  invisible(basemap)
}
