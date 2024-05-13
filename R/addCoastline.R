
#' @title Add Coastline to basemap
#' @author Thomas Bryce Kelly
#' @description Adds a coastline list to a map
#' @param basemap a list generated from plotBasemap()
#' @param coastline a specific list of coastline longitude/latitude to be used for plotting. If NULL (default) then will use the coastline specified in basemap.
#' @param land.col a color value used to fill the coastline polygons
#' @param lons an optional limit to the valid longitude values that will be used. Useful to eliminate some artefacts.
#' @param lats an optional limit to the valid latitude values that will be used. Useful to eliminate some artefacts.
#' @export
addCoastline = function(basemap, coastline = NULL, land.col = NA, lons = c(-180, 180), lats = c(-90, 90)) {
  
  if (is.null(coastline)) {
    coastline = basemap$coastline
  }
  
  # Load data if necessary
  if (typeof(coastline) == "character") {
    if (!coastline %in% ls()) {
      do.call('data', list(coastline))
    }
    coastline = eval(parse(text = coastline))
  }
  
  ## Coarse trim
  field = fieldOfView(basemap, n = 100) #lon0 centered!
  
  keep = rep(T, length(coastline$data))
  for (i in 1:length(coastline$data)) {
    keep[i] = !all(coastline$data[[i]]$longitude - basemap$lon < field$lon[1] | coastline$data[[i]]$longitude - basemap$lon > field$lon[2]) | !all(coastline$data[[i]]$latitude < field$lat[1] | coastline$data[[i]]$latitude > field$lat[2])
  }
  if (sum(keep) > 1) {
    coastline$data = coastline$data[which(keep)]
  } else {
    message('No coasts selected')
  }
  
  ## Project coastline (takes a while!)
  projected.coast = lapply(coastline$data,
                           function(x) {
                             x$longitude = pmin(pmax(x$longitude, lons[1]), lons[2])
                             x$latitude = pmin(pmax(x$latitude, lats[1]), lats[2])
                             
                             basemap$projection(lon = x$longitude,
                                        lat = x$latitude,
                                        lon0 = basemap$lon,
                                        lat0 = basemap$lat)
  })
  
  projected.coast = break.polygon(basemap, projected.coast)
  
  ## Add coastlines (takes a while!)
  for (i in 1:length(projected.coast)) {
    polygon(projected.coast[[i]]$x, projected.coast[[i]]$y, col = land.col)
  }
  
  basemap
}
