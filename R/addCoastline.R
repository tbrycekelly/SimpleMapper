
#' @title Add Coastline to basemap
#' @author Thomas Bryce Kelly
#' @description Adds a coastline list to a map
#' @export
addCoastline = function(basemap, coastline, land.col) {
  
  # Load data if necessary
  if (typeof(coastline) == "character") {
    if (!coastline %in% ls()) {
      do.call('data', list(coastline))
    }
    coastline = eval(parse(text = coastline))
  }
  
  
  
  ## Coarse trim
  field = expand.grid(x = seq(-1, 1, length.out = 25),
                      y = seq(-basemap$aspect.ratio, basemap$aspect.ratio, length.out = 25)) * basemap$scale
  
  field = basemap$projection(field$x, field$y, lon0 = basemap$lon, lat0 = basemap$lat, inv = T)
  
  field.x = range(field$longitude, na.rm = T)
  field.y = range(field$latitude, na.rm = T)
  
  keep = rep(T, length(coastline$data))
  for (i in 1:length(coastline$data)) {
    keep[i] = !all(coastline$data[[i]]$longitude < field.x[1] | coastline$data[[i]]$longitude > field.x[2]) | !all(coastline$data[[i]]$latitude < field.y[1] | coastline$data[[i]]$latitude > field.y[2])
  }
  coastline$data = coastline$data[which(keep)]
  
  
  
  ## Project coastline (takes a while!)
  projected.coast = lapply(coastline$data,
                           function(x) {
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
