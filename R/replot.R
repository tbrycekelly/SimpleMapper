#' @export
replot = function(basemap) {
  
  plotBasemap(lon = basemap$lon,
              lat = basemap$lat,
              scale = basemap$scale,
              coastline = basemap$coastline,
              projection = basemap$projection,
              land.col = basemap$land.col,
              frame = basemap$frame
  )
  
  for (i in 1:length(basemap$history)) {
    do.call(eval(map$history[[i]]$func), args = c(basemap = substitute(map), map$history[[i]]$arguments))
  }
}