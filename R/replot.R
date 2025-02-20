#' Replot a previous map
#' @description A simple function that allows multiple plotting commands to be replayed from a previously rendered map.
#' @param basemap a list structure generated via plotBasemap()
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
    do.call(eval(basemap$history[[i]]$func), args = c(basemap = substitute(basemap), basemap$history[[i]]$arguments))
  }
}