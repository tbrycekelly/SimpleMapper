#' Add reference radius to a map
#' @param lon longitude 
#' @param lat latitude 
#' @param radius radius of circle in km
#' @export
addRadius = function(basemap, lon, lat, radius, ...) {
  
  # Get center in equal area projection
  a = seq(0, pi, length.out = ceiling(100 * radius / basemap$scale) + 10)
  x = radius * sin(a)
  y = radius * cos(a)
  
  projPoints = projectionEqualarea(lon = c(x, -rev(x)),
                                       lat = c(y, rev(y)),
                                       lon0 = 0,
                                       lat0 = 0,
                                       inv = T)
  
  addLine(basemap, projPoints$longitude + lon, projPoints$latitude + lat, ...)
  
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addRadius',
                                                        arguments = list(
                                                          lon = lon, lat = lat, radius = radius, ... = ...
                                                        )
  )
  
  invisible(basemap)
}
