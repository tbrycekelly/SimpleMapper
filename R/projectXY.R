#' XY Projection (beta)
#' @param lon longitude values to project (or x values when inverting)
#' @param lat latitude values to project (or y values when inverting)
#' @param lon0 center longitude value of the projection
#' @param lat0 center latitude value of the projection
#' @param inv a boolean flag to turn on/off projection inversion
#' @export
projectionXY = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  
  if (inv) {
    x = lon * 180 / pi / R + lon0
    y = lat * 180 / pi / R + lat0
    
    lon = standardize.longitude(x)
    return(data.frame(longitude = lon, latitude = y))
  }
  
  
  #lon = (lon - lon0 + 180) %% 360 - 180
  lon = (lon - lon0) / 180 * pi
  lat = (lat - lat0) / 180 * pi
  
  x = lon 
  y = lat 
  
  data.frame(x = x * R, y = y * R)
}
