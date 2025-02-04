#' Mercator Projection (beta)
#' @param lon longitude values to project (or x values when inverting)
#' @param lat latitude values to project (or y values when inverting)
#' @param lon0 center longitude value of the projection
#' @param lat0 center latitude value of the projection
#' @param inv a boolean flag to turn on/off projection inversion
#' @export
projectionMercator = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  #lon0 = 0
  #lat0 = 0
  lon = lon * 0.999
  lat = lat * 0.999
  
  
  if (inv) {
    x = lon * 180 / pi / R + lon0
    y = 2 * atan(exp(lat / R)) - 0.5 * pi
    lat = y * 180 / pi + lat0
    
    lon = standardize.longitude(x)
    return(data.frame(longitude = lon, latitude = y))
  }
  
  
  #lon = (lon - lon0 + 180) %% 360 - 180
  lon = (lon / 180 * pi)
  lat = (lat - lat0) / 180 * pi
  lon0 = lon0 / 180 * pi
  lat0 = lat0 / 180 * pi
  
  x = lon
  y = log(tan(pi * 0.25 + 0.5 * lat))
  #y = 0.5 * log( (1+sin(lat)) / (1 - sin(lat)) ) ## 0.5x
  
  
  data.frame(x = x * R, y = y * R)
}
