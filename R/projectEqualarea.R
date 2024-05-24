#' Equal Area Projection
#' @param lon longitude values to project (or x values when inverting)
#' @param lat latitude values to project (or y values when inverting)
#' @param lon0 center longitude value of the projection
#' @param lat0 center latitude value of the projection
#' @param inv a boolean flag to turn on/off projection inversion
#' @export
projectionEqualarea = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  
  if (inv) {
    
    x = lon / R
    y = lat / R
    lat0 = lat0 / 180 * pi
    
    rho = sqrt(x^2 + y^2)
    #c = 2 * asin(pmin(0.5 * rho, 1))
    c = 2 * asin(0.5 * rho)
    
    lon = atan2(x * sin(c), (rho * cos(lat0) * cos(c) - y * sin(lat0) * sin(c)))
    lat = asin(cos(c) * sin(lat0) + y * sin(c) * cos(lat0) / rho)
    lon = standardize.longitude(lon * 180 / pi - lon0)
    
    return(data.frame(longitude = lon,
                      latitude = lat * 180 / pi))
  }
  
  lon = lon / 180 * pi
  lat = lat / 180 * pi
  lon0 = lon0 / 180 * pi
  lat0 = lat0 / 180 * pi
  
  k = sqrt(2 / (1 + sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * cos(lon - lon0)))
  x = k * cos(lat) * sin(lon - lon0)
  y = k * (cos(lat0) * sin(lat) - sin(lat0) * cos(lat) * cos(lon - lon0))
  
  data.frame(x = x * R, y = y * R)
}


