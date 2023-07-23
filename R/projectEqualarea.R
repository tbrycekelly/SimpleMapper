
#' @export
projectionEqualarea = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  
  if (inv) {
    
    x = lon / R
    y = lat / R
    lat0 = lat0 / 180 * pi
    
    rho = sqrt(x^2 + y^2)
    c = 2 * asin(0.5 * rho)
    
    lon = atan(x * sin(c) / (rho * cos(lat0) * cos(c) - y * sin(lat0) * sin(c)))
    lat = asin(cos(c) * sin(lat0) + y * sin(c) * cos(lat0) / rho)
    
    return(data.frame(longitude = lon * 180 / pi + lon0,
                      latitude = lat * 180 / pi))
  }
  
  lon = lon / 180 * 3.1415926535
  lat = lat / 180 * 3.1415926535
  lon0 = lon0 / 180 * 3.1415926535
  lat0 = lat0 / 180 * 3.1415926535
  
  k = sqrt(2 / (1 + sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * cos(lon - lon0)))
  x = k * cos(lat) * sin(lon - lon0)
  y = k * (cos(lat0) * sin(lat) - sin(lat0) * cos(lat) * cos(lon - lon0))
  
  data.frame(x = x * R, y = y * R)
}
