
#' @export
projectionOrthographic = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  
  if (inv) {
    ## Rescale
    x = lon / R
    y = lat / R
    lat0 = lat0 / 180 * pi
    
    rho = sqrt(x^2 + y^2)
    c = asin(rho)
    
    lon = atan(x * sin(c) / (rho * cos(lat0) * cos(c) - y * sin(lat0) * sin(c)))
    lat = asin(cos(c) * sin(lat0) + y * sin(c) * cos(lat0) / rho)
    
    return(data.frame(longitude = lon * 180 / pi + lon0,
                      latitude = lat * 180 / pi))
  }
  
  ## Remove antimeridian points 
  # TODO Note sure if this works
  k = which(abs(lon - lon0 %% 360) > 90 & abs(lat - lat0) > 90)
  lon[k] = NA
  lat[k] = NA
  
  
  lon = lon / 180 * pi
  lat = lat / 180 * pi
  lon0 = lon0 / 180 * pi
  lat0 = lat0 / 180 * pi
  
  x = cos(lat) * sin(lon - lon0)
  y = cos(lat0) * sin(lat) - sin(lat0) * cos(lat) * cos(lon - lon0)
  
  data.frame(x = x * R, y = y * R)
}
