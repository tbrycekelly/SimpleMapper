#' Stereographic Projection
#' @param lon longitude values to project (or x values when inverting)
#' @param lat latitude values to project (or y values when inverting)
#' @param lon0 center longitude value of the projection
#' @param lat0 center latitude value of the projection
#' @param inv a boolean flag to turn on/off projection inversion
#' @export
projectionStereographic = function(lon, lat, lon0 = 0, lat0 = 0, inv = F) {
  R = 6.371e3
  
  if (inv) {
    ## Rescale
    x = lon / R
    y = lat / R
    lat0 = lat0 / 180 * pi
    
    rho = sqrt(x^2 + y^2)
    c = 2 * atan(0.5 * rho)
    
    lon = atan2(x * sin(c),  (rho * cos(lat0) * cos(c) - y * sin(lat0) * sin(c)))
    lat = asin(cos(c) * sin(lat0) + y * sin(c) * cos(lat0) / rho)
    
    lat = lat * 180 / pi
    lon = lon * 180 / pi + lon0
    lon = standardize.longitude(lon)
    
    return(data.frame(longitude = lon,
                      latitude = lat))
  }
  
  lon = (lon - lon0) / 180 * pi
  lat = lat / 180 * pi
  lat0 = lat0 / 180 * pi
  
  k = 2 / (1+sin(lat0)*sin(lat) + cos(lat0)*cos(lat)*cos(lon))
  
  x = k * cos(lat) * sin(lon)
  y = k * (cos(lat0) * sin(lat) - sin(lat0) * cos(lat) * cos(lon))
  
  ## Remove points within certain distance of antipod: 
  # Assuming FOV is 160 degrees: +- 80 degrees 
  #k = which((lon < -80/180*pi | lon > 80/180*pi) & (lat < -80/180*pi | lat > 80/180*pi))
  #x[k] = NA
  #y[k] = NA
  
  data.frame(x = x * R, y = y * R)
}
