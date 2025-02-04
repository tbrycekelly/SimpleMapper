#' Orthographic Projection
#' @param lon longitude values to project (or x values when inverting)
#' @param lat latitude values to project (or y values when inverting)
#' @param lon0 center longitude value of the projection
#' @param lat0 center latitude value of the projection
#' @param inv a boolean flag to turn on/off projection inversion
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
    
    lon = atan2(x * sin(c), (rho * cos(lat0) * cos(c) - y * sin(lat0) * sin(c)))
    lat = asin(cos(c) * sin(lat0) + y * sin(c) * cos(lat0) / rho)
    
    lon = lon * 180 / pi + lon0
    lon = standardize.longitude(lon)
    
    return(data.frame(longitude = lon,
                      latitude = lat * 180 / pi))
  }
  
  ## Remove antimeridian points 
  # TODO Note sure if this works
  lon = lon - lon0
  #k = which(abs(lon %% 360) > 90 & abs(lat - lat0) > 90)
  #lon[k] = 90
  #lat[k] = NA
  
  lon = lon / 180 * pi
  lat = lat / 180 * pi
  lon0 = lon0 / 180 * pi
  lat0 = lat0 / 180 * pi
  
  x = cos(lat) * sin(lon - lon0)
  y = cos(lat0) * sin(lat) - sin(lat0) * cos(lat) * cos(lon - lon0)
  
  data.frame(x = x * R, y = y * R)
}
