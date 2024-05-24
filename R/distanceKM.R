#' Calculate Distance between Points (kilometers)
#' @param lon1 longitude of first point
#' @param lon2 longitude of second point
#' @param lat1 latitude of first point
#' @param lat2 latitude of second point
#' @export
distanceKM = function(lon1, lat1, lon2, lat2) {
  R = 6.371e3 # km
  lon1 = lon1 * pi / 180
  lon2 = lon2 * pi / 180
  lat1 = lat1 * pi / 180
  lat2 = lat2 * pi / 180
  
  
  R * acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2))
}
