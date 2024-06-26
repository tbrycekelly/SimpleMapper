#' Standardize longitude -180 - 180
#' @param lon longitude values to be standardize into [-180, +180]
#' @export
standardize.longitude = function(lon) {
  k = !is.na(lon) & lon > 180
  lon[k] = lon[k] - 360
  
  k = !is.na(lon) & lon < -180
  lon[k] = lon[k] + 360
  
  lon
}
