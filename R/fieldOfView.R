
#' @title Add Coastline to basemap
#' @author Thomas Bryce Kelly
#' @description Adds a coastline list to a map
#' @param basemap a list generated from plotBasemap()
#' @param n an integer used to set the resolution of the FOV grid.
#' @export
fieldOfView = function(basemap, n = 10) {
  usr = par('usr')
  
  field = expand.grid(lon = seq(usr[1], usr[2], length.out = n),
                      lat = seq(usr[3], usr[4], length.out = n))
  field = basemap$projection(field$lon, field$lat, lon0 = 0, lat0 = basemap$lat, inv = T) # lon = 0 in center of screen
  
  field.lon = range(field$longitude, na.rm = T)
  field.lat = range(field$latitude, na.rm = T) 
  
  list(lon = field.lon,
       lon0 = basemap$lon,
       lat = field.lat,
       field = field)
}