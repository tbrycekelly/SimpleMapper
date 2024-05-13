
#' @title Add Coastline to basemap
#' @author Thomas Bryce Kelly
#' @description Adds a coastline list to a map
#' @param basemap a list generated from plotBasemap()
#' @param coastline a specific list of coastline longitude/latitude to be used for plotting. If NULL (default) then will use the coastline specified in basemap.
#' @param land.col a color value used to fill the coastline polygons
#' @param lons an optional limit to the valid longitude values that will be used. Useful to eliminate some artefacts.
#' @param lats an optional limit to the valid latitude values that will be used. Useful to eliminate some artefacts.
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