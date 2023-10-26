#' @title Get Bathymetry
#' @description Get the bathymetry you want from NOAA (marmap)
#' @param map a map object as returned by make.map()
#' @param res the resolution of the bathymetery to be requested. Value is in arc minutes. Smaller values will take longer to download and to plot.
#' @param override a flag to override the default longitude bounds used in order to facilitate high latitude map making.
#' @param keep a flag used to determine if the bathymetry data should be saved to enable offline reuse later.
#' @import marmap
#' @author Laura Whitmore
#' @author Thomas Bryce Kelly
#' @import marmap
#' @export
getBathymetry = function(map, res = 60) {
  
  ## get spatial extent
  usr = par()$usr
  N = 1e3
  grid = data.frame(x = c(seq(usr[1], usr[2], length.out = N),
                          rep(usr[2], N),
                          seq(usr[2], usr[1], length.out = N),
                          rep(usr[1], N)),
                    y = c(rep(usr[3], N),
                          seq(usr[3], usr[4], length.out = N),
                          rep(usr[4], N),
                          seq(usr[4], usr[3], length.out = N)))
  grid = map$projection(grid$x, grid$y, map$lon, map$lat, inv = T)
  
  pole = map$projection(lon = c(0,0), lat = c(-90, 90), map$lon, map$lat) # S/N
  pole$x = pole$x >= usr[1] & pole$y <= usr[2]
  pole$y = pole$y >= usr[3] & pole$y <= usr[4]
  pole = pole$x & pole$y#S/N
  
  if (!is.na(pole[1]) & pole[1]) {
    lat.lim = c(-90, max(grid$latitude, na.rm = T))
  } else if (!is.na(pole[2]) & pole[2]) {
    lat.lim = c(min(grid$latitude, na.rm = T), 90)
  } else {
    lat.lim = range(grid$latitude, na.rm = T)
  }
  lon.lim = range(grid$longitude, na.rm = T)
  lon.lim = lon.lim + c(-0.05, 0.05) * diff(lon.lim) # Add 5% buffer
  
  ##Load Bathymetry (getNOAA.bathy is marmaps)
  bathy = marmap::getNOAA.bathy(lon1 = lon.lim[1], lon2 = lon.lim[2],
                        lat1 = lat.lim[1], lat2 = lat.lim[2],
                        resolution = res, keep = F)
  
  ##Convert bathy
  bathyLon = as.numeric(rownames(bathy))
  bathyLat = as.numeric(colnames(bathy))
  bathyZ = as.numeric(bathy)
  dim(bathyZ) = dim(bathy)
  
  
  ## return
  list(lon = bathyLon, lat = bathyLat, z = bathyZ, res = res, diagnostics = list(lon = lon.lim, lat = lat.lim, pole = pole))
}
