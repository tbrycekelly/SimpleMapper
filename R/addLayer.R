
#' @title Add Map Layer
#' @description  Add a image layer to the map!
#' @author Thomas Bryce Kelly
#' @export
addLayer = function(basemap,
                         lon,
                         lat,
                         z,
                         zlim = NULL,
                         ztrim = F,
                         pal = greyscale(255),
                         trim = NULL,
                         refine = 0,
                         verbose = T) {
  
  # TODO Orient lon based on center of map to naturally deal with antimeridian situations.
  ## Misc corrections
  lon = as.array(lon)
  lat = as.array(lat)
  z = as.array(z)
  
  if (length(dim(lon)) < 2) {
    if (verbose) { message(' Establishing Grid...', appendLF = F) }
    
    if (length(z) == 1.0 * length(lon) * length(lat)) {
      z = array(z, dim = c(length(lon), length(lat)))
      lon = matrix(lon, nrow = dim(z)[1], ncol = dim(z)[2])
      lat = matrix(lat, nrow = dim(z)[1], ncol = dim(z)[2], byrow = T)
    } else { ## TODO add check here that the dimentions are actually correct!
      z = array(z, dim = c(length(unique(lon)), length(unique(lat))))
      lon = matrix(unique(lon), nrow = dim(z)[1], ncol = dim(z)[2])
      lat = matrix(unique(lat), nrow = dim(z)[1], ncol = dim(z)[2], byrow = T)
    }
    if (verbose) { message(' Done. ') }
  }
  
  nz = length(z)
  
  ## Trim
   if (trim & length(z) > 100) {
     
     if (verbose) { message(' Starting domain trimming... ', appendLF = F)}
     usr = par('usr')
     
     corners = expand.grid(lon = c(usr[1], usr[2]),
                           lat = c(usr[3], usr[4]))
     corners = basemap$projection(corners$lon, corners$lat, lon0 = basemap$lon, lat0 = basemap$lat, inv = T)
     
     
     #if (corners[1,1] > corners[2,1]) { ## antimeridian
    #   if (verbose) { message(' antimeridian... ', appendLF = F)}
    #   antimeridian = T
    # } else {
    #   antimeridian = F
    #   lon = lon %% 360
    #   lon[lon > 180] = lon[lon > 180] - 360
    # }
     
     field = expand.grid(lon = seq(usr[1], usr[2], length.out = 100),
                         lat = seq(usr[3], usr[4], length.out = 100))
     field = basemap$projection(field$lon, field$lat, lon0 = basemap$lon, lat0 = basemap$lat, inv = T)
     
     #if (!antimeridian & any(field[,1] > 180)) {
    #   l = which(!is.na(field[,1]) & field[,1] > 180)
    #   field[l,1] = field[l,1] - 360
    # }
     
     field.lon = range(field$longitude - basemap$lon, na.rm = T)
     field.lat = range(field$latitude - basemap$lat, na.rm = T) 
     
     ## Trim longitude
     #if (field.lat[1] > -80 & field.lat[2] < 80) { ## only if a pole isn't visible!
       if (verbose) { message(' longitude... ', appendLF = F)}
       k = apply(lon - basemap$lon, 1, function(x) {any(x >= field.lon[1] & x <= field.lon[2])})
       
       if (sum(k) > 2) {
         z = z[k,]
         lon = lon[k,]
         lat = lat[k,]
       }
     #}
     
     ## Trim latitude
     if (verbose) { message(' latitude... ', appendLF = F) }
     k = apply(lat - basemap$lat, 2, function(x) {any(x >= field.lat[1] & x <= field.lat[2])})
     if (sum(k) > 2) {
       z = z[,k]
       lon = lon[,k]
       lat = lat[,k]
     }
     if (verbose) { message(' complete, n = ', length(z), ' (', 100 - round(100 * length(z) / nz), '% trimmed)')}
   }
  
  ## Subsample
  if (refine < 0) {
    for (i in 1:abs(refine)) {
      temp = gridSubsample(lon, lat, z)
      lon = temp$x
      lat = temp$y
      z = temp$z
    }
    if (verbose) {message(' Subsampled grid by 1:', 2^-refine,'x.')}
  }
  
  
  ## Project and Plot
  if (verbose) {message(' Projecting grid...')}
  
  xy = basemap$projection(lon = as.numeric(lon), lat = as.numeric(lat), lon0 = basemap$lon, lat0 = basemap$lat)
  xy = list(x = array(xy$x, dim = dim(lon)),
            y = array(xy$y, dim = dim(lon)))
  
  ## Refinement
  if (refine > 0) {
    for (i in 1:refine) {
      temp = gridRefinement(xy$x, xy$y, z)
      xy$x = temp$x
      xy$y = temp$y
      z = temp$z
    }
    if (verbose) {message(' Refined grid by ', 2^refine,'x.')}
  } 
  
  xy = calc.vertex(x = xy$x, y = xy$y)
  
  ## Color scale
  if (is.null(zlim)) { zlim = range(pretty(as.numeric(z), na.rm = TRUE)) }
  if (is.null(ztrim)) { ztrim = zlim }
  z[z < zlim[1]] = ztrim[1]
  z[z > zlim[2]] = ztrim[2]
  
  col = (z - zlim[1]) / (zlim[2] - zlim[1]) * (length(pal) - 1) + 1
  col = pal[round(col)]
  col = array(col, dim = dim(lon))
  
  
  
  if (verbose) { message(' Starting plotting... ', appendLF = F) }
  for (i in 1:dim(col)[1]) {
    for (j in 1:dim(col)[2]) {
      if (!is.na(col[i,j])) {
        polygon(x = c(xy$x[i,j], xy$x[i,j+1], xy$x[i+1,j+1], xy$x[i+1,j]),
                y = c(xy$y[i,j], xy$y[i,j+1], xy$y[i+1,j+1], xy$y[i+1,j]),
                col = col[i,j], border = NA)
      }
    }
  }
  if (verbose) { message(' complete.') }
  
  ## Extras
  if (verbose) { message(' Final stats: \tN.low: ', sum(z < zlim[1]), '\tN.high: ', sum(z > zlim[2]), '\tN: ', length(z)) }

  basemap
}
