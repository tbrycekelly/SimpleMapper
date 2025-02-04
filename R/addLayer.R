
#' @title Add Map Layer
#' @description  Add a image layer to the map!
#' @param basemap a list object such as from plotBasemap()
#' @param lon a vector or array of longitude values that match the z grid provided
#' @param lat a vector or array of latitude values that match the z grid provided
#' @param z a grid (matrix or array) to be used for plotting
#' @param zlim the value limits used to match up colors to numerical values. Default is range(z)
#' @param ztrim the values used to substitute for numeric values outside of zlim. E.g. an NA value will skip plotting.
#' @param pal a vector of colors applied against the z grid to generate colors
#' @param trim a boolean value to turn on automatic trimming of the z grid to the visible map. Can cause (obvious) artefacts in some circumstances.
#' @param refine an integer value used to refine (or subsample) the provided grid. Positive values will refine and negative values will subsample.
#' @param verbose a boolean flag to turn on/off displayed messages
#' @author Thomas Bryce Kelly
#' @export
addLayer = function(basemap,
                         lon,
                         lat,
                         z,
                         zlim = NULL,
                         ztrim = NULL,
                         pal = greyscale(255),
                         trim = T,
                         refine = 0,
                         verbose = T) {
  
  basemap$history[[length(basemap$history) + 1]] = list(func = 'addLayer',
                                                        arguments = list(
                                                          lon = lon, lat = lat, z = z, zlim = zlim, ztrim = ztrim, pal = pal, trim = trim, refine = refine, verbose = verbose
                                                        )
  )
  
  # TODO Orient lon based on center of map to naturally deal with antimeridian situations.
  ## Misc corrections
  lon = as.array(lon)
  lon = standardize.longitude(lon - basemap$lon) # NB: relative longitude throughout!
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
     
    field = fieldOfView(basemap, 100)# lon = 0 in center of screen
     
    ## Trim latitude
    if (verbose) { message(' latitude... ', appendLF = F) }
    k = apply(lat, 2, function(x) {any(x >= floor(field$lat[1]) & x <= ceiling(field$lat[2]))})
    if (sum(k) > 2) {
      z = z[,k]
      lon = lon[,k]
      lat = lat[,k]
    }
    if (verbose) { message(' complete, n = ', length(z), ' (', 100 - round(100 * length(z) / nz), '% trimmed)')}
     
    if (verbose) { message(' longitude... ', appendLF = F) }
    k = apply(lon, 1, function(x) {any(x >= floor(field$lon[1]) & x <= ceiling(field$lon[2]))})
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
  
  xy = basemap$projection(lon = as.numeric(lon), lat = as.numeric(lat), lon0 = 0, lat0 = basemap$lat)
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
  if (is.null(zlim)) {
    zlim = range(pretty(as.numeric(z), na.rm = TRUE))
    if (verbose) {
      message(' No zlim specified, automatically picked ', zlim[1], ' ', zlim[2], '.')
    }
  }
  if (is.null(ztrim)) { ztrim = zlim }
  ztrim[1] = pmax(ztrim[1], zlim[1])
  ztrim[2] = pmin(ztrim[2], zlim[2])
  
  z[!is.na(z) & z < zlim[1]] = ztrim[1]
  z[!is.na(z) & z > zlim[2]] = ztrim[2]
  
  col = (z - zlim[1]) / (zlim[2] - zlim[1]) * (length(pal) - 1) + 1
  col = pal[round(col)]
  col = array(col, dim = dim(lon))
  
  ## Would love to find a more efficient way to plot many small rectangles
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
  
  invisible(basemap)
}
