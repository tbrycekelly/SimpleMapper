#' @title Add EEZ to map
#' @description  Add the EEZ boundary lines to a map
#' @param basemap a list object such as from plotBasemap()
#' @param eez a list of eez coordinates such as that provided herein as eez5.
#' @param verbose a boolean flag to turn on/off displayed messages
#' @author Thomas Bryce Kelly
#' @export
addEEZ = function(basemap, eez = NULL, pattern = '*', ..., verbose = T) { #TODO Only plot lines that are within the viewing window.
  k = grep(pattern, names(eez))
  
  for (i in 1:length(k)) {
    if (verbose) {
      message(' (', i,'/', length(k),') Adding EEZ line ', names(eez)[k[i]], ' to map...')
    }
    for (j in 1:length(eez[[k[i]]])) {
      basemap = addLine(basemap, lon = eez[[k[i]]][[j]]$lon, lat = eez[[k[i]]][[j]]$lat, ...)
    }
  }
  
  basemap
}
