#' @title Add EEZ to map
#' @description  Add the EEZ boundary lines to a map
#' @param basemap a list object such as from plotBasemap()
#' @param eez a list of eez coordinates such as that provided herein as eez2.
#' @param pattern a string to search against the names contained within the eez data list.
#' @param verbose a boolean flag to turn on/off displayed messages
#' @param ... optional arguments passed to addLine
#' @author Thomas Bryce Kelly
#' @import graphics
#' @export
addEEZ = function(basemap, eez = NULL, pattern = '*', ..., verbose = T) { #TODO Only plot lines that are within the viewing window.
  if (is.null(eez)) {
    eez = SimpleMapper::eez1
  }
  
  k = grep(pattern, names(eez))
  
  for (i in 1:length(k)) {
    if (verbose) {
      message(' (', i,'/', length(k),') Adding EEZ line ', names(eez)[k[i]], ' to map...')
    }
    for (j in 1:length(eez[[k[i]]])) {
      addLine(basemap, lon = eez[[k[i]]][[j]]$lon, lat = eez[[k[i]]][[j]]$lat, ...)
    }
  }
  
  basemap
}
