#' @title Load NetCDF
#' @import ncdf4
#' @param file The location of a .nc file.
#' @param var a list of variable names to be loaded. Default NULL loads all variables.
#' @param test boolean value to turn on testing.
#' @param verbose boolean flag to turn on/off printing to screen.
#' @export
readNC = function(file, var = NULL, test = F, verbose = T) {
  
  if (verbose) { message('Attempting to load data from ', file)}
  file = ncdf4::nc_open(file)
  if (verbose) { message(' File openned.')}
  
  all.var = names(file$var)
  for (name in names(file$dim)) {
    if (file$dim[[name]]$dimvarid$id > 0) {
      all.var = c(all.var, name)
    }
  }
  all.var = all.var[all.var != 'nv']
  
  
  if (is.null(var)) {
    var = all.var
    if (verbose) { message(' No variables specified, loading all ', length(var), ' entires.') }
  }
  
  ## Allow number of variable to be used as well
  if (is.numeric(var)) {
    var = names(file$var)[var]
  }
  
  ## Load data
  data = list()
  
  for (v in var) {
    
    if (v %in% all.var) {
      a = Sys.time()
      data[[v]] = ncdf4::ncvar_get(nc = file, varid = v)
      if (test) { data[[v]] = data[[v]][1] }
      if (verbose) {
        d = dim(data[[v]])
        if (is.null(d)) { d = length(data[[v]])}
        message(' Variable ', v, ' (', paste(d, collapse = 'x'),') loaded from file. \t', round(difftime(Sys.time(), a, units = 'secs')), 's')
      }
    } else {
      a = Sys.time()
      data[[v]] = NA
      if (verbose) {
        message(' Variable ', v, ' (X) does not exist in file. \t', round(difftime(Sys.time(), a, units = 'secs')), 's')
      }
    }
  }
  
  if (verbose) {message(' Closing file. Finished.')}
  ncdf4::nc_close(file)
  
  ##return
  data
}