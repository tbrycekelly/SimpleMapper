#' @title Map numeric values to colors
#' @author Thomas Bryce Kelly
#' @description Assign values (numeric) to a set of discrete colors linearly.
#' @keywords colormap palette
#' @export
mapColor = function(x, xlim = NULL, pal = greyscale(16), clip = F) {
  if (is.null(xlim)) {
    xlim = range(x, na.rm = T)
  }

  if (!clip) {
    x[x < xlim[1]] = xlim[1]
    x[x > xlim[2]] = xlim[2]
  }

  ## Map to discrete interval
  xmap = (length(pal)-1) * (x - xlim[1]) / (xlim[2] - xlim[1]) + 1 # [1 -> length(pal)] mapping
  xmap[xmap < 1] = NA
  xmap[xmap > length(pal)] = NA
  xmap = round(xmap)

  pal[xmap]
}
