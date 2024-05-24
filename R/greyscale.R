#' @title Greyscale Palette
#' @export
#' @author Thomas Bryce Kelly
#' @param n the number of greyscale colors desired
#' @param rev a boolean flag to reverse the color palette
#' @importFrom grDevices grey.colors
greyscale = function(n, rev = FALSE) {
  grDevices::grey.colors(n, 0, 1, rev = rev)
}
