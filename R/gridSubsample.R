#' @title Calculate subsampled grid
#' @author Thomas Bryce Kelly
#' @param x the x positions in the grid
#' @param y the y positions in the grid
#' @param z the grid of values to be refined
#' @param approx boolean flag to turn on fast-approximation method
gridSubsample = function(x = NULL, y = NULL, z, approx = F) {
  if (is.null(dim(z))) {stop('grid.refinement: z must be an array object of two dimensions.')}
  dim = dim(z)
  
  if (is.null(x) & is.null(y)) {
    x = c(1:dim[1])
    y = c(1:dim[2])
  }
  
  if (is.null(dim(x)) & is.null(dim(y))) {
    x = array(x, dim = dim)
    y = t(array(y, dim = rev(dim)))
  }
  
  ## Vertex
  vertex.x = array(0, dim = floor(c(dim(x)[1], dim(x)[2]) / 2))
  vertex.y = vertex.x
  vertex.z = vertex.x
  
  if (approx) {
    for (i in 1:dim(vertex.x)[1]) {
      for (j in 1:dim(vertex.x)[2]) {
        vertex.x[i, j] = x[2*i, 2*j]
        vertex.y[i, j] = y[2*i, 2*j]
        vertex.z[i, j] = z[2*i, 2*j]
      }
    }
  } else {
    for (i in 1:dim(vertex.x)[1]) {
      for (j in 1:dim(vertex.x)[2]) {
        vertex.x[i, j] = 0.25 * (x[2*i, 2*j] + x[2*i-1, 2*j] + x[2*i, 2*j-1] + x[2*i-1, 2*j-1])
        vertex.y[i, j] = 0.25 * (y[2*i, 2*j] + y[2*i-1, 2*j] + y[2*i, 2*j-1] + y[2*i-1, 2*j-1])
        vertex.z[i, j] = mean(c(z[2*i, 2*j], z[2*i-1, 2*j], z[2*i, 2*j-1], z[2*i-1, 2*j-1]), na.rm = T)
      }
    }
  }
  
  list(x = vertex.x, y = vertex.y, z = vertex.z)
}
