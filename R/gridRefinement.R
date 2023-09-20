#' @title Calcuye extended grid
#' @author Thomas Bryce Kelly
gridRefinement = function(x = NULL, y = NULL, z) {
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
  vertex.x = array(0, dim = c(2*dim(x)[1]-1, 2*dim(x)[2]-1))
  vertex.y = vertex.x
  vertex.z = vertex.x
  
  ## fill in known values
  for (i in 1:dim(x)[1]) {
    for (j in 1:dim(x)[2]) {
      vertex.x[2*i-1, 2*j-1] = x[i,j]
      vertex.y[2*i-1, 2*j-1] = y[i,j]
      vertex.z[2*i-1, 2*j-1] = z[i,j]
    }
  }
  
  ## Interpolate x
  for (i in 1:(dim(x)[1]-1)) {
    for (j in 1:dim(x)[2]) {
      vertex.x[2*i, 2*j-1] = 0.5 * (x[i,j] + x[i+1,j])
      vertex.y[2*i, 2*j-1] = 0.5 * (y[i,j] + y[i+1,j])
      vertex.z[2*i, 2*j-1] = 0.5 * (z[i,j] + z[i+1,j])
    }
  }
  
  ## Interpolate y
  for (i in 1:dim(x)[1]) {
    for (j in 1:(dim(x)[2]-1)) {
      vertex.x[2*i-1, 2*j] = 0.5 * (x[i,j] + x[i,j+1])
      vertex.y[2*i-1, 2*j] = 0.5 * (y[i,j] + y[i,j+1])
      vertex.z[2*i-1, 2*j] = 0.5 * (z[i,j] + z[i,j+1])
    }
  }
  
  ## corners
  for (i in 1:(dim(x)[1]-1)) {
    for (j in 1:(dim(x)[2]-1)) {
      vertex.x[2*i, 2*j] = 0.25 * (x[i,j] + x[i,j+1] + x[i+1,j] + x[i+1,j+1])
      vertex.y[2*i, 2*j] = 0.25 * (y[i,j] + y[i,j+1] + y[i+1,j] + y[i+1,j+1])
      vertex.z[2*i, 2*j] = 0.25 * (z[i,j] + z[i,j+1] + z[i+1,j] + z[i+1,j+1])
    }
  }
  
  list(x = vertex.x, y = vertex.y, z = vertex.z)
}


