break.polygon = function(basemap, projected.coast) {
  # TODO Should rewrite this to look for polygons that are in all four quadrants AND are always outside the screen.
  # Then only have to deal with one edge case where all you see is land, accurately. 
  
  i = 1
  while (i < length(projected.coast)) {
    dx = diff(sign(projected.coast[[i]]$x))
    dy = diff(sign(projected.coast[[i]]$y))
    
    k = which((abs(projected.coast[[i]]$x[-1]) > basemap$scale/10 & dx != 0) | (abs(projected.coast[[i]]$y[-1]) > basemap$scale/10 & dy != 0))
    k = c(k, length(projected.coast[[i]]$x)) ## Add end.
    
    if (length(k) > 1) {
      break
      for (j in 2:length(k)) {
        
        ## split off parts of polygons with more than 5 points
        projected.coast[[length(projected.coast) + 1]] = projected.coast[[i]][(k[j-1]+1):k[j],]
      }
      ## Trim original down to just first likely contiguous area
      projected.coast[[i]] = projected.coast[[i]][1:k[1],]
    }
    i = i + 1
  }
  ## Return
  projected.coast
}
