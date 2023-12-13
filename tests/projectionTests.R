library(SimpleMapper)

results = list()

{ ## Test Orthographic
  center = c(23, -10)
  points = data.frame(lon = runif(100, min = center[1] - 45, max = center[1] + 45),
                      lat = runif(100, min = center[2] - 45, max = center[2] + 45))
  
  tmp = projectionOrthographic(lon = points$lon,
                               lat = points$lat,
                               lon0 = center[1],
                               lat0 = center[2],
                               inv = F)
  tmp = projectionOrthographic(tmp$x,
                               tmp$y,
                               lon0 = center[1],
                               lat0 = center[2],
                               inv = T)
  
  results$projectionOrthographic = all(sqrt((tmp$longitude - points$lon)^2 + (tmp$latitude - points$lat)^2) < 0.001)
}



{ ## Test EqualArea
  center = c(23, -10)
  points = data.frame(lon = runif(100, min = center[1] - 45, max = center[1] + 45),
                      lat = runif(100, min = center[2] - 45, max = center[2] + 45))
  
  tmp = projectionEqualarea(lon = points$lon,
                               lat = points$lat,
                               lon0 = center[1],
                               lat0 = center[2],
                               inv = F)
  tmp = projectionEqualarea(tmp$x,
                               tmp$y,
                               lon0 = center[1],
                               lat0 = center[2],
                               inv = T)
  
  results$projectionEqualarea = all(sqrt((tmp$longitude - points$lon)^2 + (tmp$latitude - points$lat)^2) < 0.001)
}


{ ## Test Mercator
  center = c(23, -10)
  points = data.frame(lon = runif(100, min = center[1] - 45, max = center[1] + 45),
                      lat = runif(100, min = center[2] - 45, max = center[2] + 45))
  
  tmp = projectionMercator(lon = points$lon,
                            lat = points$lat,
                            lon0 = center[1],
                            lat0 = center[2],
                            inv = F)
  tmp = projectionMercator(tmp$x,
                            tmp$y,
                            lon0 = center[1],
                            lat0 = center[2],
                            inv = T)
  
  results$projectionMercator = all(sqrt((tmp$longitude - points$lon)^2 + (tmp$latitude - points$lat)^2) < 0.001)
}


message('projectionTests results: \n', paste(names(results), results, collapse = '\n', sep = '\t'))

return(T)
