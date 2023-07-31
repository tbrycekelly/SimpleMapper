library(SimpleMapper)

basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60)


basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60, projection = projectionEqualarea)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionEqualarea)


basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = mapAxis(basemap, lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10))

basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic) %>%
  mapAxis(lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10)) %>%
  mapScale() %>%
  mapPoints(lon = -150, lat = 55)


basemap = plotBasemap(coastline = 'coastline3', scale = 2000, lon = -150, lat = 90, projection = projectionOrthographic)
basemap = mapAxis(basemap, lons = c(170, seq(-180, 170, by = 10)), lat = seq(70, 90, by = 5), label.sides = NA, col = '#00000020')

#par(new = T, plt = c(0.6, 0.9, 0.1, 0.4))
basemap = plotBasemap(coastline = 'coastline3', scale = 10000, lon = -150, lat = 90, projection = projectionOrthographic, frame = F)
basemap = mapAxis(basemap, lons = c(170, seq(-180, 170, by = 30)), lat = seq(0, 90, by = 20), label.sides = NA)

basemap = plotBasemap(coastline = 'coastline1', scale = 20000, lon = 0, lat = 0, projection = projectionMercator)
basemap = plotBasemap(coastline = 'coastline1', scale = 20000, lon = -150, lat = 0, projection = projectionMercator)
basemap = mapAxis(basemap, lons = c(170, seq(-180, 170, by = 10)), lat = seq(70, 90, by = 5), label.sides = NA)

basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = mapAxis(basemap, lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10))
basemap = addLayer(basemap, lon = c(-150:-140), lat = c(50:60), z = array(runif(121), dim = c(11,11)))
basemap = mapScale(basemap)


