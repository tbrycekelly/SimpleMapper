library(SimpleMapper)

basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60)


basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60, projection = projectionEqualarea)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionEqualarea)


basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = addAxis(basemap, lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10))

basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = addAxis(basemap, lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10), label.sides = c(1,2,4))
basemap = addScale(basemap)
basemap = addPoints(basemap, lon = -150, lat = 55)


basemap = plotBasemap(coastline = 'coastline3', scale = 2000, lon = -150, lat = 90, projection = projectionOrthographic)
basemap = addAxis(basemap, lons = c(170, seq(-180, 170, by = 10)), lat = seq(70, 90, by = 5), label.sides = NA, col = '#00000020')

#par(new = T, plt = c(0.6, 0.9, 0.1, 0.4))
basemap = plotBasemap(coastline = 'coastline3', scale = 10000, lon = -150, lat = 90, projection = projectionOrthographic, frame = F)
basemap = addAxis(basemap, lons = c(170, seq(-180, 170, by = 30)), lat = seq(0, 90, by = 20), label.sides = NA)

basemap = plotBasemap(coastline = 'coastline1', scale = 20000, lon = 0, lat = 0, projection = projectionMercator)
basemap = plotBasemap(coastline = 'coastline1', scale = 20000, lon = -150, lat = 0, projection = projectionMercator)
basemap = addAxis(basemap, lons = c(170, seq(-180, 170, by = 10)), lat = seq(70, 90, by = 5), label.sides = NA)

basemap = plotBasemap(coastline = 'coastline2', scale = 2000, lon = -150, lat = 60, projection = projectionOrthographic)
basemap = addAxis(basemap, lons = c(170, seq(-180, -120, by = 10)), lat = seq(40, 90, by = 10))

basemap = addLayer(basemap, lon = c(-150:-140), lat = c(50:60), z = array(runif(121), dim = c(11,11)))
basemap = addScale(basemap)


basemap = plotBasemap(coastline = 'coastline2', scale = 10e3, lon = 0, lat = 90, projection = projectionEqualarea)
basemap = addAxis(basemap, lats = c(45, 60, 75), lons = seq(-180, 150, by = 30))

basemap = plotBasemap(coastline = 'coastline3', scale = 5e3, lon = 30, lat = 78, projection = projectionEqualarea)
basemap = addAxis(basemap, lats = c(45, 60, 75), lons = seq(-180, 150, by = 30))
basemap = addRadius(basemap, 30, 78, 3000)




basemap = plotBasemap(coastline = 'coastline3', scale = 8e3, lon = 30, lat = 78, projection = projectionEqualarea)
basemap = addAxis(basemap, lats = c(45, 60, 75), lons = seq(-180, 150, by = 30))

bathy = getBathymetry(basemap, res = basemap$scale/200)
basemap = addLayer(basemap, bathy$lon, bathy$lat, bathy$z, pal = pals::ocean.deep(16), zlim = c(-3e3, 100), trim = T)
basemap = addCoastline(basemap)




basemap = plotBasemap(coastline = 'coastline4', scale = 200, lon = -150, lat = 60)
bathy = getBathymetry(basemap, res = 1)
map = addLayer(basemap, bathy$lon, bathy$lat, bathy$z, pal = pals::ocean.(16), trim = F, zlim = c(-3e3, 1))
basemap = addCoastline(basemap)


