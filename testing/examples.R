map = plotBasemap()
map = addAxis(map, lons = seq(-180, 180, by = 5), lats = seq(-90, 90, by = 5), sides = 2)


map = plotBasemap(scale = 5e3)
map = addAxis(map, lons = seq(-180, 180, by = 5), lats = seq(-90, 90, by = 15))


map = plotBasemap(scale = 20e3, lat = 65.3)
map = addAxis(map, lons = seq(-180, 180, by = 5), lats = seq(-90, 90, by = 15), sides = c(1,2))
D