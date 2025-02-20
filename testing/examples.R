library(SimpleMapper)

map = plotBasemap()
map = addLatitude(map)
map = addLongitude(map)

map = plotBasemap(scale = 5e3, land.col = 'black')
map = addLatitude(map)
map = addLongitude(map)


map = plotBasemap(scale = 20e3, lat = 65.3)
map = addLatitude(map)
map = addLongitude(map)
map = addEEZ(map, col = 'red')



map = plotBasemap(scale = 8e3, lat = -45.3, lon = -105)
map = addLatitude(map)
map = addLongitude(map)


map = plotBasemap(scale = 3e3, lat = -50.3, lon = -105)
map = plotBasemap(scale = 6e3, lat = 60, lon = -20, projection = projectionStereographic)
map = plotBasemap(scale = 16e3, lat = 60, lon = 100, projection = projectionXY)
map = addLatitude(map, sides = 2)
map = addLongitude(map)



map = plotBasemap(scale = 250, lat = 25.3, lon = -110)
map = addLatitude(map)
map = addLongitude(map)
