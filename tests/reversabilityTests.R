library(SimpleMapper)


N = 1000
points = data.frame(lon = runif(N, -180, 180), lat = runif(N, -90, 90))
center = points[1,]
center = list(lon = 0, lat = 0)


## Project, invert, test
tmp = projectionEqualarea(lon = points$lon, lat = points$lat, lon0 = center$lon, lat0 = center$lat)
tmp = projectionEqualarea(tmp$x, tmp$y, center$lon, center$lat, inv = T)


#hist(abs(points$lon - tmp$longitude))
plot(points$lon, tmp$longitude)
abline(0, 1, col = 'red', lwd = 2)

plot(points$lat, tmp$latitude)
abline(0, 1, col = 'red', lwd = 2)
message('Maximum deviation is (', round(max(abs(points$lon - tmp$longitude)),4), ', ', round(max(abs(points$lat - tmp$latitude)),4), ')')


## Project, invert, test
tmp = projectionOrthographic(lon = points$lon, lat = points$lat, lon0 = center$lon, lat0 = center$lat)
tmp = projectionOrthographic(tmp$x, tmp$y, center$lon, center$lat, inv = T)

summary(abs(points$lon - tmp$longitude), na.rm = T)
summary(abs(points$lat - tmp$latitude), na.rm = T)
summary(abs(points$lon - tmp$longitude) + abs(points$lat - tmp$latitude), na.rm = T)

hist(abs(points$lon - tmp$longitude))
plot(points$lon, tmp$longitude)
abline(0, 1, col = 'red', lwd = 2)
message('Maximum deviation is (', round(max(abs(points$lon - tmp$longitude)),4), ', ', round(max(abs(points$lat - tmp$latitude)),4), ')')


## Project, invert, test
tmp = projectionStereographic(lon = points$lon, lat = points$lat, lon0 = center$lon, lat0 = center$lat)
tmp = projectionStereographic(tmp$x, tmp$y, center$lon, center$lat, inv = T)

summary(abs(points$lon - tmp$longitude), na.rm = T)
summary(abs(points$lat - tmp$latitude), na.rm = T)
summary(abs(points$lon - tmp$longitude) + abs(points$lat - tmp$latitude), na.rm = T)

hist(abs(points$lon - tmp$longitude))
plot(points$lon, tmp$longitude)
abline(0, 1, col = 'red', lwd = 2)
message('Maximum deviation is (', round(max(abs(points$lon - tmp$longitude)),4), ', ', round(max(abs(points$lat - tmp$latitude)),4), ')')
