devtools::install_github('tbrycekelly/SimpleMapper')
library(SimpleMapper)


sschl = readNC('~/../Downloads/A20170012017031.L3m_MO_SST_4km.nc')

## (Optional) Trim sschl since it's global and I don't need global:
#k = which(sschl$lat > 20 & sschl$lat < 60)
#sschl$lat = sschl$lat[k]
#sschl$chlor_a = sschl$chlor_a[,k]

#k = which(sschl$lon > -90 & sschl$lon < -50)
#sschl$lon = sschl$lon[k]
#sschl$chlor_a = sschl$chlor_a[k,]


## Make a basemap:
# Coatlines: there are 5 built in resolutions: 'coastline1' = coarse; 'coastline5' = very high resolution. There are also 'antarctic1' -> 'antarctic5' as well.
map = plotBasemap(coastline = 'coastline4',
                  lon = -71, 
                  lat = 42,
                  scale = 500)


## Add a layer:
map = addLayer(basemap = map, 
               lon = sschl$lon,
               lat = sschl$lat,
               z = sschl$sst)

## Read coastline to clean it up:
map = addCoastline(map)

## Add other points/lines/etc:
map = addPoints(map,
                lon = runif(10, -72, -70),
                lat = rnorm(10, 40.5, 0.2),
                col = 'red', pch = 16)



## to add some color:
map = plotBasemap(coastline = 'coastline4',
                  lon = -71, 
                  lat = 42,
                  scale = 500)


## Add a layer:
library(pals)
map = plotBasemap(coastline = 'coastline4',
                  lon = -71, 
                  lat = 42,
                  scale = 500)

map = addLayer(basemap = map, 
               lon = sschl$lon,
               lat = sschl$lat,
               z = sschl$sst, pal = cubicl(16), zlim = c(0, 20))

map = addCoastline(map)

map = addScale(map)
