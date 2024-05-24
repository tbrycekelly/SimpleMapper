library(sf)


## Citation:
# Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386

## Download raw data from https://www.marineregions.org/downloads.php
# Under Shapefiles > World EEZ v.12 > Shapefile
# and
# Shapefiles > High Seas v.1 > Shapefile
eez = sf::st_read('~/../Downloads/World_EEZ_v12_20231025/World_EEZ_v12_20231025/eez_boundaries_v12.shp')
#eez = sf::st_read('~/../Downloads/World_EEZ_v12_20231025/World_EEZ_v12_20231025/eez_v12.shp')
eez$SOVEREIGN1[is.na(eez$SOVEREIGN1)] = 'Other'
eez$SOVEREIGN2[is.na(eez$SOVEREIGN2)] = 'Other'


eez.data = list()
for (name in unique(c(eez$SOVEREIGN1, eez$SOVEREIGN2))) {
  if (!is.na(name)) {
    l = which((name == eez$SOVEREIGN1 | name == eez$SOVEREIGN2) &
                !(eez$LINE_TYPE %in% c('12 NM', 'Archipelagic baseline', 'Normal baseline (official)', 'Straight baseline')))
    
    eez.data[[name]] = list()
    for (i in 1:length(l)) {
      k = l[i]
      for (j in 1:length(eez$geometry[[k]])) {
        eez.data[[name]][[length(eez.data[[name]]) + 1]] = data.frame(lon = eez$geometry[[k]][[j]][,1],
                                           lat = eez$geometry[[k]][[j]][,2])
      }
    }
  }
}

highseas = sf::st_read('~/../Downloads/World_High_Seas_v1_20200826/High_Seas_v1.shp')

eez.data[['Highseas']] = list()
for (i in 1:length(highseas$geometry[[1]])) {
  for (j in 1:length(highseas$geometry[[1]][[i]])) {
    eez.data[['Highseas']][[length(eez.data[['Highseas']]) + 1]] = data.frame(lon = highseas$geometry[[1]][[4]][[1]][,1],
                                                                              lat = highseas$geometry[[1]][[4]][[1]][,2])
  }
}

eez5 = eez.data
save(eez5, file = 'data/eez5.rdata')

object.size(eez5)

eez4 = eez5
k = 3


subsample = function(eez, k = 3, minPoints = 100) {
  for (i in 1:length(eez)) {
    for (j in 1:length(eez[[i]])) {
      n = nrow(eez[[i]][[j]])
      if (n > minPoints) {
        #eez[[i]][[j]] = data.frame(lon = runmed(eez[[i]][[j]]$lon, k),
        #                           lat = runmed(eez[[i]][[j]]$lat, k))
        
        eez[[i]][[j]] = eez[[i]][[j]][round(seq(1, n, length.out = n / k)),]
      }
    }
  }
  eez
}

eez4 = subsample(eez5)
as.numeric(object.size(eez4) / object.size(eez5))

eez3 = subsample(eez4)
as.numeric(object.size(eez3) / object.size(eez5))

eez2 = subsample(eez3)
as.numeric(object.size(eez2) / object.size(eez5))

eez1 = subsample(eez2)
as.numeric(object.size(eez1) / object.size(eez5))

map = plotBasemap(scale = 4e3, lon = -120, lat = 60)
map = addEEZ(map, eez = eez5, col = 'black')
map = addEEZ(map, eez = eez2, col = 'red')


save(eez5, file = 'data/eez5.rdata')
save(eez4, file = 'data/eez4.rdata')
save(eez3, file = 'data/eez3.rdata')
save(eez2, file = 'data/eez2.rdata')
save(eez1, file = 'data/eez1.rdata')

