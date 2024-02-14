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

object.size(eez)
