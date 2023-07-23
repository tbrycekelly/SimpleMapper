library(rgdal)


## Citation:
# Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386

## Download raw data from https://www.marineregions.org/downloads.php
# Under Shapefiles > World EEZ v.11 > Shapefile
# and
# Shapefiles > High Seas v.1 > Shapefile

eez = readOGR('eez_boundaries_v11.shp')
eez@data$SOVEREIGN1[is.na(eez@data$SOVEREIGN1)] = 'Other'
eez@data$SOVEREIGN2[is.na(eez@data$SOVEREIGN2)] = 'Other'

highseas = readOGR('High_Seas_v1.shp')

eez.data = list()
for (name in unique(c(eez@data$SOVEREIGN1, eez@data$SOVEREIGN2))) {
  if (!is.na(name)) {
    l = which((name == eez@data$SOVEREIGN1 | name == eez@data$SOVEREIGN2) & eez@data$LINE_TYPE != 'Straight Baseline')
    
    eez.data[[name]] = list()
    for (i in 1:length(l)) {
      k = l[i]
      eez.data[[name]][[i]] = data.frame(lon = eez@lines[[k]]@Lines[[1]]@coords[,1],
                                         lat = eez@lines[[k]]@Lines[[1]]@coords[,2])
    }
  }
}

eez.data[['Highseas']] = list()
for (i in 1:length(highseas@polygons[[1]]@Polygons)) {
  for (j in 1:length(highseas@polygons[[1]]@Polygons[[i]])) {
    eez.data[['Highseas']][[length(eez.data[['Highseas']]) + 1]] = data.frame(lon = highseas@polygons[[1]]@Polygons[[i]]@coords[,1],
                                                                              lat = highseas@polygons[[1]]@Polygons[[i]]@coords[,2])
  }
}

save(eez.data, file = 'data/eez.rdata')
