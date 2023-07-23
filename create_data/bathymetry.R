library(marmap)

## Global
bathy = getNOAA.bathy(-180, 180, -90, 90, resolution = 15)
bathy.global = list(Lon = as.numeric(rownames(bathy)),
                    Lat = as.numeric(colnames(bathy)),
                    Z = bathy, res = 15)
save(bathy.global, file = 'data/bathy.global.rdata')

## Arctic
bathy = getNOAA.bathy(-180, 180, 60, 90, resolution = 10)
bathy.arctic = list(Lon = as.numeric(rownames(bathy)),
                    Lat = as.numeric(colnames(bathy)),
                    Z = bathy, res = 10)

save(bathy.arctic, file = 'data/bathy.arctic.rdata')

## Antarctic
bathy = getNOAA.bathy(-180, 180, -90, -45, resolution = 30)
bathy.antarctic = list(Lon = as.numeric(rownames(bathy)),
                       Lat = as.numeric(colnames(bathy)),
                       Z = bathy, res = 15)

save(bathy.antarctic, file = 'data/bathy.antarctic.rdata')


## Pacific
bathy = getNOAA.bathy(-70, 100, -60, 60, resolution = 15, antimeridian = TRUE)
bathy.pacific = list(Lon = as.numeric(rownames(bathy)),
                     Lat = as.numeric(colnames(bathy)),
                     Z = bathy, res = 15)

save(bathy.pacific, file = 'data/bathy.pacific.rdata')


## Atlantic
bathy = getNOAA.bathy(-70, -90, -60, 10, resolution = 15, antimeridian = F)
bathy.pacific = list(Lon = as.numeric(rownames(bathy)),
                     Lat = as.numeric(colnames(bathy)),
                     Z = bathy, res = 15)

save(bathy.pacific, file = 'data/bathy.atlantic.rdata')
