# Simple Mapper
 A Simple mapping library for R. This library is self contained with some optional linkages to other libraries/datasets.

Inspiration for this project originated back in 2017 when we were first exploring options for mapping and visualizing spatial data in the R universe. What we found, and largely still find, was an ecosystem of GIS-centric libraries with far more horse-power than we needed.

Power is great, except when it comes with a learning curve and inflexibility. Finding the then current libraries unable to reliably spit out a map of our data points with clear cause-and-effect structures (e.g., do this to add an arrow or do this to add a label), we decided to build our own library suited to the task of making maps simpler.

#### Getting Started




#### Customizable Projections

__SimpleMapper__ is designed from the ground up to be extensible, customizable, and adaptable; so if you would like to apply a custom projection it is as easy as this:

  projectionRotate = function(lon, lat, lon0 = 0, lat0 = 0, inv = F, ...) {
  
    ## Start with= a centered rectangular projection
    lon = lon - lon0
    lat = lat - lat0
    
    ## Apply random rotation
    phi = runif(1, 0, 2*pi)
    lon = cos(phi) * lon - sin(phi) * lat
    lat = cost(phi) * lat + sin(phi) * lon
    
    return(data.frame(longitude = lon,
                      latitude = lat))
  }

This probably isn't a very useful projection, but it certainly works. Here's a few example iterations.

