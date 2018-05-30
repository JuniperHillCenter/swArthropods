library(rgdal)
library(rgeos)


sev <- readOGR('permits/sev/sevilleta.kml', 'sevilleta.kml')
sevRoads <- readOGR('permits/sev/sev_roads.kml', 'sev_roads.kml')
blue <- readOGR('permits/sev/blue_grama_site.kml', 'blue_grama_site.kml')
black <- readOGR('permits/sev/black_grama_site.kml', 'black_grama_site.kml')
coreSites <- rbind(blue, black)


crs <- CRS('+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
origCRS <- CRS(proj4string(sev))

sev <- spTransform(sev, crs)
sevRoads <- spTransform(sevRoads, crs)
coreSites <- spTransform(coreSites, crs)

sampArea <- gBuffer(sevRoads, width = 1500)
sampArea <- gIntersection(sampArea, sev, byid = TRUE, drop_lower_td = TRUE)
sampArea <- gDifference(sampArea, gBuffer(coreSites, width = 1000), byid = TRUE, drop_lower_td = TRUE)

plot(sev)
plot(sampArea, col = hsv(0.6, 0.5), add = TRUE)

xy <- SpatialPoints(cbind(x = runif(1000, bbox(sampArea)[1, 1], bbox(sampArea)[1, 2]), 
                          y = runif(1000, bbox(sampArea)[2, 1], bbox(sampArea)[2, 2])), 
                    proj4string = crs)

npoints <- 10
xy <- xy[!is.na(over(xy, sampArea)), ]
xy <- xy[sample(nrow(xy@coords), npoints), ]

plot(xy, add = TRUE)

xy <- spTransform(xy, origCRS)
xy <- SpatialPointsDataFrame(xy, data = data.frame(name = 1:npoints), proj4string = origCRS)

writeOGR(xy, dsn = 'permits/sev/sev_candidate_points.kml', layer = 'sev_candidate_points', 
         driver = 'KML', overwrite_layer = TRUE)
