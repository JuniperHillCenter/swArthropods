library(sp)
library(rgdal)
library(rgeos)


# make a sampling area based on a bounding region and proximity to roads
sampArea <- function(region, roads, roadBuffer = 100, exclude = NULL, utmZone = 13) {
    crs <- CRS(sprintf('+proj=utm +zone=%s +ellps=WGS84 +datum=WGS84 +units=m +no_defs', 
                       utmZone))
    origCRS <- CRS(proj4string(region))
    
    region <- spTransform(region, crs)
    roads <- spTransform(roads, crs)
    
    sampHere <- gBuffer(roads, width = 1500)
    sampHere <- gIntersection(sampHere, region, byid = TRUE, drop_lower_td = TRUE)
    sampHere <- gDifference(sampHere, gBuffer(roads, width = roadBuffer))
    
    if(!is.null(exclude)) {
        exclude <- spTransform(exclude, crs)
        sampHere <- gDifference(sampHere, exclude)
    }
    
    sampHere <- spTransform(sampHere, origCRS)
    
    return(sampHere)
}


# generate points within the region
sampPoints <- function(n, area, dmin) {
    xy <- SpatialPoints(cbind(x = runif(n * 100, bbox(area)[1, 1], bbox(area)[1, 2]), 
                              y = runif(n * 100, bbox(area)[2, 1], bbox(area)[2, 2])), 
                        proj4string = CRS(proj4string(area)))
    
    xy <- xy[!is.na(over(xy, area)), ]
    xy <- xy[sample(nrow(xy@coords), n), ]
    
    d <- spDists(xy)
    g <- cutree(hclust(as.dist(d)), h = dmin / 1000)
    
    xy <- SpatialPointsDataFrame(xy, data = data.frame(name = 1:n, group = g), 
                                 proj4string = CRS(proj4string(area)))
    
    return(xy)
}


# put it all together
makePoints <- function(n, region, roads, roadBuffer = 100, exclude = NULL, 
                       dmin = 1000, utmZone = 13) {
    area <- sampArea(region = region, roads = roads, roadBuffer = roadBuffer, 
                     exclude = exclude, utmZone = utmZone)
    pnts <- sampPoints(n, area, dmin)
    
    return(spTransform(pnts, CRS(proj4string(region))))
}
