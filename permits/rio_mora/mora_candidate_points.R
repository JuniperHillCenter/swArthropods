library(ggmap)
library(ggplot2)
library(broom)

source('make_candidate_points.R')

mor <- readOGR('permits/rio_mora/rio_mora.kml', 'rio_mora.kml')
morRoads <- readOGR('permits/rio_mora/rio_mora_roads.kml', 'rio_mora_roads.kml')
morShrub <- readOGR('permits/rio_mora/rio_mora_shrub.kml', 'rio_mora_shrub.kml')


# sat map
rioMoraLoc <- c(lon = -105.067623, lat = 35.843262)
rioMoraMap <- get_map(rioMoraLoc, zoom = 13, maptype = 'satellite', messaging = FALSE)

ylim <- c(35.815, 35.87)

rioMora <- tidy(readOGR('permits/rio_mora/rio_mora_refuge.kml', 'rio_mora_refuge.kml'))
rioMora <- rioMora[!rioMora$hole, ]
rioMora <- cbind(rioMora, poly = 'Refuge boundary')

pdf('permits/rio_mora/mora_candidate_points.pdf', width = 10, height = 7.5)

# p <- makePoints(12, region = mor, roads = morRoads, roadBuffer = 100, exclude = morShrub, 
#                 dmin = 1000)

ggmap(rioMoraMap) + 
    geom_polygon(data = rioMora, 
                 aes(long, lat, colour = poly),
                 alpha = 0.3) +
    geom_text(aes(x = x, y = y, label = name, color = group), 
               data = data.frame(coordinates(p), name = as.character(p$name), 
                                 group = as.character(p$group))) + 
    coord_fixed(ylim = ylim, ratio = 1 / cos(pi * mean(ylim) / 180)) + 
    theme(axis.text = element_text(size = 0), 
          axis.ticks = element_line(color = 'transparent', size = 0), 
          axis.title = element_text(size = 0),
          legend.position = 'none', 
          plot.margin = margin(0, 0, 0, 0, 'pt')) +
    scale_color_manual(values = c(hcl(seq(0, 300, length.out = length(unique(p$group)))), 
                                  'white'))

dev.off()

writeOGR(p, 'permits/rio_mora/mora_candidate_points.kml', 'mora_candidate_points', 
         driver = 'KML', overwrite_layer = TRUE)

# for gps
p@data <- data.frame(name = paste0('mor_', LETTERS[p$group], '-', 
                                   gsub(' ', '0', format(p$name, width = 2, 
                                                         justify = 'right'))))

writeOGR(p, 'permits/rio_mora/mora_candidate_points.gpx', 'mora_candidate_points', 
         driver = 'GPX')
