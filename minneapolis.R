### Google Maps API key for geocoding: AIzaSyCrwuP4LKkmwsB_d9MLRoVrcFoIjO6cHVc

### Twitter API key: 

### scraping data from Twitter (w/ text mining): https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/

### Grid from polygon: https://rpubs.com/huanfaChen/grid_from_polygon
### Creating square grid from polygon: https://ecosystems.psu.edu/research/labs/walter-lab/manual/chapter1/1.9-creating-a-square-polygon-grid-over-a-study-area

### preamble
getwd()
work_dir <- "D:/Documents (D Drive)/PhD/Research projects/Pogroms/Minneapolis protests 2020"
setwd(work_dir)

packages <- c("reshape", "plyr", "dplyr", "car", "stargazer", "gridExtra", "olsrr", 
              "foreign", "ggplot2", "ggmap", "mapsapi", "sf", "sp", "data.table", 
              "mapdata", "maps", "raster", "rworldmap", "GADMTools", "rgdal", "nngeo", 
              "mapview", "plm", "gplots", "haven", "lfe", "plm", 
              "haven", "knitr", "AER", "DataCombine", "jtools", "maptools", "mapdata",
              "rgeos", "geosphere", "tidyr", "coefplot", "margins", "rtweet", "raster",
              "lubridate") # combines packages
lapply(packages, library, character.only = TRUE) # loads all packages in "packages" list
rm(packages, work_dir)

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

geocodeQueryCheck()

#########################
#######
####### MPLS map
#######
#########################

### Transforming polygon into a grid
# https://rpubs.com/huanfaChen/grid_from_polygon
# https://ecosystems.psu.edu/research/labs/walter-lab/manual/chapter1/1.9-creating-a-square-polygon-grid-over-a-study-area

Albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96+x_0=0+y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
Robinson_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

### download Minneapolis map, convert to SF class, transform into WGS84 datum
mpls <- readOGR("Minneapolis_Neighborhoods.shp", stringsAsFactors = FALSE)
mpls <- st_as_sf(mpls, coords = c("lat","lon"))

### subset Minneapolis map to South Minneapolis neighborhoods

### In qualitative terms, South Minneapolis refers to the Bryant, Central, 
### Corcoran, Phillips East, Phillips West, and Powderhorn Park neighborhoods 
### Center for Urban and Regional Affairs 2018: http://gentrification.umn.edu/sites/gentrification.dl.umn.edu/files/general/south-2-23-18.pdf

south_mpls_neighborhoods <- c("Bryant", "Central", "Corcoran", "East Phillips", "Phillips West", 
                              "Powderhorn Park")
south_mpls <- mpls %>% 
  filter(BDNAME %in% south_mpls_neighborhoods)

### convert South Minneapolis back into SpatialPolygonsDataFrame, project to Albers datum
south_mpls <- as_Spatial(south_mpls)
south_mpls <- spTransform(south_mpls, CRS = Albers_crs)

### transform polygon of South Minneapolis neighborhoods into a grid, 
### pixelated at 200m by 100m (the size of a Midwestern city block)

# create square grid from polygon

# identify bounds of South Minneapolis grid
bbox(south_mpls)
x = seq(from = bbox(south_mpls)[1, 1], to = bbox(south_mpls)[1, 2], by = 200)
y = seq(from = bbox(south_mpls)[2, 1], to = bbox(south_mpls)[2, 2], by = 100)

south_mpls_xy <- expand.grid(x = x, y = y)
plot(south_mpls_xy)

# remake SpatialPointsDataFrame with grid points
south_mpls_grid <- SpatialPointsDataFrame(coords = south_mpls_xy, data = south_mpls_xy,
                                          proj4string = CRS(Albers_crs))
plot(south_mpls_grid)

gridded(south_mpls_grid) <- TRUE

# grid of points -> Spatial Polygon -> SPDF
south_mpls_grid <- as(south_mpls_grid, "SpatialPolygons")
plot(south_mpls_grid)

south_mpls_grid_spdf <- SpatialPolygonsDataFrame(south_mpls_grid, 
                                                 data = data.frame(id = row.names(south_mpls_grid),
                                                                   row.names = row.names(south_mpls_grid)))
plot(south_mpls_grid_spdf)

# project SPDF onto original CRS projection, transform grid and original map to WGS84 datum
south_mpls <- spTransform(south_mpls, CRS = Albers_crs)
south_mpls_grid_spdf <- spTransform(south_mpls_grid_spdf, CRS = Albers_crs)

# to test, overlay SPDF of South Minneapolis grid with neighborhoods
plot(south_mpls_grid_spdf, col = "white")
plot(south_mpls, add = T, lwd = 5)

# clip grid to intersecting cells
south_mpls_grid_spdf$id <- 1:nrow(south_mpls_grid_spdf)
intersect_south_mpls_grid <- raster::intersect(south_mpls_grid_spdf, south_mpls)
intersect_south_mpls_grid <- south_mpls_grid_spdf[south_mpls_grid_spdf$id %in% intersect_south_mpls_grid$id, ]
plot(intersect_south_mpls_grid)

south_mpls <- intersect_south_mpls_grid
south_mpls$grid_id <- seq(1:nrow(south_mpls))

rm(mpls, south_mpls_grid, south_mpls_grid_spdf, south_mpls_xy, intersect_south_mpls_grid)

### create image of south minneapolis
jpeg("south_mpls_grid.jpeg")
plot(south_mpls)
dev.off()

### convert SPDF of South MPLS into "sf" class
south_mpls_merge <- st_as_sf(south_mpls, coords = c("lat","lon"))

### load in structural fires data from City of MPLS: http://opendata.minneapolismn.gov/datasets/4b496601fd324038b990dbd906444c7a_0
mpls_fires <- readOGR("Structural_Fires.shp", stringsAsFactors = FALSE)
mpls_fires <- spTransform(mpls_fires, CRS = Albers_crs)
south_mpls <- spTransform(south_mpls, CRS = Albers_crs)

### subset structural fires to South MPLS neighborhoods
south_mpls_fires <- raster::intersect(mpls_fires, south_mpls)
south_mpls_fires <- st_as_sf(south_mpls_fires, coords = c("lat","lon"))

### subset structural fires to May 25 - 29, 2020 (less than police curfew of 2000 / 8 pm)
south_mpls_fires$alarmDate <- ymd(south_mpls_fires$alarmDate)
south_mpls_fires <- subset(south_mpls_fires, south_mpls_fires$alarmDate <= as.Date("2020-05-29") &
                             south_mpls_fires$alarmDate >= as.Date("2020-05-25"))

rm(mpls_fires)

### left-join structural fires to South MPLS grid unit to identify grid units associated with structural fires (st_intersects is join function)
south_mpls_fires_grid <- st_join(south_mpls_merge, south_mpls_fires, join = st_intersects)

### aggregate south_mpls_fires by grid unit
south_mpls_fires_grid$fire <- ifelse(is.na(south_mpls_fires_grid$incidentID) == FALSE, 1, 0)
south_mpls_fires_aggregate <- aggregate(south_mpls_fires_grid$fire,
                                        by = list(south_mpls_fires_grid$grid_id.x),
                                        FUN = sum)

south_mpls_fires <- merge(south_mpls_fires_aggregate,
                          south_mpls_merge,
                          by.x = "Group.1", by.y = "grid_id")

rm(south_mpls_fires_aggregate, south_mpls_merge)

### transform fires data
setnames(south_mpls_fires, old = "x", new = "fires_total")
south_mpls_fires$fires_dummy <- ifelse(south_mpls_fires$fires_total > 0, 1, 0)

### map of structural fires

### convert south_mpls_fires back into SpatialPolygonsDataFrame, project to Albers datum
south_mpls_fires_sf <- st_as_sf(south_mpls_fires)
south_mpls_fires_spdf <- as_Spatial(south_mpls_fires_sf)
south_mpls_fires_spdf <- spTransform(south_mpls_fires_spdf, CRS = Robinson_crs)

rm(south_mpls_fires_sf)

### transform fires data to be legible in ggplot
south_mpls_fires_spdf@data$id <- rownames(south_mpls_fires_spdf@data)
south_mpls_fires_fortify <- fortify(south_mpls_fires_spdf, region = "Group.1")
south_mpls_fires_spdf <- merge(south_mpls_fires_fortify, south_mpls_fires_spdf@data,
                              by.x = "id", by.y = "Group.1")

rm(south_mpls_fires_fortify)

### binary structural fires map
binary_fires <- ggplot(data = south_mpls_fires_spdf,
                        aes(x = long, y = lat, 
                            group = group,
                            fill = factor(fires_dummy)))

pdf("binary_fires.pdf", width = 7, height = 7)
binary_fires +
  geom_polygon() + 
  scale_fill_manual(name = "Legend",
                    values = c("gray", "red"),
                    breaks = c("1", "0"),
                    labels = c("Structural fire reported", 
                               "No structural fire reported"),
                    na.value = "gray") +
  ggtitle("Structural fires in South Minneapolis, May 25 - May 29, 2020") +
  labs(caption = "Source: http://opendata.minneapolismn.gov/") +
  geom_path(col = "gray90", size = 0.05) +
  coord_map(projection = "albers", lat0 = 47, lat1 = 56)
dev.off()

#########################
#######
####### Scrape Twitter data: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#######
#########################

