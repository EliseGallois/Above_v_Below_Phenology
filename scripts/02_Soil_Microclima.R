### 2. Extract microclima data from belowground Cairngorms plots ###
### Elise Gallois, elise.gallois94@gmail.com ###
### Extract microclima data for soil core points ####
### Date: 10th Jan 2022 ###

#### 1 - LOAD PACKAGES  ##### 

library(dplyr)
library(readr)
library(tidyverse) 
library(esquisse)
library(ggpubr)
library(raster) 
library(gridExtra) 
library(viridis)

#install.packages("devtools")
#devtools::install_github("ilyamaclean/microclima")
#devtools::install_github('mrke/NicheMapR')
"rgdal_show_exportToProj4_warnings"="none"
library(microclima)
library(NicheMapR)

#### 2 - LOAD  PHENOLOGY  DATA ####

cairn_soil <- read_csv("users/egallois/soilcores/data/cairn_soil.csv")

# extract lon and lat
xy <- cairn_soil[,c(5,4)]
str(xy)
coordinates(xy) <- c("Longitude", "Latitude")
projection(xy) <- "+proj=longlat +datum=WGS84"

xy <-spTransform(xy, CRS= "+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs")
str(xy)

#### 2. RUN MODEL ####
#download global climates dataset from nichemapr (only run if not downloaded onto computer! huge file)
get.global.climate()

# prepare raster at 30m res
r <- microclima::get_dem(lat = 57.113, lon = -3.831, resolution = 30)
plot(r)
points(cairn_soil$Longitude,cairn_soil$Latitude, pch=10, cex=.1, col = 3)

# Takes ~ c. 5 minutes to run
temps <- microclima::runauto(r, "05/11/2019", "05/11/2019", hgt = 0.1,
                             l = NA, x = NA, habitat = "Open shrublands", 
                             plot.progress = TRUE)

#extract mean, min and max temp outputs
meantemp <- temps$tmean    
maxtemp <- temps$tmax
mintemp <- temps$tmin

#save mean min and max temp as rasters
writeRaster(meantemp, 'users/egallois/soilcores/maps/nov19_meantemp.tif', format = 'GTiff')
writeRaster(maxtemp, 'users/egallois/soilcores/maps/nov19_maxtemp.tif', format = 'GTiff')
writeRaster(mintemp, 'users/egallois/soilcores/maps/nov19_mintemp.tif', format = 'GTiff') 

# Takes ~ c. 5 minutes to run
temps <- microclima::runauto(r, "12/06/2019", "12/06/2019", hgt = 0.1,
                             l = NA, x = NA, habitat = "Open shrublands", 
                             plot.progress = TRUE)

#extract mean, min and max temp outputs
meantemp <- temps$tmean    
maxtemp <- temps$tmax
mintemp <- temps$tmin

#save mean min and max temp as rasters
writeRaster(meantemp, 'users/egallois/soilcores/maps/jun19_meantemp.tif', format = 'GTiff')
writeRaster(maxtemp, 'users/egallois/soilcores/maps/jun19_maxtemp.tif', format = 'GTiff')
writeRaster(mintemp, 'users/egallois/soilcores/maps/jun19_mintemp.tif', format = 'GTiff') 

#### 3. EXTRACT DATA POINTS ####

# Load rasters for future use
nov19mean <- raster('users/egallois/soilcores/maps/nov19_meantemp.tif')
nov19max <- raster('users/egallois/soilcores/maps/nov19_maxtemp.tif')
nov19min <- raster('users/egallois/soilcores/maps/nov19_mintemp.tif')
jun19mean <- raster('users/egallois/soilcores/maps/jun19_meantemp.tif')
jun19max <- raster('users/egallois/soilcores/maps/jun19_maxtemp.tif')
jun19min <- raster('users/egallois/soilcores/maps/jun19_mintemp.tif')

# convert from character to numeric
cairn_soil$Latitude <- as.numeric(cairn_soil$Latitude)
cairn_soil$Longitude <- as.numeric(cairn_soil$Longitude)

# get coordinate and extent values for point and raster data
proj4string(nov19mean)

# prepare spatial points layer
#rename lat lon to x y 
names(cairn_soil)[names(cairn_soil)=="Latitude"]<- "Y"
names(cairn_soil)[names(cairn_soil)=="Longitude"]<- "X"

# correct to UTM proj
cord.dec = SpatialPoints(cbind(cairn_soil$X, cairn_soil$Y), 
                         proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))

# align crs for all datasets
cord.UTM_nov19mean <- spTransform((cord.UTM), crs(nov19mean))
cord.UTM_nov19max <- spTransform((cord.UTM), crs(nov19max))
cord.UTM_nov19min <- spTransform((cord.UTM), crs(nov19min))

cord.UTM_jun19mean <- spTransform((cord.UTM), crs(jun19mean))
cord.UTM_jun19max <- spTransform((cord.UTM), crs(jun19max))
cord.UTM_jun19min <- spTransform((cord.UTM), crs(jun19min))

# extract climate based on points
nov19mean_point <- raster::extract(nov19mean, cord.UTM_nov19mean, method='simple',df=TRUE)
nov19max_point <- raster::extract(nov19max, cord.UTM_nov19max, method='simple',df=TRUE)
nov19min_point <- raster::extract(nov19min, cord.UTM_nov19min, method='simple',df=TRUE)
jun19mean_point <- raster::extract(jun19mean, cord.UTM_jun19mean, method='simple',df=TRUE)
jun19max_point <- raster::extract(jun19max, cord.UTM_jun19max, method='simple',df=TRUE)
jun19min_point <- raster::extract(jun19min, cord.UTM_jun19min, method='simple',df=TRUE)

# bind these climate points together with coords and plot ID
combinePointValue = cbind(cairn_soil,
                          nov19mean_point, nov19max_point, nov19min_point,
                          jun19mean_point, jun19max_point, jun19min_point)

colnames(combinePointValue)

# remove redundant columns
cairn_full <- combinePointValue[,c(1,2,3,4,5,21,23,25,27,29,31)]

# save csv
write.csv(cairn_full, file = "users/egallois/soilcores/data/cairn19_microdata.csv", row.names = FALSE)

#### 4. PLOT POINTS ON MAP ####
plot(jun19mean,col=inferno(100), main="Mean Surface Temperature 12th June 2019 (\u00B0C)")
plot(cord.UTM_jun19mean,  pch=10, cex=1, col = 3, add=TRUE)


plot(nov19mean,col=inferno(100), main="Mean Surface Temperature 5th November 2019 (\u00B0C)")
plot(cord.UTM_jun19mean,  pch=10, cex=1, col = 3, add=TRUE)
