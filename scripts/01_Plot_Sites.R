# 1. Make map of all sites #
## Elise Gallois, elise.gallois94@gmail.com 
## Script created: 25th Oct 2021
# Objective 1: Make map of all datasets for newsletter and later publications


#### LOAD PACKAGES ####
library(tidyverse)
library(esquisse) 
library(rworldmap)
library(rgdal) 
library(raster) 
library(ggsn)
library(viridis)
library(ggalt)
library(maptools)
library(readr)
library(ggrepel)

#### 1 - Load data ####
soilcores <- read.csv("users/egallois/soilcores/maps/soilcore_sites_2023.csv")
str(soilcores)


soilcores_process <- soilcores %>% 
                     filter(site %in% c("Cairngorms", "Kluane", "Niwot Ridge", "Toolik Lake", "BC Coastal Mountains", "Kluane"))

#### 2 - Plot data on map, colour by tundra type ####

# make polar map 
data("wrld_simpl", package = "maptools")                                                                            
(soil_map <- ggplot() +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = "grey80", 
                 colour = "grey80", size = 0.5, alpha = 0.5) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    scale_y_continuous(breaks = seq(45, 90, by = 50), labels = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL, legend = "Current Status") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = NA),
          axis.ticks = element_blank(),
          legend.title = element_blank()) +
    geom_point(data = soilcores_process,  
               aes(x = lon, y = lat), size = 7, position = "jitter", alpha = 0.6, color = "#F09205") +
    geom_label_repel(data = soilcores_process,
                     aes(x = lon, y = lat,
                         label = site) ,
                     # Setting the positions of the labels
                     box.padding = 0.9, size = 4, nudge_x = 2, nudge_y = -6, 
                     segment.alpha = 0.8, segment.colour = "#F09205"))






ggsave(soil_map, filename = "users/egallois/soilcores/maps/soilcore_sites.png",
       height = 5, width = 5)



#### 3 - Make map of all sites for job talk ####
#### 1 - Load data ####
field <- read.csv("users/egallois/soilcores/maps/phd_sites.csv")
str(field)


#### 2 - Plot data on map, colour by tundra type ####

# make polar map 
data("wrld_simpl", package = "maptools")                                                                            
(soil_map <- ggplot() +
        geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = "grey80", 
                     colour = "grey80", size = 0.5, alpha = 0.5) +
        coord_map("ortho", orientation = c(90, 0, 0)) +
        scale_y_continuous(breaks = seq(45, 90, by = 50), labels = NULL) +
        scale_x_continuous(breaks = NULL) +
        labs(x = NULL, y = NULL, legend = "Current Status") +
        theme(panel.background = element_blank(),
              panel.grid.major = element_line(colour = NA),
              axis.ticks = element_blank(),
              legend.title = element_blank()))






ggsave(soil_map, filename = "users/egallois/soilcores/maps/field_all_sites.png",
       height = 5, width = 9)
