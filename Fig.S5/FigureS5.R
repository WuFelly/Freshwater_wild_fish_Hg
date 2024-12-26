library(ggplot2)  # v3.5.0
library(sf)       # v1.0-15
library(dplyr)    # v1.1.4

setwd('path_to_your_working_directory')

sampling_site_raw_data <- read.csv('Freshwater_wild_sampling_sites.csv')
sampling_site_number <-distinct(sampling_site_raw_data, lat, lon)
sampling_site_raw_data$jitter_lon <- jitter(sampling_site_raw_data$lon, factor = 0.35)
sampling_site_raw_data$jitter_lat <- jitter(sampling_site_raw_data$lat, factor = 0.35)

China_hydro <- sf::st_read('China_hydrological_map/Line.shp')
China_map <- sf::st_read('China_map/China_area.shp')
nine <- sf::st_read('China_map/Nine_dash_line.shp')
south_china_sea <- sf::st_read('China_map/South_China_Sea.shp')

# Remap to China hydrological map===============================================
ggplot() +
  geom_sf(data = China_map, aes(geometry = geometry),color = '#5f5f5f', fill = "white",linewidth = 0.5)+
  geom_sf(data = nine, aes(geometry = geometry),color = '#5f5f5f', linewidth = 0.5)+ 
  geom_sf(data = south_china_sea, aes(geometry = geometry), color = '#5f5f5f', linewidth = 0.5)+
  geom_sf(data = China_hydro, aes(geometry = geometry), color = '#6495ED', fill = "white",linewidth = 0.1)+
  geom_point(data = sampling_site_raw_data, aes(x = jitter_lon, y = jitter_lat),
             shape = 21, stroke = 0.05, color = "#5f5f5f", fill = '#ffc001', size = 2.5,
             position = position_jitter(width = 0.1, height = 0.1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) 

#ggsave('freshwater_wild_sampling_sites.pdf', plot = last_plot(), width = 8, height = 5, units = 'in')
