library(dplyr)        # v1.1.4
library(ggplot2)      # v3.5.0
library(sf)           # v1.0-15
library(maptools)     # v1.1-8
library(raster)       # v 3.6-26
library(spData)       # v2.3.0
library(terra)        # v1.7-65
library(geodata)      # v0.5-9
library(patchwork)    # v1.2.0
library(RColorBrewer) # v1.1-3
library(ggspatial)    # v1.1.9
library(ggplotify)    # v0.1.2

setwd('path_to_your_working_directory')

piscivore_present <- read.csv("Piscivore_present_2022.csv")
piscivore_near_245 <- read.csv("Piscivore_near_future_ssp245.csv")
piscivore_far_245 <- read.csv("Piscivore_far_future_ssp245.csv")
piscivore_near_585 <- read.csv("Piscivore_near_future_ssp585.csv")
piscivore_far_585 <- read.csv("Piscivore_far_future_ssp585.csv")
omn_present <- read.csv("Omnivore_present_2022.csv")
omn_near_245 <- read.csv("Omnivore_near_future_ssp245.csv")
omn_far_245 <- read.csv("Omnivore_far_future_ssp245.csv")
omn_near_585 <- read.csv("Omnivore_near_future_ssp585.csv")
omn_far_585 <- read.csv("Omnivore_far_future_ssp585.csv")

# Plot future piscivore maps====================================================
piscivore_near_245_merged <- merge(piscivore_present, piscivore_near_245, by = c("lat", "lon"), suffixes = c("_present", "_near245"))
piscivore_far_245_merged <- merge(piscivore_present, piscivore_far_245, by = c("lat", "lon"), suffixes = c("_present", "_far245"))
piscivore_near_585_merged <- merge(piscivore_present, piscivore_near_585, by = c("lat", "lon"), suffixes = c("_present", "_near585"))
piscivore_far_585_merged <- merge(piscivore_present, piscivore_far_585, by = c("lat", "lon"), suffixes = c("_present", "_far585"))

piscivore_near_245_merged <- piscivore_near_245_merged[!is.na(piscivore_near_245_merged$mehg_ppb_near245),]
piscivore_far_245_merged <- piscivore_far_245_merged[!is.na(piscivore_far_245_merged$mehg_ppb_far245),]
piscivore_near_585_merged <- piscivore_near_585_merged[!is.na(piscivore_near_585_merged$mehg_ppb_near585),]
piscivore_far_585_merged <- piscivore_far_585_merged[!is.na(piscivore_far_585_merged$mehg_ppb_far585),]

piscivore_near245_increase <- piscivore_near_245_merged$mehg_ppb_near245 - piscivore_near_245_merged$mehg_ppb_present
piscivore_far245_increase <- piscivore_far_245_merged$mehg_ppb_far245 - piscivore_far_245_merged$mehg_ppb_present
piscivore_near585_increase <- piscivore_near_585_merged$mehg_ppb_near585 - piscivore_near_585_merged$mehg_ppb_present
piscivore_far585_increase <- piscivore_far_585_merged$mehg_ppb_far585 - piscivore_far_585_merged$mehg_ppb_present

piscivore_results <- data.frame(
  lat = piscivore_near_245_merged$lat,
  lon = piscivore_near_245_merged$lon,
  near245_increase = piscivore_near245_increase,
  far245_increase = piscivore_far245_increase,
  near585_increase = piscivore_near585_increase,
  far585_increase = piscivore_far585_increase
)

create_piscivore_map <- function(pred_data, mehg_column, title) {
  pred_data <- pred_data %>% distinct(lat, lon, .data[[mehg_column]])
  
  China_map <- map_data('world', region = 'China')
  china <- sf::st_read('China_map/China_area.shp')
  china_area <- sf::st_read('China_map/Area.shp')
  nine <- sf::st_read('China_map/Nine_dash_line.shp')
  south_china_sea <- sf::st_read('China_map/South_China_Sea.shp')
  
  pred_data <- pred_data %>%
    mutate(
      Level = case_when(
        .data[[mehg_column]] <= 5 ~ 'Level_1',
        .data[[mehg_column]] > 5 & .data[[mehg_column]] <= 10 ~ "Level_2",
        .data[[mehg_column]] > 10 & .data[[mehg_column]] <= 20 ~ "Level_3",
        .data[[mehg_column]] > 20 & .data[[mehg_column]] <= 30 ~ "Level_4",
        .data[[mehg_column]] > 30 & .data[[mehg_column]] <= 40 ~ "Level_5",
        .data[[mehg_column]] > 40 & .data[[mehg_column]] <= 50 ~ "Level_6",
        .data[[mehg_column]] > 50 & .data[[mehg_column]] <= 100 ~ "Level_7",
        .data[[mehg_column]] > 100  ~ "Level_8",
        TRUE ~ "Other"
      )
    )
  
  main_map <- ggplot() +
    geom_tile(data = pred_data, aes(x = lon, y = lat, fill = Level)) +
    scale_fill_brewer(palette = 'RdYlGn', direction = -1) + 
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          legend.position = 'right') +
    labs(x = 'Longitude', y = 'latitude', 
         title = title) +
    geom_sf(data = china_area, aes(geometry = geometry), fill = NA, color = 'black', linewidth = 0.3) +
    geom_sf(data = nine, aes(geometry = geometry),color = 'black', linewidth = 0.3)+ 
    geom_sf(data = south_china_sea, aes(geometry = geometry), color = 'black', linewidth = 0.3)
  
  return(main_map)
}

maps <- list()
maps$near245 <- create_piscivore_map(piscivore_results, "near245_increase", "Near Future SSP245")
maps$far245 <- create_piscivore_map(piscivore_results, "far245_increase", "Far Future SSP245")
maps$near585 <- create_piscivore_map(piscivore_results, "near585_increase", "Near Future SSP585")
maps$far585 <- create_piscivore_map(piscivore_results, "far585_increase", "Far Future SSP585")

piscivore_map <- (maps$near245|maps$far245)/(maps$near585|maps$far585)
piscivore_map

#ggsave("Piscivore_map.pdf", piscivore_map, width = 10, height = 10, units = "in")

# Plot future omnivore maps=====================================================
omn_near_245_merged <- merge(omn_present, omn_near_245, by = c("lat", "lon"), suffixes = c("_present", "_near245"))
omn_far_245_merged <- merge(omn_present, omn_far_245, by = c("lat", "lon"), suffixes = c("_present", "_far245"))
omn_near_585_merged <- merge(omn_present, omn_near_585, by = c("lat", "lon"), suffixes = c("_present", "_near585"))
omn_far_585_merged <- merge(omn_present, omn_far_585, by = c("lat", "lon"), suffixes = c("_present", "_far585"))

omn_near_245_merged <- omn_near_245_merged[!is.na(omn_near_245_merged$mehg_ppb_near245),]
omn_far_245_merged <- omn_far_245_merged[!is.na(omn_far_245_merged$mehg_ppb_far245),]
omn_near_585_merged <- omn_near_585_merged[!is.na(omn_near_585_merged$mehg_ppb_near585),]
omn_far_585_merged <- omn_far_585_merged[!is.na(omn_far_585_merged$mehg_ppb_far585),]

omn_near245_increase <- omn_near_245_merged$mehg_ppb_near245 - omn_near_245_merged$mehg_ppb_present
omn_far245_increase <- omn_far_245_merged$mehg_ppb_far245 - omn_far_245_merged$mehg_ppb_present
omn_near585_increase <- omn_near_585_merged$mehg_ppb_near585 - omn_near_585_merged$mehg_ppb_present
omn_far585_increase <- omn_far_585_merged$mehg_ppb_far585 - omn_far_585_merged$mehg_ppb_present

omn_results <- data.frame(
  lat = omn_near_245_merged$lat,
  lon = omn_near_245_merged$lon,
  near245_increase = omn_near245_increase,
  far245_increase = omn_far245_increase,
  near585_increase = omn_near585_increase,
  far585_increase = omn_far585_increase
)

create_map <- function(pred_data, mehg_column, title) {
  pred_data <- pred_data %>% distinct(lat, lon, .data[[mehg_column]])
  
  China_map <- map_data('world', region = 'China')
  china <- sf::st_read('China_map/China_area.shp')
  china_area <- sf::st_read('China_map/Area.shp')
  nine <- sf::st_read('China_map/Nine_dash_line.shp')
  south_china_sea <- sf::st_read('China_map/South_China_Sea.shp')
  
  pred_data <- pred_data %>%
    mutate(
      Level = case_when(
        .data[[mehg_column]] <= 5 ~ 'Level_1',
        .data[[mehg_column]] > 5 & .data[[mehg_column]] <= 10 ~ "Level_2",
        .data[[mehg_column]] > 10 & .data[[mehg_column]] <= 20 ~ "Level_3",
        .data[[mehg_column]] > 20 & .data[[mehg_column]] <= 30 ~ "Level_4",
        .data[[mehg_column]] > 30 & .data[[mehg_column]] <= 40 ~ "Level_5",
        .data[[mehg_column]] > 40 & .data[[mehg_column]] <= 50 ~ "Level_6",
        .data[[mehg_column]] > 50 & .data[[mehg_column]] <= 100 ~ "Level_7",
        .data[[mehg_column]] > 100  ~ "Level_8",
        TRUE ~ "Other"
      )
    )
  
  main_map <- ggplot() +
    geom_tile(data = pred_data, aes(x = lon, y = lat, fill = Level)) +
    scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          legend.position = 'right') +
    labs(x = 'Longitude', y = 'latitude', 
         title = title) +
    geom_sf(data = china_area, aes(geometry = geometry), fill = NA, color = 'black', linewidth = 0.3) +
    geom_sf(data = nine, aes(geometry = geometry),color = 'black', linewidth = 0.3)+
    geom_sf(data = south_china_sea, aes(geometry = geometry), color = 'black', linewidth = 0.3)
  
  return(main_map)
  
}

maps <- list()
maps$near245 <- create_map(omn_results, "near245_increase", "Near Future SSP245")
maps$far245 <- create_map(omn_results, "far245_increase", "Far Future SSP245")
maps$near585 <- create_map(omn_results, "near585_increase", "Near Future SSP585")
maps$far585 <- create_map(omn_results, "far585_increase", "Far Future SSP585")

omnivore_map <- (maps$near245|maps$far245)/(maps$near585|maps$far585)
omnivore_map

#ggsave("Omnivore_map_0830.pdf", omnivore_map, width = 10, height = 10, units = "in")

# Piscivore & Omnivore present day map==========================================
piscivore_present <-distinct(piscivore_present, lat, lon, mehg_ppb)
omn_present <-distinct(omn_present, lat, lon, mehg_ppb)

China_map <- map_data('world', region = 'China')
china <- sf::st_read('China_map/China_area.shp')
china_area <- sf::st_read('China_map/Area.shp')
nine <- sf::st_read('China_map/Nine_dash_line.shp')
south_china_sea <- sf::st_read('China_map/South_China_Sea.shp')

piscivore_present <- piscivore_present %>%
  mutate(
    Level = case_when(
      piscivore_present$mehg_ppb >= 0.19 & piscivore_present$mehg_ppb <= 1.5 ~ 'Level_1',
      piscivore_present$mehg_ppb > 1.5 & piscivore_present$mehg_ppb <= 10 ~ "Level_2",
      piscivore_present$mehg_ppb > 10 & piscivore_present$mehg_ppb <= 20 ~ "Level_3",
      piscivore_present$mehg_ppb > 20 & piscivore_present$mehg_ppb <= 30 ~ "Level_4",
      piscivore_present$mehg_ppb > 30 & piscivore_present$mehg_ppb <= 50 ~ "Level_5",
      piscivore_present$mehg_ppb > 50 & piscivore_present$mehg_ppb <= 100 ~ "Level_6",
      piscivore_present$mehg_ppb > 100 & piscivore_present$mehg_ppb <= 120 ~ "Level_7",
      piscivore_present$mehg_ppb > 120  ~ "Level_8",
      TRUE ~ "Other"
    )
  )

omn_present <- omn_present %>%
  mutate(
    Level = case_when(
      omn_present$mehg_ppb >= 0.19 & omn_present$mehg_ppb <= 1.5 ~ 'Level_1',
      omn_present$mehg_ppb > 1.5 & omn_present$mehg_ppb <= 10 ~ "Level_2",
      omn_present$mehg_ppb > 10 & omn_present$mehg_ppb <= 20 ~ "Level_3",
      omn_present$mehg_ppb > 20 & omn_present$mehg_ppb <= 30 ~ "Level_4",
      omn_present$mehg_ppb > 30 & omn_present$mehg_ppb <= 50 ~ "Level_5",
      omn_present$mehg_ppb > 50 & omn_present$mehg_ppb <= 100 ~ "Level_6",
      omn_present$mehg_ppb > 100 & omn_present$mehg_ppb <= 120 ~ "Level_7",
      omn_present$mehg_ppb > 120  ~ "Level_8",
      TRUE ~ "Other"
    )
  )

piscivore_present_map <- ggplot() +
  geom_tile(data = piscivore_present, aes(x = lon, y = lat, fill = Level)) +
  scale_fill_brewer(palette = 'RdYlGn', direction = -1) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = 'right') +
  labs(x = 'Longitude', y = 'latitude', 
       title = "Piscivore, 5 kg") +
  geom_sf(data = china_area, aes(geometry = geometry), fill = NA, color = 'black', linewidth = 0.3) +
  geom_sf(data = nine, aes(geometry = geometry),color = 'black', linewidth = 0.5)+ 
  geom_sf(data = south_china_sea, aes(geometry = geometry), color = 'black', linewidth = 0.3)

omn_present_map <- ggplot() +
  geom_tile(data = omn_present, aes(x = lon, y = lat, fill = Level)) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = 'right') +
  labs(x = 'Longitude', y = 'latitude', 
       title = "Omnivore, 5 kg") +
  geom_sf(data = china_area, aes(geometry = geometry), fill = NA, color = 'black', linewidth = 0.3) +
  geom_sf(data = nine, aes(geometry = geometry),color = 'black', linewidth = 0.5)+ 
  geom_sf(data = south_china_sea, aes(geometry = geometry), color = 'black', linewidth = 0.3)

piscivore_omnivore_map <- piscivore_present_map/omn_present_map
piscivore_omnivore_map
#ggsave("Piscivore_omnivore_present_map.pdf", piscivore_omnivore_map, width = 10, height = 10, units = "in")

save.image("Piscivore_omnivore_present_future_map.RData")
