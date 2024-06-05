# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 07/11
# This script plots our ensemble metrics from both models together
  # and our continuous habitat suitability predictions
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)

# Predictions:

# Continuous prediction rasters:
informed_present_continuous <- rast("outputs/skwenkwinem_informed_predict_present_cont.tif")
bioclim30s_present_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_present_cont.tif")
bioclim30s_future_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_future_cont_585.tif")

# reproject to North America Albers equal-area conic
# https://spatialreference.org/ref/esri/102008/
# define CRS
new_crs <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

# reproject continuous rasters to new CRS
informed_present_continuous <- terra::project(informed_present_continuous, new_crs, method = "near")
bioclim30s_present_continuous <- terra::project(bioclim30s_present_continuous, new_crs, method = "near")
bioclim30s_future_continuous <- terra::project(bioclim30s_future_continuous, new_crs, method = "near")

# Extent objects:

# total study area boundary
na_bound_vect <- vect("data/extents/na_bound_vect.shp") # WGS84
# project na_bound_vect to new CRS:
na_bound_vect <- terra::project(na_bound_vect, new_crs) 
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")

# Skeetchestn territory boundary vector for masking:
skeetch_vect <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject:
skeetch_vect <- terra::project(skeetch_vect, new_crs)
# turn Skeetchestn boundary polygon into lines geometry:
skeetch_lines <- as.lines(skeetch_vect)

# create an extent object slightly larger than skeetch_vect
skeetch_vect # round up extent values:
skeetch_extent <- ext(-121.6, -120.1, 50.3, 51.6)



# crop continuous rasters to na_bound_vect
informed_present_continuous <- terra::crop(informed_present_continuous, na_bound_vect)
bioclim30s_present_continuous <- terra::crop(bioclim30s_present_continuous, na_bound_vect)
bioclim30s_future_continuous <- terra::crop(bioclim30s_future_continuous, na_bound_vect)




# Plotting for entire study area:

# Informed Model:
informed_full_extent_cont <- ggplot() +
  geom_spatraster(data = informed_present_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability", na.value = "transparent", limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 2.5)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("30", "35", "40", "45", "50", "55", "60"), 
                     expand = c(0, 0)) +
  labs(title = "Present habitat suitability", 
       subtitle = "Informed model")

informed_full_extent_cont

ggsave("outputs/informed_full_extent_cont.png", plot = informed_full_extent_cont)


# Bioclim30s Present Model:
bioclim_pres_full_extent_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_present_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability", na.value = "transparent", limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 2.5)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("30", "35", "40", "45", "50", "55", "60"), 
                     expand = c(0, 0))
  labs(title = "Present habitat suitability", 
       subtitle = "Bioclim model")

bioclim_pres_full_extent_cont

ggsave("outputs/bioclim_pres_full_extent_cont.png", plot = bioclim_pres_full_extent_cont)


# Bioclim30s Future Model:
bioclim_fut_full_extent_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_future_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability", na.value = "transparent", limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 2.5)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("30", "35", "40", "45", "50", "55", "60"), 
                     expand = c(0, 0))
  labs(title = "Future habitat suitability", 
       subtitle = "Bioclim model")

bioclim_fut_full_extent_cont

ggsave("outputs/bioclim_fut_full_extent_cont_585.png", plot = bioclim_fut_full_extent_cont)



# Plot continuous rasters together for full study extent:



# create new rasters so we don't overwrite the originals
informed_present_continuous_temp <- informed_present_continuous
bioclim_present_continuous_temp <- bioclim30s_present_continuous
bioclim_future_continuous_temp <- bioclim30s_future_continuous

# need to change layer names
names(informed_present_continuous_temp) <- "Informed present"
names(bioclim_present_continuous_temp) <- "Bioclim present"
names(bioclim_future_continuous_temp) <- "Bioclim future"

# create a multilayer raster:
continuous_predictions <- c(informed_present_continuous_temp, 
                            bioclim_present_continuous_temp,
                            bioclim_future_continuous_temp)
continuous_predictions

predictions_continuous_plot <- ggplot() +
  geom_spatraster(data = continuous_predictions) +
  facet_wrap(~lyr, nrow = 1, ncol = 3) +
  geom_spatvector(data = skeetch_lines, colour = "white") +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank(), 
        strip.text = element_text(size = 12, vjust = +0.5)) +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability",
                       na.value = "transparent",
                       limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 3.0, size = 10)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("30", "35", "40", "45", "50", "55", "60"), 
                     expand = c(0, 0)) +
  theme(title = element_text(size = 10), 
        panel.spacing = unit(20, "pt"))

predictions_continuous_plot

ggsave("outputs/full_extent_cont_plots_585.png", predictions_continuous_plot, 
       width = 8, height = 4, units = "in")



# Skeetchestn Territory Plots:

# use UTM projection:
informed_present_UTM <- terra::project(informed_present_continuous, "EPSG:32610")
bioclim30s_present_UTM <- terra::project(bioclim30s_present_continuous, "EPSG:32610")
bioclim30s_future_UTM <- terra::project(bioclim30s_future_continuous, "EPSG:32610")
skeetch_extent_UTM <- terra::project(skeetch_extent, from = "EPSG:4326", to = "EPSG:32610")
skeetch_lines_UTM <- terra::project(skeetch_lines, "EPSG:32610")

# Crop predictions to Skeetchestn Territory:
informed_present_skeetch <- crop(informed_present_UTM, skeetch_extent_UTM)
bioclim30s_present_skeetch <- crop(bioclim30s_present_UTM, skeetch_extent_UTM)
bioclim30s_future_skeetch <- crop(bioclim30s_future_UTM, skeetch_extent_UTM)


# Plot continuous prediction from informed model for Skeetchestn Territory:

skeetch_informed_cont <- ggplot() +
  geom_spatraster(data = informed_present_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines_UTM, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability", na.value = "transparent", limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 2.5)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                     expand = c(0, 0)) +
  labs(title = "Present habitat suitability", 
       subtitle = "Informed model")

skeetch_informed_cont

ggsave("outputs/skeetch_informed_cont.png", plot = skeetch_informed_cont)



# plot continuous prediction from bioclim30s present model for Skeetchestn:

skeetch_bioclim_present_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_present_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Probability of Presence") +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                     expand = c(0, 0)) +
  labs(title = "Present Habitat Suitability", 
       subtitle = "Bioclim30s Model")

skeetch_bioclim_present_cont

ggsave("outputs/skeetch_bioclim_present_cont.png", plot = skeetch_bioclim_present_cont)


# plot continuous prediction from bioclim30s future model for Skeetchestn:

skeetch_bioclim_future_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_future_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Relative \nHabitat \nSuitability", na.value = "white") +
  theme(legend.title = element_text(size = 10)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                     expand = c(0, 0)) +
  labs(title = "Future Habitat Suitability", 
       subtitle = "Bioclim30s Model")

skeetch_bioclim_future_cont

ggsave("outputs/skeetch_bioclim_future_cont_585.png", plot = skeetch_bioclim_future_cont)



# Plot the Skeetchestn predictions together:

# First create temp rasters so we don't overwrite the originals:
informed_present_skeetch_temp <- informed_present_skeetch
bioclim_present_skeetch_temp <- bioclim30s_present_skeetch
bioclim_future_skeetch_temp <- bioclim30s_future_skeetch


# change names of our raster layers:
names(informed_present_skeetch_temp) <- "Informed Present"
names(bioclim_present_skeetch_temp) <- "Bioclim Present"
names(bioclim_future_skeetch_temp) <- "Bioclim Future"

# create a multilayer raster:
predictions_cont_skeetch <- c(informed_present_skeetch_temp, 
                              bioclim_present_skeetch_temp, 
                              bioclim_future_skeetch_temp)

predictions_cont_skeetch_plot <- ggplot() +
  geom_spatraster(data = predictions_cont_skeetch) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  facet_wrap(~lyr, nrow = 1, ncol = 3) +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank(), 
        strip.text = element_text(size = 14, vjust = +0.5)) +
  scale_fill_viridis_c(name = "Relative \nhabitat\nsuitability",
                       na.value = "transparent",
                       limits = c(0, 1.0)) +
  guides(fill = guide_colorbar(ticks.colour = NA)) +
  theme(legend.title = element_text(vjust = + 3.0, size = 10)) +
  scale_x_continuous(name = "Longitude (°W)",
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(title = element_text(size = 10)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                     expand = c(0, 0)) 
  

predictions_cont_skeetch_plot

ggsave("outputs/skeetch_cont_plots_585.png", predictions_cont_skeetch_plot, 
       width = 12, height = 4, units = "in")