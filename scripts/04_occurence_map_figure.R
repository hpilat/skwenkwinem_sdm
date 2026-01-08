# Author: Hannah Pilat, Jason Pither
# Date: April 12th, 2024
# Updated by Jason Pither on May 21, 2025

# This is script 04/12

# This script produces the Map figure 
# Please first run these scripts in the following order:
# 01_data_download.R
# 02_continental_divide.R
# 03_cropped_extent.R

library(here)
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(geodata)

# Download and load GADM admin1 boundaries for each country, SpatVector format
canada_bound <- geodata::gadm(country = "CAN", level = 1, path = here::here("data", "raw"))
usa_bound    <- geodata::gadm(country = "USA", level = 1, path = here::here("data", "raw"))
mexico_bound <- geodata::gadm(country = "MEX", level = 1, path = here::here("data", "raw"))

# If needed, ensure they are SpatVector objects
#canada_bound <- vect(canada_bound)
#usa_bound    <- vect(usa_bound)
#mexico_bound <- vect(mexico_bound)

# Filter USA to include:
# - All states except Hawaii, Puerto Rico, and minor territories
# - Retain Alaska and the Lower 48
usa_filtered <- usa_bound[!usa_bound$NAME_1 %in% c(
  "Hawaii", "Puerto Rico", "Guam", "American Samoa",
  "Northern Mariana Islands", "United States Virgin Islands"
), ]

# crop out anything south of 24 degrees North in the USA,
# and Aleutian islands
usa_cont_extent <- ext(-170, -65, 24, 75)
usa_contiguous <- crop(usa_filtered, usa_cont_extent)

# plot(usa_contiguous)

# merge individual country polygons:
north_america <- c(canada_bound, usa_contiguous, mexico_bound)
north_america <- vect(north_america)

# reproject to North America Albers equal-area conic
# https://spatialreference.org/ref/esri/102008/
# define CRS
new_crs <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
north_america <- terra::project(north_america, new_crs)
plot(north_america)

# import other geospatial layers
#north_america <- terra::vect(here::here("data", "extents", "north_america.shp"))
na_bound_vect <- terra::vect(here::here("data", "extents", "na_bound_vect.shp"))
skwenkwinem_vect <- terra::vect(here::here("data", "processed", "skwenkwinem_sf.shp"))

# reproject objects to conic equal area projection:
na_bound_vect <- terra::project(na_bound_vect, new_crs)
skwenkwinem_vect <- terra::project(skwenkwinem_vect, new_crs)
skwenkwinem_vect <- crop(skwenkwinem_vect, na_bound_vect)
# plot(skwenkwinem_vect)

# create a dataframe containing a coordinate for the Skeetchestn band office
# 50.83951982786047, -120.95445365748702
skeetch_coord <- data.frame(lat = 50.83951982786047, lon = -120.95445365748702)
skeetch_coord_vect <- terra::vect(skeetch_coord, crs = "EPSG:4326")

north_america_plot <- ggplot() +
  tidyterra::geom_spatvector(data = north_america, aes(fill = NULL), show.legend = FALSE) +
  tidyterra::geom_spatvector(data = na_bound_vect, aes(alpha = 0.5), fill = "lightgreen", show.legend = FALSE) +
  tidyterra::geom_spatvector(data = skwenkwinem_vect, alpha = 0.25, cex = 0.75) +
  geom_spatvector(data = skeetch_coord_vect, color = "white", size = 1.75, shape = 17) +
  theme_classic() +
  scale_x_continuous(name = "Longitude (°W)",
                     expand = c(0,0)) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_y_continuous(name = "Latitude (°N)",
                     expand = c(0, 0)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank())

north_america_plot

ggsave(filename = here::here("outputs", "north_america_context_plot.png"), north_america_plot)
