# This is script 09/10 (?)
# This script calculates area predicted as suitable by our models
  # over our entire study extent

library(sf)
library(terra)
library(tidyverse)
library(tidyterra)


# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp") # WGS84
# reproject to Albers equal area:
na_bound_albers <- project(na_bound_vect, "EPSG:3005")

# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp") #WGS84
na_bound_sf_albers <- st_transform(na_bound_sf, "EPSG:3005")

# Skeetchestn territory boundary vector for masking:
skeetch_vect_albers <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# tranform to sf object:
skeetch_sf_albers <- st_as_sf(skeetch_vect_albers)
# transform to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect_albers, "EPSG:4326")


# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extentWGS84 <- ext(-121.6, -120.1, 50.3, 51.6) # xmin, xmax, ymin, ymax
# reproject to Albers equal area:
skeetch_extent_albers <- project(skeetch_extentWGS84, from = "EPSG:4326", to = "EPSG:3005")




# Read in Binary Predictions:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")
# reproject to Albers equal area:
informed_present_binary <- project(informed_present_binary, "EPSG:3005")
bioclim30s_present_binary <- project(bioclim30s_present_binary, "EPSG:3005")
bioclim30s_future_binary <- project(bioclim30s_future_binary, "EPSG:3005")

summary(informed_present_binary) # 62469 NA values
summary(bioclim30s_present_binary) # 61 236 NA values
summary(bioclim30s_future_binary) # 61 236 NA values

# Area of overall study extent:
# calculate study area, in m^2 (default)
na_bound_vect <- project(na_bound_vect, "EPSG:3005")
na_bound_poly <- as.polygons(na_bound_vect)
na_bound_sf_albers <- st_as_sf(na_bound_poly)
na_bound_area <- st_area(na_bound_sf_albers) # 4.18e+12 m^2
na_bound_area <- units::set_units(st_area(na_bound_sf_albers), km^2) # 4 183 596  km^2


# Area of Skeetchestn Territory:
plot(skeetch_sf_albers)
skeetch_sf_albers # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf_albers) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf_albers), km^2) # 6996 km^2



# Informed Prediction:
informed_present_binary
plot(informed_present_binary)

# filter out cells classified as "presence"
informed_presence_filt <- informed_present_binary %>% 
  dplyr::filter(binary_mean == "presence")
plot(informed_presence_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_presence_polygons <- as.polygons(informed_presence_filt)

# convert to sf object so we can calculate area:
informed_presence_sf <- st_as_sf(informed_presence_polygons)
informed_presence_sf

# calculate area:
informed_presence_area <- st_area(informed_presence_sf) # 9.59e+11
# convert from m^2 to km^2
informed_presence_area <- units::set_units(st_area(informed_presence_sf), km^2) 
# 958 665 km^2 of suitable habitat

informed_present_binary
# filter out cells classified as "pseudoabs"
informed_pseudo_filt <- informed_present_binary %>% 
  dplyr::filter(binary_mean == "pseudoabs")
plot(informed_pseudo_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_pseudo_polygons <- as.polygons(informed_pseudo_filt)

# convert to sf object so we can calculate area:
informed_pseudo_sf <- st_as_sf(informed_pseudo_polygons)
informed_pseudo_sf

# calculate area:
informed_pseudo_area <- st_area(informed_pseudo_sf) # 2.94e+12
# convert from m^2 to km^2
informed_pseudo_area <- units::set_units(st_area(informed_pseudo_sf), km^2) 
# 2 935 413 km^2 of suitable habitat


# Bioclim30s present prediction:
bioclim30s_present_binary
plot(bioclim30s_present_binary)

# filter out cells classified as "presence"
bioclim_present_filt <- bioclim30s_present_binary %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_present_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_present_polygons <- as.polygons(bioclim_present_filt)

# convert to sf object so we can calculate area:
bioclim_present_sf <- st_as_sf(bioclim_present_polygons)
bioclim_present_sf

# calculate area:
bioclim_present_area <- st_area(bioclim_present_sf) # 1.02e+12
# convert from m^2 to km^2
bioclim_present_area <- units::set_units(st_area(bioclim_present_sf), km^2) 
# 1 017 274 km^2 of suitable habitat

# get the intersection of the informed and bioclim predicted suitable habitat:
model_agreement_present <- terra::intersect(informed_presence_filt, bioclim_present_filt)
model_agreement_present
plot(model_agreement_present)

# filter out cells classified as "TRUE" (where the models intersect)
agreement_present_filt <- model_agreement_present %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_present_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_present_polygons <- as.polygons(agreement_present_filt)

# convert to sf object so we can calculate area:
agreement_present_sf <- st_as_sf(agreement_present_polygons)
agreement_present_sf

# calculate area:
agreement_present_area <- st_area(agreement_present_sf) # 7.32e+11
# convert from m^2 to km^2
agreement_present_area <- units::set_units(st_area(agreement_present_sf), km^2) 
# 731 673 km^2 of suitable habitat


# calculate difference in suitable area from present to future:
difference_area_pres <- bioclim_present_area - informed_presence_area
# 58 609 km^2 more predicted by bioclim model than informed model



# Bioclim30s Future Prediction:

bioclim30s_future_binary

# filter out cells classified as "presence"
bioclim_future_filt <- bioclim30s_future_binary %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_future_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_future_polygons <- as.polygons(bioclim_future_filt)

# convert to sf object so we can calculate area:
bioclim_future_sf <- st_as_sf(bioclim_future_polygons)
bioclim_future_sf

# calculate area:
bioclim_future_area <- st_area(bioclim_future_sf) # 8.94e+11
# convert from m^2 to km^2
bioclim_future_area <- units::set_units(st_area(bioclim_future_sf), km^2) 
# 893 504 km^2 of suitable habitat


# get the intersection of the informed and bioclim predicted suitable habitat:
model_agreement_future <- terra::intersect(bioclim_present_filt, bioclim_future_filt)
model_agreement_future
plot(model_agreement_future)

# filter out cells classified as "TRUE" (where the models intersect)
agreement_future_filt <- model_agreement_future %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_future_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_future_polygons <- as.polygons(agreement_future_filt)

# convert to sf object so we can calculate area:
agreement_future_sf <- st_as_sf(agreement_future_polygons)
agreement_future_sf

# calculate area:
agreement_future_area <- st_area(agreement_future_sf) # 7.21e+11
# convert from m^2 to km^2
agreement_future_area <- units::set_units(st_area(agreement_future_sf), km^2) 
# 720 600 km^2 of suitable habitat


# calculate difference in suitable area from present to future:
difference_area_fut <- bioclim_future_area - bioclim_present_area
# -123 770 km^2



# Total study area:


# convert to sf object:
na_bound_sf <- st_as_sf(na_bound_vect)

# reproject to Albers equal area
na_bound_albers <- st_transform(na_bound_sf, "EPSG:3005")
na_bound_albers

# calculate area:
na_bound_area <- st_area(na_bound_sf) # 4.15e+12
# convert from m^2 to km^2
na_bound_area <- units::set_units(st_area(na_bound_sf), km^2) 
# 4 149 199 km^2 of suitable habitat

# proportion of total area predicted suitable by informed model:
proportion_informed <- informed_presence_area / na_bound_area
# 0.231

# proportion of total area predicted suitable by bioclim present model:
proportion_bioclim_present <- bioclim_present_area / na_bound_area
# 0.245

# proportion of total area predicted suitable by bioclim future model:
proportion_bioclim_future <- bioclim_future_area / na_bound_area
# 0.215

# proportion of total area predicted suitable by both 
  # informed and bioclim present models (agreement area)
proportion_agree_present <- agreement_present_area / na_bound_area
# 0.176

# proportion of total area predicted suitable by both bioclim 
  # present and future models (agreement area)
proportion_agree_future <- agreement_future_area / na_bound_area
# 0.174

# calculate percent overlap of total suitable area for 
# informed and bioclim present models
overlap_informed_bioclim <- agreement_present_area / (informed_presence_area + bioclim_present_area - agreement_present_area)
# 0.37 without subtracting agreement area in denominator
# 0.588 when subtracting agreement area in denominator

# calculate percent overlap of total suitable area for 
# bioclim present and future models
overlap_bioclim_pres_fut <- agreement_future_area / (bioclim_present_area + bioclim_future_area - agreement_future_area)
# 0.377 without subtracting agreement area in denominator
# 0.605 when subtracting agreement area in denominator