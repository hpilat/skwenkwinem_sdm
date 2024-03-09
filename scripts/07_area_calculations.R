# This is script 07/07
# This script calculates suitable habitat in km^2 for our total study area
# and Skeetchestn Territory
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim_30s.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)

# Predictions:

# Continuous:
informed_present_continuous <- rast("outputs/ran_informed_prediction_present.tif")
bioclim30s_present_continuous <- rast("outputs/ran_bioclim30s_predict-present.tif")
bioclim30s_future_continuous <- rast("outputs/ran_predict_future_bioclim30s.tif")

# Binary:
informed_present_binary <- rast("outputs/ran_informed_prediction_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/ran_bioclim30s_predict-present-binary.tif")
bioclim30s_future_binary <- rast("outputs/ran_bioclim30s_predict-future-binary.tif")



# Extent objects:

# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")
# Skeetchestn territory boundary vector for masking:
skeetch_vect <- vect("data/extents/skeetch_vect_cropped_albers.shp")
# transform to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect, "EPSG:4326")



# Cropping Predictions to Skeetchestn Territory:

# Informed Prediction:
informed_prediction_present_skeetch <- crop(informed_present_continuous, skeetch_vectWGS84)
plot(informed_prediction_present_skeetch)

# plot with skeetch_vectWGS84 overlaid
plot(informed_prediction_present_skeetch)
lines(skeetch_vectWGS84)

# write to .tif file:
writeRaster(informed_prediction_present_skeetch, filename = "outputs/informed_prediction_present_skeetch.tif", overwrite = TRUE)

# Bioclim30s Present Prediction:
bioclim30s_present_continuous_skeetch <- crop(bioclim30s_present_continuous, skeetch_vectWGS84)
plot(bioclim30s_present_continuous_skeetch)
writeRaster(bioclim30s_present_continuous_skeetch, filename = "outputs/bioclim30s_present_continuous_skeetch.tif", overwrite = TRUE)

# plot with skeetch_vectWGS84 overlaid
plot(bioclim30s_present_continuous_skeetch)
lines(skeetch_vectWGS84)

# write to .tif file:
writeRaster(bioclim30s_present_continuous_skeetch, filename = "outputs/informed_prediction_present_skeetch.tif", overwrite = TRUE)


# Bioclim30s Future Prediction:
bioclim30s_future_continuous_skeetch <- crop(bioclim30s_future_continuous, skeetch_vectWGS84)
plot(bioclim30s_future_continuous_skeetch)

# plot with skeetch_vectWGS84 overlaid
plot(bioclim30s_future_continuous_skeetch)
lines(skeetch_vectWGS84)

# write to .tif file:
writeRaster(bioclim30s_future_continuous_skeetch, filename = "outputs/bioclim30s_future_continuous_skeetch.tif", overwrite = TRUE)



# Area Calculations


# Overall study extent:
# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
na_bound_albers <- st_transform(na_bound_sf, "EPSG:3005")
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_albers) # 3.83e+12 m^2
na_bound_area <- units::set_units(st_area(na_bound_albers), km^2) # 3 805 697  km^2


# Skeetchestn Territory:
skeetch_sf <- read_sf("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)
crs(skeetch_sf) # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) # 6996 km^2



## Informed Model (Present Projections Only)


# turn presence into polygon so we can calculate suitable area
# first need to transform CRS to Albers equal area projection
informed_present_binary_Albers <- project(informed_present_binary, "EPSG:3005")
# then need to filter out presence cells from raster
informed_present_presence <- informed_present_binary %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
informed_present_presence <- as.polygons(informed_present_presence)

# now turn polygons into sf object
informed_present_sf <- st_as_sf(informed_present_presence)

crs(informed_present_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
informed_present_area <- st_transform(informed_present_sf, "EPSG:3005")
informed_present_area <- st_area(informed_present_sf) # 1.48e+12 m^2
# convert from m^2 to km^2
informed_present_area <- units::set_units(st_area(informed_present_sf), km^2) 
# 936 476 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_present <- informed_present_area/na_bound_area # 24.6%


# Present Suitable Area for Skeetchestn, from Informed Model:


# crop results to Skeetchestn territory - convert CRS to Albers first?
informed_binary_skeetch <- crop(informed_present_binary, skeetch_vectWGS84)
informed_binary_skeetch <- mask(informed_binary_skeetch, skeetch_vectWGS84)
plot(informed_binary_skeetch)

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
informed_present_presence_skeetch <- informed_binary_skeetch %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
informed_present_presence_skeetch <- as.polygons(informed_present_presence_skeetch)

# now turn prediction_present_pres polygons into sf object
informed_present_skeetch_sf <- st_as_sf(informed_present_presence_skeetch)
crs(informed_present_skeetch_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
informed_present_skeetch_area <- st_transform(informed_present_skeetch_sf, "EPSG:3005")
informed_present_skeetch_area <- st_area(informed_present_skeetch_sf) # 1.98e+9 m^2
# convert from m^2 to km^2
informed_present_skeetch_area <- units::set_units(st_area(informed_present_skeetch_sf), km^2) 
# 1746 km^2 of suitable habitat


# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_informed_present_skeetch <- informed_present_skeetch_area/skeetch_area
# 25%



# Bioclim30s Present Area:



# turn presence into polygon so we can calculate suitable area
# first need to transform CRS to Albers equal area projection
bioclim30s_present_binary_Albers <- project(bioclim30s_present_binary, "EPSG:3005")
# then need to filter out presence cells from raster
bioclim30s_present_presence <- bioclim30s_present_binary %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
bioclim30s_present_presence <- as.polygons(bioclim30s_present_presence)

# now turn polygons into sf object
bioclim30s_present_sf <- st_as_sf(bioclim30s_present_presence)

crs(bioclim30s_present_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
bioclim30s_present_area <- st_transform(bioclim30s_present_sf, "EPSG:3005")
bioclim30s_present_area <- st_area(bioclim30s_present_sf) # 1.48e+12 m^2
# convert from m^2 to km^2
bioclim30s_present_area <- units::set_units(st_area(bioclim30s_present_sf), km^2) 
# 949 392 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_bioclim30s_present <- bioclim30s_present_area/na_bound_area
# 24.9%


# Present Suitable Area for Skeetchestn from Bioclim30s Model:


# crop results to Skeetchestn territory - convert CRS to Albers first?
bioclim30s_binary_skeetch <- crop(bioclim30s_present_binary, skeetch_vectWGS84)
bioclim30s_binary_skeetch <- mask(bioclim30s_binary_skeetch, skeetch_vectWGS84)
plot(bioclim30s_binary_skeetch)

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_present_presence_skeetch <- bioclim30s_binary_skeetch %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
bioclim30s_present_presence_skeetch <- as.polygons(bioclim30s_present_presence_skeetch)

# now turn prediction_present_pres polygons into sf object
bioclim30s_present_skeetch_sf <- st_as_sf(bioclim30s_present_presence_skeetch)
crs(bioclim30s_present_skeetch_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
bioclim30s_present_skeetch_area <- st_transform(bioclim30s_present_skeetch_sf, "EPSG:3005")
bioclim30s_present_skeetch_area <- st_area(bioclim30s_present_skeetch_sf) # 1.98e+9 m^2
# convert from m^2 to km^2
bioclim30s_present_skeetch_area <- units::set_units(st_area(bioclim30s_present_skeetch_sf), km^2) 
# 1685 km^2 of suitable habitat

# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_bioclim30s_present_skeetch <- bioclim30s_present_skeetch_area/skeetch_area
# 24.1%



# Bioclim30s Future Area:



# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_future_presence <- bioclim30s_future_binary %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
bioclim30s_future_presence <- as.polygons(bioclim30s_future_presence)

# now turn prediction_present_pres polygons into sf object
bioclim30s_future_sf <- st_as_sf(bioclim30s_future_presence)

crs(bioclim30s_future_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
bioclim30s_future_area <- st_transform(bioclim30s_future_sf, "EPSG:3005")
bioclim30s_future_area <- st_area(bioclim30s_future_sf) # 1.15e+12 m^2
# convert from m^2 to km^2
bioclim30s_future_area <- units::set_units(st_area(bioclim30s_future_sf), km^2) 
# 911 753 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_future <- bioclim30s_future_area/na_bound_area
# 24%

# now calculate difference between suitable habitat in the present and 2081-2100
# first need to convert area from class "units" to numeric
bioclim30s_present_area_num <- as.numeric(bioclim30s_present_area)
bioclim30s_future_area_num <- as.numeric(bioclim30s_future_area)
change_area_present_to_2100 <- bioclim30s_future_area_num - bioclim30s_present_area_num
# -37638.8290758764 km^2 change in suitable habitat

# percent change:
percent_change <- (change_area_present_to_2100/bioclim30s_future_area_num)* 100
# -4.1281... % decrease in suitable habitat?? That can't be right


# Future Suitable Area Calculations for Skeetchestn


# crop and mask total projection to Skeetchestn Territory
bioclim30s_future_binary_eqArea <- project(bioclim30s_future_binary, "EPSG:3005")
bioclim30s_future_binary_skeetch <- crop(bioclim30s_future_binary_eqArea, skeetch_vect)
bioclim30s_future_binary_skeetch <- mask(bioclim30s_future_binary_skeetch, skeetch_vect)

ggplot() +
  geom_spatraster(data = bioclim30s_future_binary_skeetch, aes(fill = binary_mean))
# extent is too small, need to fix

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_future_presence_skeetch <- bioclim30s_future_binary_skeetch %>% 
  filter(binary_mean == "presence")

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
bioclim30s_future_presence_skeetch <- as.polygons(bioclim30s_future_presence_skeetch)

# now turn prediction_future polygons into sf object
bioclim30s_future_skeetch_sf <- st_as_sf(bioclim30s_future_presence_skeetch)
crs(bioclim30s_future_skeetch_sf) # BC Albers

# calculate area
bioclim30s_future_skeetch_area <- st_area(bioclim30s_future_skeetch_sf) # 1.98e+9 m^2
# convert from m^2 to km^2
bioclim30s_future_skeetch_area <- units::set_units(st_area(bioclim30s_future_skeetch_sf), km^2) 
# 1589 km^2 of suitable habitat

# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_future_skeetch <- bioclim30s_future_skeetch_area/skeetch_area
# 22.7%

# area changed from present to future:
change_area_skeetch <- bioclim30s_future_skeetch_area - bioclim30s_present_skeetch_area
# -95.9