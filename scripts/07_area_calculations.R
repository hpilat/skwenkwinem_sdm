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
library(devtools)
# devtools::install_github("h-a-graham/rayvista", dependencies = TRUE)

# Predictions:

# Continuous:
informed_present_continuous <- rast("outputs/skwenkwinem_informed_predict_present_cont.tif")
bioclim30s_present_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_present_cont.tif")
bioclim30s_future_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_future_cont.tif")

# Binary:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")



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
writeRaster(informed_prediction_present_skeetch, filename = "outputs/skwenkwinem_informed_predict_present_cont_skeetch.tif", overwrite = TRUE)

# Bioclim30s Present Prediction:
bioclim30s_present_continuous_skeetch <- crop(bioclim30s_present_continuous, skeetch_vectWGS84)
plot(bioclim30s_present_continuous_skeetch)
writeRaster(bioclim30s_present_continuous_skeetch, filename = "outputs/skwenkwinem_bioclim30s_present__skeetch.tif", overwrite = TRUE)

# plot with skeetch_vectWGS84 overlaid
plot(bioclim30s_present_continuous_skeetch)
lines(skeetch_vectWGS84)

# write to .tif file:
writeRaster(bioclim30s_present_continuous_skeetch, filename = "outputs/skwenkwinem_informed_predict_present_cont_skeetch.tif", overwrite = TRUE)


# Bioclim30s Future Prediction:
bioclim30s_future_continuous_skeetch <- crop(bioclim30s_future_continuous, skeetch_vectWGS84)
plot(bioclim30s_future_continuous_skeetch)

# plot with skeetch_vectWGS84 overlaid
plot(bioclim30s_future_continuous_skeetch)
lines(skeetch_vectWGS84)

# write to .tif file:
writeRaster(bioclim30s_future_continuous_skeetch, filename = "outputs/skwenkwinem_bioclim30s_predict_future_cont_skeetch.tif", overwrite = TRUE)



# Area Calculations



# Overall study extent:
# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
na_bound_albers <- st_transform(na_bound_sf, "EPSG:3005")
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_albers)
na_bound_area <- units::set_units(st_area(na_bound_albers), km^2) # 4 183 596  km^2


# Skeetchestn Territory:
skeetch_sf <- read_sf("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)
crs(skeetch_sf) # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) # 6996 km^2



# Present Suitable Area from Informed Model:



# turn presence into polygon so we can calculate suitable area
# first need to transform CRS to Albers equal area projection
informed_present_binary_Albers <- project(informed_present_binary, "EPSG:3005")
# then need to filter out presence cells from raster
informed_present_presence <- informed_present_binary %>% 
  dplyr::filter(binary_mean == "presence")

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
# 956 602 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_informed_present <- informed_present_area/na_bound_area # 22.9%



# Present Suitable area for Skeetchestn Territory from Informed Model:



# crop results to Skeetchestn territory
informed_binary_skeetch <- crop(informed_present_binary, skeetch_vectWGS84)
informed_binary_skeetch <- mask(informed_binary_skeetch, skeetch_vectWGS84)
plot(informed_binary_skeetch)

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
informed_present_presence_skeetch <- informed_binary_skeetch %>% 
  dplyr::filter(binary_mean == "presence")

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
# 4128 km^2 of suitable habitat


# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_informed_present_skeetch <- informed_present_skeetch_area/skeetch_area
# 59%



# Present Suitable Area from Bioclim30s Model:



# turn presence into polygon so we can calculate suitable area
# first need to transform CRS to Albers equal area projection
bioclim30s_present_binary_Albers <- project(bioclim30s_present_binary, "EPSG:3005")
# then need to filter out presence cells from raster
bioclim30s_present_presence <- bioclim30s_present_binary %>% 
  dplyr::filter(binary_mean == "presence")

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
# 1 014 711 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_bioclim30s_present <- bioclim30s_present_area/na_bound_area
# 24.3%



# Present Suitable Area for Skeetchestn Territory from Bioclim30s Model:



# crop results to Skeetchestn territory - convert CRS to Albers first?
bioclim30s_binary_skeetch <- crop(bioclim30s_present_binary, skeetch_vectWGS84)
bioclim30s_binary_skeetch <- mask(bioclim30s_binary_skeetch, skeetch_vectWGS84)
plot(bioclim30s_binary_skeetch)

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_present_presence_skeetch <- bioclim30s_binary_skeetch %>% 
  dplyr::filter(binary_mean == "presence")

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
# 3169 km^2 of suitable habitat

# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_bioclim30s_present_skeetch <- bioclim30s_present_skeetch_area/skeetch_area
# 45.3%



# Future Suitable Area from Bioclim30s Model:



# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_future_presence <- bioclim30s_future_binary %>% 
  dplyr::filter(binary_mean == "presence")

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
# 891 433 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_future <- bioclim30s_future_area/na_bound_area
# 21.3%

# now calculate difference between suitable habitat in the present and 2081-2100
# first need to convert area from class "units" to numeric
bioclim30s_present_area_num <- as.numeric(bioclim30s_present_area)
bioclim30s_future_area_num <- as.numeric(bioclim30s_future_area)
change_area_present_to_2100 <- bioclim30s_future_area_num - bioclim30s_present_area_num
# -123 277.849540642 km^2 change in suitable habitat

# percent change:
percent_change_total_area <- proportion_suitable_future - proportion_suitable_bioclim30s_present
# -4.1281... % decrease in suitable habitat?? That can't be right



# Future Suitable Area in Skeetchestn Territory from Bioclim30s Model:



# crop and mask total projection to Skeetchestn Territory
bioclim30s_future_binary_eqArea <- project(bioclim30s_future_binary, "EPSG:3005")
bioclim30s_future_binary_skeetch <- crop(bioclim30s_future_binary_eqArea, skeetch_vect)
bioclim30s_future_binary_skeetch <- mask(bioclim30s_future_binary_skeetch, skeetch_vect)

ggplot() +
  geom_spatraster(data = bioclim30s_future_binary_skeetch, aes(fill = binary_mean))

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
bioclim30s_future_presence_skeetch <- bioclim30s_future_binary_skeetch %>% 
  dplyr::filter(binary_mean == "presence")

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
# 1422 km^2 of suitable habitat

# proportion of suitable area relative to Skeetchestn Territory:
proportion_suitable_future_skeetch <- bioclim30s_future_skeetch_area/skeetch_area
# 20.3%

# area changed from present to future:
# now calculate difference between suitable habitat in the present and 2081-2100
# first need to convert area from class "units" to numeric
bioclim30s_present_skeetch_num <- as.numeric(bioclim30s_present_skeetch_area)
bioclim30s_future_skeetch_num <- as.numeric(bioclim30s_future_skeetch_area)
change_skeetch_present_to_2100 <- bioclim30s_future_skeetch_num - bioclim30s_present_skeetch_num
#  km^2 change in suitable habitat

# percent change in suitable habitat from present to future:
percent_change_skeetch <- proportion_suitable_future_skeetch - proportion_suitable_bioclim30s_present_skeetch
# -25%



# Isolate presence cells from the binary maps:

# Informed Prediction:

informed_present_cells <- informed_present_binary %>% 
  dplyr::filter(binary_mean == "presence")
# need to re-name layer so it's distinguished from bioclim30s layer:
names(informed_present_cells) <- "Informed_Only"
# change values
informed_present_cells[[informed_present_cells == "presence"]] <- "Informed_Only"

# Bioclim30s Prediction:
bioclim30s_present_cells <- bioclim30s_present_binary %>% 
  dplyr::filter(binary_mean == "presence")
# need to re-name layer so it's distinguished from informed layer:
names(bioclim30s_present_cells) <- "Bioclim_Only"


# Area of agreement (same cells classified as present in both informed and bioclim predictions)
agreed_present_cells <- informed_present_cells == bioclim30s_present_cells
# change layer name:
names(agreed_present_cells) <- "Model_Overlap"

# Try terra::intersect
model_intersection <- terra::intersect(informed_present_cells, bioclim30s_present_cells)
names(model_intersection) <- "Model_Intersection"

# merge rasters into one:
agreement_rast <- c(informed_present_cells, bioclim30s_present_cells, agreed_present_cells)


# need to distinguish presence values between informed and bioclim30s layers
# convert cell values from "presence" to 1 for informed model
informed_present_cells_num <-as.numeric(informed_present_cells)

plot(informed_present_cells_num)
plot(bioclim30s_present_cells, col = "purple", background = "transparent", alpha = 0.75)


ggplot() +
  geom_spatraster(data = agreed_present_cells, aes(fill = Model_Overlap)) +
  geom_spatraster(data = informed_present_cells_num, aes(fill = Informed_Only)) +
  geom_spatraster(data = bioclim30s_present_cells, aes (fill = Bioclim_Only))




# plot Skeetchestn prediction with 3D elevation
elevation <- rast("data/processed/elevation.tif")

# Calculate hillshade
slopes <- terra::terrain(elevation, "slope", unit = "radians")
aspect <- terra::terrain(elevation, "aspect", unit = "radians")
hillshade <- terra::shade(slopes, aspect)

# Plot hillshading as a basemap:
# Use Skeetchestn Territory as x and y limits:
terra::plot(hillshade, col = gray(0:100 / 100), legend = FALSE, axes = FALSE, add = TRUE)
# overlay with elevation:
terra::contour(elevation, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
# add contour lines:
terra::plot(hillshade, col = "gray40", add = TRUE)




# Isolate AUC metrics from model metrics:

# read in model metrics csv files:
informed_model_metrics <- read.csv("outputs/skwenkwinem_informed_model_metrics.csv", header = TRUE)
bioclim30s_model_metrics <- read.csv("outputs/skwenkwinem_bioclim30s_model_metrics.csv", header = TRUE)

# select only relevant columns and filter out all rows except roc_auc
informed_model_metrics_AUC <- informed_model_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

bioclim30s_model_metrics_AUC <- bioclim30s_model_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

# write to new csv to import into word
write.csv(informed_model_metrics_AUC, file = "outputs/skwenkwinem_informed_model_metrics_AUC.csv")
write.csv(bioclim30s_model_metrics_AUC, file = "outputs/skwenkwinem_bioclim30s_model_metrics_AUC.csv")


# repeat above steps for ensemble metrics:
informed_ensemble_metrics <- read.csv("outputs/skwenkwinem_informed_ensemble_metrics.csv", header = TRUE)
bioclim30s_ensemble_metrics <- read.csv("outputs/skwenkwinem_bioclim30s_ensemble_metrics.csv", header = TRUE)

# select only relevant columns and filter out all rows except roc_auc
informed_ensemble_metrics_AUC <- informed_ensemble_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

bioclim30s_ensemble_metrics_AUC <- bioclim30s_ensemble_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

# write to new csv to import into word
write.csv(informed_ensemble_metrics_AUC, file = "outputs/skwenkwinem_informed_ensemble_metrics_AUC.csv")
write.csv(bioclim30s_ensemble_metrics_AUC, file = "outputs/skwenkwinem_bioclim30s_ensemble_metrics_AUC.csv")
