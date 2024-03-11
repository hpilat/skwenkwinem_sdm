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
# first, convert skeetch_vect_WGS84 from polygon to lines geometry:
skeetch_lines <- as.lines(skeetch_vectWGS84)

ggplot() +
  geom_spatraster(data = bioclim30s_present_continuous_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = "Id", colour = "white")) +
  scale_fill_continuous() 

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



# Get area of agreement between Informed and Bioclim30s (present) models:



# Informed
# Use binary prediction raster
plot(informed_present_binary)
summary(informed_present_binary)

# need to reclassify cells from presence to 1 and pseudoabsence to 0
# terra::classify requires us to provide a matrix of values from -> values to
# create a list so 1 (presence) stays as 1 and 2 (pseudoabsence) becomes 0
matrix_informed_cols <- c(1, 2, 1, 0)
matrix_informed <- matrix(matrix_informed_cols, ncol=2)
matrix_informed

# reclassify values to 1 (presence) and 0 (pseudoabsence)
informed_classified <- terra::classify(informed_present_binary, matrix_informed)
plot(informed_classified)

# Bioclim
# use binary prediction raster:
plot(bioclim30s_present_binary)

# reclassify cells, starting with creating matrix
# want values to go from 1 (presence) to 2 (distinguished from informed raster)
  # and 2 (pseudoabsences) to 0 (same as in informed raster)
matrix_bioclim_cols <- c(1, 2, 2, 0)
matrix_bioclim <- matrix(matrix_bioclim_cols, ncol=2)
matrix_bioclim

# classify binary raster according to matrix
bioclim_classified <- classify(bioclim30s_present_binary, matrix_bioclim)
plot(bioclim_classified)

# add informed_classified and bioclim_classified together
model_agreement <- (informed_classified + bioclim_classified)
plot(model_agreement)
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

# save to file
writeRaster(model_agreement, filename = "outputs/model_agreement.tif")

# calculate area of agreement:
# first need to project to Albers equal area projection
model_agreement_albers <- project(model_agreement, "EPSG:3005")

# filter out cells in agreement between models (value = 3)
model_agreement_filt <- model_agreement_albers %>% 
  dplyr::filter(binary_mean == 3)
plot(model_agreement_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_polygons <- as.polygons(model_agreement_filt)

# convert to sf object so we can calculate area:
model_agreement_sf <- st_as_sf(model_agreement_polygons)
crs(model_agreement_sf) # BC Albers

# calculate area
model_agreement_area <- st_area(model_agreement_sf) # 6.48e+11
# convert from m^2 to km^2
model_agreement_area <- units::set_units(st_area(model_agreement_sf), km^2) 
# 647 603 km^2 of suitable habitat



# Repeat above steps to get area of agreement between bioclim present and future predictions



# will reuse Bioclim30s present from above: bioclim_classified
# Bioclim30s Future:
# use binary prediction raster:
plot(bioclim30s_future_binary)

# reclassify cells, starting with creating matrix
# want values to go from 1 (presence) to 2 (distinguished from informed raster)
# and 2 (pseudoabsences) to 0 (same as in informed raster)
matrix_bioclim_fut_cols <- c(1, 2, 4, 0)
matrix_bioclim_fut <- matrix(matrix_bioclim_fut_cols, ncol=2)
matrix_bioclim_fut

# classify binary raster according to matrix
bioclim_classified_fut <- classify(bioclim30s_future_binary, matrix_bioclim_fut)
plot(bioclim_classified_fut)

# add informed_classified and bioclim_classified together
model_agreement_fut <- (bioclim_classified + bioclim_classified_fut)
plot(model_agreement_fut)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions

# save to file
writeRaster(model_agreement_fut, filename = "outputs/model_agreement_future.tif")

# calculate area of agreement:
# first need to project to Albers equal area projection
model_agreement_fut_albers <- project(model_agreement_fut, "EPSG:3005")

# filter out cells in agreement between models (value = 3)
model_agreement_fut_filt <- model_agreement_fut_albers %>% 
  dplyr::filter(binary_mean == 6)
plot(model_agreement_fut_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_fut_polygons <- as.polygons(model_agreement_fut_filt)

# convert to sf object so we can calculate area:
model_agreement_fut_sf <- st_as_sf(model_agreement_fut_polygons)
crs(model_agreement_fut_sf) # BC Albers

# calculate area
model_agreement_fut_area <- st_area(model_agreement_fut_sf) # 6.44e+11
# convert from m^2 to km^2
model_agreement_fut_area <- units::set_units(st_area(model_agreement_fut_sf), km^2) 
# 644 494 km^2 of suitable habitat



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
  dplyr::filter(.metric == "roc_auc") %>% 
  # drop .metric column
  dplyr::select("wflow_id", "mean", "std_err") %>% 
  # rename columns to be more informative
  dplyr::rename(algorithm = wflow_id) %>% 
  # add model column and input "informed" in the rows
  add_column(model = "informed", .before = "algorithm")

bioclim30s_ensemble_metrics_AUC <- bioclim30s_ensemble_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc") %>% 
  # drop .metric column
  dplyr::select("wflow_id", "mean", "std_err") %>% 
  # rename columns to be more informative
  dplyr::rename(algorithm = wflow_id) %>% 
  # add model column and input "informed" in the rows
  add_column(model = "bioclim30s", .before = "algorithm")

# now bind the rows together into 1 object:
ensemble_AUC <- rbind(informed_ensemble_metrics_AUC, bioclim30s_ensemble_metrics_AUC)

ggplot(ensemble_AUC, aes(x = "algorithm", y = "mean")) +
  geom_pointrange(aes(ymin = lower, ymax = upper))

# write to new csv to import into word
write.csv(informed_ensemble_metrics_AUC, file = "outputs/skwenkwinem_informed_ensemble_metrics_AUC.csv")
write.csv(bioclim30s_ensemble_metrics_AUC, file = "outputs/skwenkwinem_bioclim30s_ensemble_metrics_AUC.csv")



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
