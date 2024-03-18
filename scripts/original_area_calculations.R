# This is script 08/08
# This script isolates area of agreement between our predictions for our total
# study area and Skeetchestn Territory and plots them
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R
# 07_continuous_plots.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)

# Read in Binary Predictions:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")
# reproject to Albers equal area:
informed_present_binary <- project(informed_present_binary, "EPSG:3005")
bioclim30s_present_binary <- project(bioclim30s_present_binary, "EPSG:3005")
bioclim30s_future_binary <- project(bioclim30s_future_binary, "EPSG:3005")



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



# Area of overall study extent:
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_sf_albers) # 4.18e+12 m^2
na_bound_area <- units::set_units(st_area(na_bound_sf_albers), km^2) # 4 183 596  km^2


# Area of Skeetchestn Territory:
plot(skeetch_sf_albers)
skeetch_sf_albers # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf_albers) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf_albers), km^2) # 6996 km^2



# Get area of agreement between Informed and Bioclim30s (present) models:



# Full study extent:
# Informed
# Use binary prediction raster
plot(informed_present_binary)
summary(informed_present_binary)

# need to reclassify cells from presence to 1 and pseudoabsence to 0
# terra::classify requires us to provide a matrix of values from -> values to
# create a list so 1 (presence) stays as 1 and 2 (pseudoabsence) becomes 0
matrix_informed_cols <- c(1, 2, 1, 0)
matrix_informed <- matrix(matrix_informed_cols, ncol = 2)
matrix_informed

# reclassify values to 1 (presence) and 0 (pseudoabsence)
informed_classified <- terra::classify(informed_present_binary, matrix_informed)
plot(informed_classified)



# calculate area predicted to be suitable by informed model:
# mask to our study area boundary polygon
informed_classified_masked <- mask(informed_classified, na_bound_albers)


# filter out cells classified as informed (1), removes pseudoabsences (0)
informed_classified_filt <- informed_classified_masked %>% 
  dplyr::filter(binary_mean == 1)
plot(informed_classified_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_classified_polygons <- as.polygons(informed_classified_filt)

# convert to sf object so we can calculate area:
informed_classified_sf <- st_as_sf(informed_classified_polygons)
informed_classified_sf

# calculate area:
informed_classified_area <- st_area(informed_classified_sf) # 9.59e+11
# convert from m^2 to km^2
informed_classified_area <- units::set_units(st_area(informed_classified_sf), km^2) 
# 958 544 km^2 of suitable habitat


# Calculate area predicted to be suitable by the Bioclim (present) model:
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


# calculate area predicted to be suitable by bioclim model:
# mask to our study extent:
bioclim_classified_masked <- mask(bioclim_classified, na_bound_albers)

# filter out cells classified as bioclim (2), removes pseudoabsences (0)
bioclim_classified_filt <- bioclim_classified_masked %>% 
  dplyr::filter(binary_mean == 2)
plot(bioclim_classified_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_classified_polygons <- as.polygons(bioclim_classified_filt)

# convert to sf object so we can calculate area:
bioclim_classified_sf <- st_as_sf(bioclim_classified_polygons)
bioclim_classified_sf

# calculate area:
bioclim_classified_area <- st_area(bioclim_classified_sf) # 1.02e+12
# convert from m^2 to km^2
bioclim_classified_area <- units::set_units(st_area(bioclim_classified_sf), km^2) 
# 1 017 274 km^2 of suitable habitat


# difference in area predicted:
difference_present_predictions <- bioclim_classified_area - informed_classified_area
# 58 730 km^2 more predicted by bioclim model


# add informed_classified and bioclim_classified together to get model_agreement:
model_agreement <- (informed_classified + bioclim_classified)
plot(model_agreement)
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

# save to file
writeRaster(model_agreement, filename = "outputs/model_agreement.tif", overwrite = TRUE)


# calculate area predicted to be suitable by both informed and bioclim30s (present) models:
# mask to our study extent:
model_agreement_masked <- mask(model_agreement, na_bound_albers)

# calculate area covered by NA cells:
na_model_agreement_filt <- model_agreement_masked %>% 
  dplyr::filter(binary_mean == NA & binary_mean == NaN)
summary(na_model_agreement_filt)

# convert to polygons so we can convert to sf object:
na_model_agreement_poly <- as.polygons(na_model_agreement_filt)

# convert to sf object so we can calculate area:
na_model_agreement_sf <- st_as_sf(na_model_agreement_poly)
na_model_agreement_sf

# calculate area:
na_model_agreement_area <- st_area(na_model_agreement_sf) # 7.32e+11
# convert from m^2 to km^2
model_agreement_area <- units::set_units(st_area(model_agreement_sf), km^2) 
# 731 673 km^2 of suitable habitat


# filter out cells classified as suitable by both models (3)
model_agreement_filt <- model_agreement_masked %>% 
  dplyr::filter(binary_mean == 3)
plot(model_agreement_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_polygons <- as.polygons(model_agreement_filt)

# convert to sf object so we can calculate area:
model_agreement_sf <- st_as_sf(model_agreement_polygons)
model_agreement_sf

# calculate area:
model_agreement_area <- st_area(model_agreement_sf) # 7.32e+11
# convert from m^2 to km^2
model_agreement_area <- units::set_units(st_area(model_agreement_sf), km^2) 
# 731 673 km^2 of suitable habitat

# calculate proportion of total area classified as present for informed model:
proportion_informed <- informed_classified_area/na_bound_area
# 0.229

# calculate proportion of total area classified as present for bioclim model:
proportion_bioclim_pres <- bioclim_classified_area/na_bound_area
# 0.243

# calculate proportion of total area classified as present by both informed
  # and bioclim present models:
proportion_both_present <- model_agreement_area/na_bound_area
# 0.175

# calculate percent overlap:
percent_overlap <- (model_agreement_area / (informed_classified_area + bioclim_classified_area)) *100
# 37 % overlap


# check that all areas add to total area: need to isolate pseudoabsences
# calculate area predicted to be unsuitable in bioclim model:

# filter out cells classified as pseudoabsences (0)
pseudo_pres_filt <- model_agreement_masked %>% 
  dplyr::filter(binary_mean == 0)
plot(pseudo_pres_filt)

# convert raster cells to polygons so we can convert to an sf object:
pseudo_pres_polygons <- as.polygons(pseudo_pres_filt)

# convert to sf object so we can calculate area:
pseudo_pres_sf <- st_as_sf(pseudo_pres_polygons)
pseudo_pres_sf

# calculate area:
pseudo_pres_area <- st_area(pseudo_pres_sf) # 2.65e+12
# convert from m^2 to km^2
pseudo_pres_area <- units::set_units(st_area(pseudo_pres_sf), km^2) 
# 2 646 553 km^2 of UNsuitable habitat

# proportion of total area classified as unsuitable:
proportion_pseudo_pres <- pseudo_pres_area/na_bound_area
# 0.633

# calculate total area, should = 4 183 596 km^2 (na_bound_area)
total_area_present <- pseudo_area + informed_classified_area + bioclim_classified_area - model_agreement_area

total_area_bioclim <- ((pseudo_bioclim_area + pseudo_informed_area) - pseudo_difference) + ((informed_classified_area + bioclim_classified_area) - model_agreement_area)
# 6 796 242 mk^7




# Plotting:



# need to convert binary_mean from numeric to factor
summary(model_agreement)
model_agreement <- as.factor(model_agreement)
summary(model_agreement)
plot(model_agreement)
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement, aes(fill = binary_mean)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "informed", "bioclim", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Informed and Bioclim30s Models (Present)")

agreement_present

ggsave("outputs/agreement_present.png", plot = agreement_present)



# Skeetchestn Territory Calculations:



# Informed Model:
# crop informed_classified to Skeetchestn extent:
  # to be used in plotting section
informed_skeetch <- crop(informed_classified, skeetch_extent)
# mask values outside Skeetchestn Territory to NA:
informed_skeetch_masked <- mask(informed_skeetch, skeetch_vectWGS84)

# project raster to Albers equal area projection:
informed_skeetch_albers <- project(informed_skeetch_masked, "EPSG:3005")

# filter out cells classified as informed (1), removes pseudoabsences (0)
informed_skeetch_filt <- informed_skeetch_albers %>% 
  dplyr::filter(binary_mean == 1)
plot(informed_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_skeetch_polygons <- as.polygons(informed_skeetch_filt)

# convert to sf object so we can calculate area:
informed_skeetch_sf <- st_as_sf(informed_skeetch_polygons)
informed_skeetch_sf

# calculate area:
informed_skeetch_area <- st_area(informed_skeetch_sf) # 3.89e+09
# convert from m^2 to km^2
informed_skeetch_area <- units::set_units(st_area(informed_skeetch_sf), km^2) 
# 3891 km^2 of suitable habitat


# Bioclim Model (present):
# crop bioclim_classified to Skeetchestn extent:
# to be used in plotting section
bioclim_skeetch <- crop(bioclim_classified, skeetch_extent)
# mask values outside Skeetchestn Territory to NA:
bioclim_skeetch_masked <- mask(bioclim_skeetch, skeetch_vectWGS84)

# project raster to Albers equal area projection:
bioclim_skeetch_albers <- project(bioclim_skeetch_masked, "EPSG:3005")

# filter out cells classified as bioclim (2), removes pseudoabsences (0)
bioclim_skeetch_filt <- bioclim_skeetch_albers %>% 
  dplyr::filter(binary_mean == 2)
plot(bioclim_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_skeetch_polygons <- as.polygons(bioclim_skeetch_filt)

# convert to sf object so we can calculate area:
bioclim_skeetch_sf <- st_as_sf(bioclim_skeetch_polygons)
bioclim_skeetch_sf

# calculate area:
bioclim_skeetch_area <- st_area(bioclim_skeetch_sf) # 2.72e+09
# convert from m^2 to km^2
bioclim_skeetch_area <- units::set_units(st_area(bioclim_skeetch_sf), km^2) 
# 2724 km^2 of suitable habitat

# calculate difference in predictions:
difference_present_skeetch <- informed_skeetch_area - bioclim_skeetch_area
# 1167 km^2


# calculate area predicted to be suitable by both informed and bioclim30s (present) models:

# crop model_agreement to Skeetchestn Territory:
  # for use in plotting:
model_agreement_skeetch <- crop(model_agreement, skeetch_extent)
# mask to Skeetchestn Territory boundary:
model_agreement_skeetch_masked <- mask(model_agreement_skeetch, skeetch_vectWGS84)

# reproject to Albers Equal Area projection:
model_agreement_skeetch_albers <- project(model_agreement_skeetch_masked, "EPSG:3005")

# filter out cells classified as suitable by both models (3)
model_agreement_skeetch_filt <- model_agreement_skeetch_albers %>% 
  dplyr::filter(binary_mean == 3)
plot(model_agreement_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_skeetch_polygons <- as.polygons(model_agreement_skeetch_filt)

# convert to sf object so we can calculate area:
model_agreement_skeetch_sf <- st_as_sf(model_agreement_skeetch_polygons)
model_agreement_skeetch_sf

# calculate area:
model_agreement_skeetch_area <- st_area(model_agreement_skeetch_sf) # 2.37e+09
# convert from m^2 to km^2
model_agreement_skeetch_area <- units::set_units(st_area(model_agreement_skeetch_sf), km^2) 
# 2372 km^2 of suitable habitat

# calculate percent overlap:
percent_overlap_skeetch <- (model_agreement_skeetch_area / (informed_skeetch_area + bioclim_skeetch_area)) *100
# 35.9 % overlap

# proportion of SKeetchestn Territory classified as present by informed model:
proportion_informed_skeetch <- informed_skeetch_area/skeetch_area
# 55.6 %

# proportion of Skeetchestn Territory classified as present by bioclim model:
proportion_bioclim_pres_skeetch <- bioclim_skeetch_area/skeetch_area
# 38.9 %


# Plot area of agreement in Skeetchestn Territory:



# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:
# turn Skeetchestn boundary polygon into lines geometry:
skeetch_lines <- as.lines(skeetch_vectWGS84)

# convert raster from numeric to factor
model_agreement_skeetch <- as.factor(model_agreement_skeetch)

model_agreement_skeetch
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence


skeetch_agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "present", "future", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Informed and Bioclim30s Models (Present)")

skeetch_agreement_present

ggsave("outputs/skeetch_agreement_present.png", plot = skeetch_agreement_present)



# Area of Agreement between Bioclim present and future predictions



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

# mask to study extent:
bioclim_classified_fut_masked <- mask(bioclim_classified_fut, na_bound_vect)
# project raster to Albers equal area projection:
bioclim_classified_fut_albers <- project(bioclim_classified_fut, "EPSG:3005")

# filter out cells classified as bioclim (4), removes pseudoabsences (0)
bioclim_classified_fut_filt <- bioclim_classified_fut_albers %>% 
  dplyr::filter(binary_mean == 4)
plot(bioclim_classified_fut_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_classified_fut_polygons <- as.polygons(bioclim_classified_fut_filt)

# convert to sf object so we can calculate area:
bioclim_classified_fut_sf <- st_as_sf(bioclim_classified_fut_polygons)
bioclim_classified_fut_sf

# calculate area:
bioclim_classified_fut_area <- st_area(bioclim_classified_fut_sf) # 8.94e+11
# convert from m^2 to km^2
bioclim_classified_fut_area <- units::set_units(st_area(bioclim_classified_fut_sf), km^2) 
# 893 504 km^2 of suitable habitat

# difference in suitable area present to future:
difference_area_future <- bioclim_classified_fut_area - bioclim_classified_area
# -111 767 km^2 (less predicted by future model)


# add bioclim_classified_fut and bioclim_classified together to get model_agreement:
model_agreement_fut <- (bioclim_classified + bioclim_classified_fut)
plot(model_agreement_fut)
# 0 = pseudoabsence
# 2 = bioclim30s present prediction of presence
# 4 = bioclim30s future prediction of presence
# 6 = agreement between both bioclim30s present and future predicted presence

# save to file
writeRaster(model_agreement_fut, filename = "outputs/model_agreement_future.tif", overwrite = TRUE)

# calculate area of agreement:
# mask to study extent:
model_agreement_fut_masked <- mask(model_agreement_fut, na_bound_vect)
# Project to Albers equal area projection
model_agreement_fut_albers <- project(model_agreement_fut_masked, "EPSG:3005")

# filter out cells in agreement between models (value = 6)
model_agreement_fut_filt <- model_agreement_fut %>% 
  dplyr::filter(binary_mean == 6)
plot(model_agreement_fut_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_fut_polygons <- as.polygons(model_agreement_fut_filt)

# convert to sf object so we can calculate area:
model_agreement_fut_sf <- st_as_sf(model_agreement_fut_polygons)
crs(model_agreement_fut_sf) # BC Albers

# calculate area
model_agreement_fut_area <- st_area(model_agreement_fut_sf) # 7.21e+11
# convert from m^2 to km^2
model_agreement_fut_area <- units::set_units(st_area(model_agreement_fut_sf), km^2) 
# 720 600 km^2 of suitable habitat



# filter out cells classified as pseudoabsence between models (value = 0)
pseudoabs_fut_filt <- model_agreement_fut %>% 
  dplyr::filter(binary_mean == 0)
plot(pseudoabs_fut_filt)

# convert raster cells to polygons so we can convert to an sf object:
pseudoabs_fut_polygons <- as.polygons(pseudoabs_fut_filt)

# convert to sf object so we can calculate area:
pseudoabs_fut_sf <- st_as_sf(pseudoabs_fut_polygons)
pseudoabs_fut_sf # BC Albers

# calculate area
pseudoabs_fut_area <- st_area(pseudoabs_fut_sf) # 2.83e+12
# convert from m^2 to km^2
pseudoabs_fut_area <- units::set_units(st_area(pseudoabs_fut_sf), km^2) 
# 2 833 434 km^2 of UNsuitable habitat


# calculate percent overlap in area of agreement:
percent_overlap_fut <- (model_agreement_fut_area/(bioclim_classified_area + bioclim_classified_fut_area - model_agreement_fut_area)) * 100
# 60.5 %

# proportion of total study area classified as suitable by both models:
proportion_agreement_fut <- model_agreement_fut_area/na_bound_area
# 0.172

# proportion of total study area classified as suitable by bioclim30s present model:
proportion_bioclim_pres <- bioclim_classified_area/na_bound_area
# 0.243

# proportion of total study area classified as suitable by bioclim30s future model:
proportion_bioclim_fut <- bioclim_classified_fut_area/na_bound_area
# 0.214

# proportion of total study area classified as UNsuitable by bioclim30s model:
proportion_pseudoabs_fut <- pseudoabs_fut_area/na_bound_area
# 0.677

# Plotting:



# need to convert binary_mean from numeric to factor
summary(model_agreement_fut)
model_agreement_fut <- as.factor(model_agreement_fut)
summary(model_agreement_fut)
plot(model_agreement_fut)
# now: 
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions

agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut, aes(fill = binary_mean)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "present", "future", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Bioclim30s Present and Future Models")

agreement_future
ggsave("outputs/agreement_future.png", plot = agreement_future)



# Skeetchestn Territory Calculations:



# Reuse Bioclim present calculations from above:
bioclim_skeetch_area

# Bioclim Model (future):
# crop bioclim_classified to Skeetchestn extent:
# to be used in plotting section
bioclim_fut_skeetch <- crop(bioclim_classified_fut, skeetch_extent)
# mask values outside Skeetchestn Territory to NA:
bioclim_fut_skeetch_masked <- mask(bioclim_fut_skeetch, skeetch_vectWGS84)
plot(bioclim_fut_skeetch_masked)

# project raster to Albers equal area projection:
bioclim_fut_skeetch_albers <- project(bioclim_fut_skeetch_masked, "EPSG:3005")

# filter out cells classified as bioclim (4), removes pseudoabsences (0)
bioclim_fut_skeetch_filt <- bioclim_fut_skeetch_albers %>% 
  dplyr::filter(binary_mean == 4)
plot(bioclim_fut_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_fut_skeetch_polygons <- as.polygons(bioclim_fut_skeetch_filt)

# convert to sf object so we can calculate area:
bioclim_fut_skeetch_sf <- st_as_sf(bioclim_fut_skeetch_polygons)
bioclim_fut_skeetch_sf

# calculate area:
bioclim_fut_skeetch_area <- st_area(bioclim_fut_skeetch_sf) # 1.14e+09
# convert from m^2 to km^2
bioclim_fut_skeetch_area <- units::set_units(st_area(bioclim_fut_skeetch_sf), km^2) 
# 1141 km^2 of suitable habitat

# calculate difference in predictions:
difference_bioclim_fut_pres_skeetch <- bioclim_skeetch_area - bioclim_fut_skeetch_area
# 1583 km^2


# calculate area predicted to be suitable by both bioclim30s present and future models:


# crop model_agreement to Skeetchestn Territory:
# for use in plotting:
model_agreement_fut_skeetch <- crop(model_agreement_fut, skeetch_extent)
# mask to Skeetchestn Territory boundary:
model_agreement_fut_skeetch_masked <- mask(model_agreement_fut_skeetch, skeetch_vectWGS84)
plot(model_agreement_fut_skeetch_masked)

# reproject to Albers Equal Area projection:
model_agreement_fut_skeetch_albers <- project(model_agreement_fut_skeetch_masked, "EPSG:3005")
plot(model_agreement_fut_skeetch_albers)

# filter out cells classified as suitable by both models (6)
model_agreement_fut_skeetch_filt <- model_agreement_fut_skeetch_albers %>% 
  dplyr::filter(binary_mean == 6)
plot(model_agreement_fut_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_fut_skeetch_polygons <- as.polygons(model_agreement_fut_skeetch_filt)

# convert to sf object so we can calculate area:
model_agreement_fut_skeetch_sf <- st_as_sf(model_agreement_fut_skeetch_polygons)
model_agreement_fut_skeetch_sf

# calculate area:
model_agreement_fut_skeetch_area <- st_area(model_agreement_fut_skeetch_sf) # 1.14e+09
# convert from m^2 to km^2
model_agreement_fut_skeetch_area <- units::set_units(st_area(model_agreement_fut_skeetch_sf), km^2) 
# 1141 km^2 of suitable habitat

# calculate percent overlap:
percent_overlap_fut_skeetch <- (model_agreement_fut_skeetch_area / (bioclim_skeetch_area + bioclim_fut_skeetch_area)) *100
# 29.5% overlap

# proportion of Skeetchestn Territory classified as present by bioclim future model:
proportion_bioclim_fut_skeetch <- bioclim_fut_skeetch_area/skeetch_area
# 16.3 %


# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:

plot(model_agreement_fut_skeetch)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions
summary(model_agreement_fut_skeetch)

# need to convert binary_mean from numeric to factor
model_agreement_fut_skeetch <- as.factor(model_agreement_fut_skeetch)
summary(model_agreement_fut_skeetch)

plot(model_agreement_fut_skeetch)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions


skeetch_agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "present", "future", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     # limits = c(120.2, 121.6),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Bioclim30s Present and Future Models")

skeetch_agreement_future

ggsave("outputs/skeetch_agreement_future.png", plot = skeetch_agreement_future)



# Plot the area of agreement plots together:

# Full study extent:

model_agreement
model_agreement_fut

# create temp rasters so we don't overwrite the originals:
model_agreement_temp <- model_agreement
model_agreement_fut_temp <- model_agreement_fut

# change the names of our rasters:
names(model_agreement_temp) <- "Informed and Bioclim Present"
names(model_agreement_fut_temp) <- "Bioclim Present and Future"

# create a multilayer raster object:
agreement_full_extent <- c(model_agreement_temp, model_agreement_fut_temp)

# convert raster from numeric to factor
agreement_full_extent <- as.factor(agreement_full_extent)

agreement_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_full_extent) +
  facet_wrap(~lyr, nrow = 1, ncol = 2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "informed", "bioclim present", "informed & bioclim", "bioclim future", "bioclim future & present"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF", "#440154FF", "#414487FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0))

ggsave("outputs/full_extent_cont_plots.png", predictions_continuous_plot, 
       width = 12, height = 4, units = "in")