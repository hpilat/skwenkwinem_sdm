# This is script 08/08
# This script isolates area of agreement between our predictions for our total
  # study area and Skeetchestn Territory
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim_30s.R
# 07_area_calculations.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)


# Binary Predictions:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")


# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")

# Skeetchestn territory boundary vector for masking:
skeetch_vect <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# transform to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect, "EPSG:4326")

# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extent <- ext(-121.6, -120.1, 50.3, 51.6) # xmin xmax ymin ymax


# Area Calculations


# Overall study extent:
# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
na_bound_albers <- st_transform(na_bound_sf, "EPSG:3005")
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_albers) # 4.18e+12 m^2
na_bound_area <- units::set_units(st_area(na_bound_albers), km^2) # 4 183 596  km^2


# Skeetchestn Territory:
skeetch_sf <- read_sf("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)
crs(skeetch_sf) # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) # 6996 km^2


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
writeRaster(model_agreement, filename = "outputs/model_agreement.tif", overwrite = TRUE)

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
writeRaster(model_agreement_fut, filename = "outputs/model_agreement_future.tif", overwrite = TRUE)

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


# Plotting:


# need to convert binary_mean from numeric to factor
model_agreement_fut <- as.factor(model_agreement_fut)
summary(model_agreement_fut)
plot(model_agreement_fut)
# now: 
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions
# NaN


agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut, aes(fill = binary_mean)) +
  scale_fill_manual(name = NULL, 
                    na.translate = FALSE, 
                    labels = c("pseudoabsence", "present", "future", "both", "NA"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF", "transparent")) +
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




# Crop area of agreement to Skeetchestn Territory:



# Informed and Bioclim30s - Present:

model_agreement <- rast("outputs/model_agreement.tif")
model_agreement # WGS84
# crop and mask to Skeetchestn Territory
model_agreement_skeetch <- crop(model_agreement, skeetch_vectWGS84)
model_agreement_skeetch_masked <- mask(model_agreement_skeetch, skeetch_vectWGS84)
plot(model_agreement_skeetch_masked)

# calculate area of agreement:
# first need to project to Albers equal area projection
model_agreement_skeetch_albers <- project(model_agreement_skeetch_masked, "EPSG:3005")

# filter out cells in agreement between models (value = 3)
model_agreement_skeetch_filt <- model_agreement_skeetch_albers %>% 
  dplyr::filter(binary_mean == 3)
plot(model_agreement_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_skeetch_polygons <- as.polygons(model_agreement_skeetch_filt)

# convert to sf object so we can calculate area:
model_agreement_skeetch_sf <- st_as_sf(model_agreement_skeetch_polygons)
crs(model_agreement_skeetch_sf) # BC Albers

# calculate area
model_agreement_skeetch_area <- st_area(model_agreement_skeetch_sf) # 6.48e+11
# convert from m^2 to km^2
model_agreement_skeetch_area <- units::set_units(st_area(model_agreement_skeetch_sf), km^2) 
# 2381 km^2 of suitable habitat in agreement


# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:
# turn Skeetchestn boundary polygon into lines geometry:
skeetch_lines <- as.lines(skeetch_vectWGS84)

skeetch_agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_continuous() +
  labs(title = "Area of Agreement", subtitle = "Informed and Bioclim30s Model", xlab = "Longitude", ylab = "Latitude")

ggsave("outputs/skeetch_agreement_present.png")


# Bioclim30s Present to Future:

# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions

model_agreement_fut # WGS84
# crop and mask to Skeetchestn Territory
model_agreement_fut_skeetch <- crop(model_agreement_fut, skeetch_extent)
model_agreement_fut_skeetch_masked <- mask(model_agreement_fut_skeetch, skeetch_vectWGS84)
plot(model_agreement_fut_skeetch_masked)

# calculate area of agreement:
# first need to project to Albers equal area projection
model_agreement_fut_skeetch_albers <- project(model_agreement_fut_skeetch_masked, "EPSG:3005")

# filter out cells in agreement between models (value = 3)
model_agreement_fut_skeetch_filt <- model_agreement_fut_skeetch_albers %>% 
  dplyr::filter(binary_mean == 6)
plot(model_agreement_fut_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
model_agreement_fut_skeetch_polygons <- as.polygons(model_agreement_fut_skeetch_filt)

# convert to sf object so we can calculate area:
model_agreement_fut_skeetch_sf <- st_as_sf(model_agreement_fut_skeetch_polygons)
crs(model_agreement_fut_skeetch_sf) # BC Albers

# calculate area
model_agreement_fut_skeetch_area <- st_area(model_agreement_fut_skeetch_sf) # 6.48e+11
# convert from m^2 to km^2
model_agreement_fut_skeetch_area <- units::set_units(st_area(model_agreement_fut_skeetch_sf), km^2) 
# 1147 km^2 of suitable habitat in agreement

# calculate percent overlap:
percent_overlap_bioclim30s_pres_fut <- (model_agreement_fut_skeetch_area/(3169 + 1422)) * 100 
# denominator = area predicted to be suitable by present and future predictions

# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:

plot(model_agreement_fut_skeetch)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions

# need to convert binary_mean from numeric to character to factor?

values(model_agreement_fut_skeetch) <- as.character(values(model_agreement_fut_skeetch))
model_agreement_fut_skeetch

# use as_factor (note the underscore) so factors are assigned in correct order
  # preserves category labels
values(model_agreement_fut_skeetch) <- as.factor(values(model_agreement_fut_skeetch))
plot(model_agreement_fut_skeetch)
# now: 
# 1 = pseudoabsence
# 2 = bioclim present prediction
# 3 = bioclim future prediction
# 4 = agreement between both bioclim present and future predictions


skeetch_agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_manual(name = NULL, 
                    labels = c("pseudoabsence", "present", "future", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725FF", "#7AD151FF")) +
  scale_x_continuous(name = "Longitude (°W)", 
                   #  breaks = c(121.4, 121.0, 120.6, 120.2),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Bioclim30s Present and Future Models")

ggsave("outputs/skeetch_agreement_future.png")

