# This is script 09/10 (?)
# This script calculates area predicted as suitable by our models
  # over our entire study extent

library(sf)
library(terra)


# From Dave:
model_agreement_fut <- rast("outputs/model_agreement_future.tif")
model_agreement_fut
# so total dimensions / area == 
3143 * 4084
# 12,836,012 cells
cell_counts <- freq(model_agreement_fut)
cell_counts
#how many cells in count column
sum(cell_counts$count)
#[1] 6656291 # !half the total dimensions
# how many cells are NA?
global(model_agreement_fut, fun="isNA")
#binary_mean 6179721 # again, about half, 
# add cells with values to calls that are NAs to check counts.
sum(cell_counts$count) + global(model_agreement_fut, fun="isNA") # good this matches the total cells from the dimensions.
#                 isNA
# binary_mean 12836012
# Proportion of total area in each category
cell_counts$prop <- cell_counts$count/sum(cell_counts$count)
cell_counts$prop
# predicted total is sum of present, future, and both
sum(cell_counts$count[2:4])
# 2037928
# present predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[2]/sum(cell_counts$count[2:4]))*100
#[1] 26.77548
# future predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[3]/sum(cell_counts$count[2:4]))*100
# [1] 13.81997
# area of agreement between present and future predicted suitable habitat
  # as a percent of total predicted suitable habitat
(cell_counts$count[4]/sum(cell_counts$count[2:4]))*100
# [1] 59.40455

# now get range of cell sizes over our Study extent:
rast_agg <- aggregate(rast(model_agreement_fut), 100)
rast_agg
cell_size <- cellSize(rast_agg, unit="km") / 10000
resampled_rast <- resample(cell_size, model_agreement_fut)
minmax(cell_size)
# cell size ranges from 0.4613605 m^2 to 0.7288025 m^2


area_zones <- zonal(resampled_rast, model_agreement_fut, sum, na.rm=TRUE)
area_zones



## Total Study Area:

# read in binary prediction rasters:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")


# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")



# Informed Prediction:
informed_present_binary

# reproject raster to Albers equal area projection:
informed_present_albers <- project(informed_present_binary, "EPSG:4326")
plot(informed_present_albers)

# filter out cells classified as "presence"
informed_presence_filt <- informed_present_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(informed_presence_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_presence_polygons <- as.polygons(informed_presence_filt)

# convert to sf object so we can calculate area:
informed_presence_sf <- st_as_sf(informed_presence_polygons)
informed_presence_sf

# project to Albers equal area:
informed_presence_sf <- st_transform(informed_presence_sf, "EPSG:3005")

# calculate area:
informed_presence_area <- st_area(informed_presence_sf) # 9.59e+11
# convert from m^2 to km^2
informed_presence_area <- units::set_units(st_area(informed_presence_sf), km^2) 
# 958 837 km^2 of suitable habitat


# Bioclim30s present prediction:
bioclim30s_present_binary

# reproject raster to Albers equal area projection:
bioclim_present_albers <- project(bioclim30s_present_binary, "EPSG:4326")
plot(bioclim_present_albers)

# filter out cells classified as "presence"
bioclim_present_filt <- bioclim_present_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_present_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_present_polygons <- as.polygons(bioclim_present_filt)

# convert to sf object so we can calculate area:
bioclim_present_sf <- st_as_sf(bioclim_present_polygons)
bioclim_present_sf

# project to Albers equal area:
bioclim_present_sf <- st_transform(bioclim_present_sf, "EPSG:3005")
bioclim_present_sf

# calculate area:
bioclim_present_area <- st_area(bioclim_present_sf) # 1.02e+12
# convert from m^2 to km^2
bioclim_present_area <- units::set_units(st_area(bioclim_present_sf), km^2) 
# 1 017 427 km^2 of suitable habitat

# get the intersection of the informed and bioclim predicted suitable habitat:
model_agreement_present <- terra::intersect(informed_presence_filt, bioclim_present_filt)
model_agreement_present
plot(model_agreement_present)

# reproject to Albers equal area projection:
agreement_present_albers <- project(model_agreement_present, "EPSG:4326")

# filter out cells classified as "TRUE" (where the models intersect)
agreement_present_filt <- agreement_present_albers %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_present_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_present_polygons <- as.polygons(agreement_present_filt)

# convert to sf object so we can calculate area:
agreement_present_sf <- st_as_sf(agreement_present_polygons)
agreement_present_sf

# project to Albers equal area:
agreement_present_sf <- st_transform(agreement_present_sf, "EPSG:3005")

# calculate area:
agreement_present_area <- st_area(agreement_present_sf) # 7.32e+11
# convert from m^2 to km^2
agreement_present_area <- units::set_units(st_area(agreement_present_sf), km^2) 
# 731 779 km^2 of suitable habitat


# Bioclim30s Future Prediction:

bioclim30s_future_binary

# reproject raster to Albers equal area projection:
bioclim_future_albers <- project(bioclim30s_future_binary, "EPSG:4326")
plot(bioclim_future_albers)

# filter out cells classified as "presence"
bioclim_future_filt <- bioclim_future_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_future_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_future_polygons <- as.polygons(bioclim_future_filt)

# convert to sf object so we can calculate area:
bioclim_future_sf <- st_as_sf(bioclim_future_polygons)
bioclim_future_sf

# project to Albers equal area:
bioclim_future_sf <- st_transform(bioclim_future_sf, "EPSG:3005") 

# calculate area:
bioclim_future_area <- st_area(bioclim_future_sf) # 8.94e+11
# convert from m^2 to km^2
bioclim_future_area <- units::set_units(st_area(bioclim_future_sf), km^2) 
# 893 504 km^2 of suitable habitat


# get the intersection of the informed and bioclim predicted suitable habitat:
model_agreement_future <- terra::intersect(bioclim_present_filt, bioclim_future_filt)
model_agreement_future
plot(model_agreement_future)

# reproject to Albers equal area projection:
agreement_future_albers <- project(model_agreement_future, "EPSG:4326")

# filter out cells classified as "TRUE" (where the models intersect)
agreement_future_filt <- agreement_future_albers %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_future_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_future_polygons <- as.polygons(agreement_future_filt)

# convert to sf object so we can calculate area:
agreement_future_sf <- st_as_sf(agreement_future_polygons)
agreement_future_sf

# project to Albers equal area:
agreement_future_sf <- st_transform(agreement_future_sf, "EPSG:3005")
agreement_future_sf

# calculate area:
agreement_future_area <- st_area(agreement_future_sf) # 7.21e+11
# convert from m^2 to km^2
agreement_future_area <- units::set_units(st_area(agreement_future_sf), km^2) 
# 720 657 km^2 of suitable habitat


# calculate difference in suitable area from present to future:
difference_area <- bioclim_future_area - bioclim_present_area
# -123 923 km^2

# calculate proportion of suitable habitat changed from present to future:
proportion_diff_area <- difference_area / bioclim_present_area
# -0.122


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