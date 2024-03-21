
library(terra)
library(tidyverse)
library(tidyterra)
library(sf)

# Skeetchestn Territory:
skeetch_sf <- st_read("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
skeetch_sf

# convert to SpatVector for masking our rasters:
skeetch_vect <- vect(skeetch_sf)
skeetch_vect
# create extent object slightly larger than vector for cropping our rasters:
skeetch_extent <- ext(1314424, 1398550, 609867, 732906) # xmin, xmax, ymin, ymax


# calculate area:
skeetch_area <- st_area(skeetch_sf) # 7e+09
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) 
# 6996 km^2 of suitable habitat

# read in binary prediction raster:
model_agreement_fut <- rast("outputs/model_agreement_future.tif")
model_agreement_fut

# project to Albers equal area:
model_agreement_fut <- project(model_agreement_fut, "EPSG:3005", method = "near")
plot(model_agreement_fut)

# crop and mask to Skeetchestn Territory:
model_agreement_fut_skeetch <- crop(model_agreement_fut, skeetch_extent)
model_agreement_fut_skeetch <- mask(model_agreement_fut_skeetch, skeetch_vect)

# get dimensions of raster:
model_agreement_fut
# so total dimensions / area == 
4123 * 4461
# 18 392 703 cells

# cell resolution in m^2:
cell_resolution_m <- 749.1733 * 749.1733
cell_resolution_km <- cell_resolution_m / 1000000

# number of cells in each category?
cell_counts <- freq(model_agreement_fut)
cell_counts
# 0 = pseudoabsence
# 2 = bioclim present 
# 4 = bioclim future 
# 6 = agreement between both bioclim30s present and future predicted presence

# how many cells in count column?
sum(cell_counts$count)
#[1] 7 168 884 # ! almost half the total dimensions
# how many cells are NA?
global(model_agreement_fut, fun="isNA")
# 11 223 819 
# add cells with values to calls that are NAs to check counts.
sum(cell_counts$count) + global(model_agreement_fut, fun="isNA") 
# 18392703 

# Proportion of total area in each category
cell_counts$prop <- cell_counts$count/sum(cell_counts$count)
cell_counts$prop

# total area of study extent:
na_bound_area <- sum(cell_counts$count[1:4])
na_bound_area
# 7 168 884 cells
na_bound_area <- na_bound_area * cell_resolution_km
na_bound_area 
# 4 023 612 km^2

cell_counts$count
# predicted total suitable habitat is the sum of present, future, and both
area_total_suitable <- sum(cell_counts$count[c(2, 4),])
area_total_suitable
# 2 120 544 cells
area_total_suitable <- total_suitable * cell_resolution_km
area_total_suitable
# 1 190 178 km^2

# predicted suitable habitat from bioclim_present model:
  # bioclim present only (row 1)
area_bioclim_present <- cell_counts$count[1]
area_bioclim_present
#  5 576 925 cells
area_bioclim_present <- area_bioclim_present * cell_resolution_km
area_bioclim_present
#  296 674 km^2

# present predicted suitable habitat, as a percent of total predicted suitable habitat:
percent_suitable_present <- (cell_counts$count[2]/sum(cell_counts$count[2:4]))*100
percent_suitable_present
# 24.92686
# present predicted suitable habitat, as a percent of total study extent:
percent_suitable_present_total <- (cell_counts$count[2]/sum(cell_counts$count[1:4]))*100
# 8.19773354259902
# future predicted suitable habitat, as a percent of total predicted suitable habitat:
percent_suitable_future <- (cell_counts$count[3]/sum(cell_counts$count[2:4]))*100
# [1] 13.81997
# future predicted suitable habitat, as a percent of total study extent:
percent_suitable_future_total <- (cell_counts$count[3]/sum(cell_counts$count[1:4]))*100
# 4.23120022847559
# area of agreement between present and future predicted suitable habitat
# as a percent of total predicted suitable habitat
area_agreement_suitable <- (cell_counts$count[4]/sum(cell_counts$count[2:4]))*100
# [1] 59.40455 overlap in predictions
area_agreement_total <- (cell_counts$count[4]/sum(cell_counts$count[1:4]))*100
# 18.1876363277988

# now get range of cell sizes over our Study extent:
rast_agg <- aggregate(rast(model_agreement_fut), 100)
rast_agg
cell_size <- cellSize(rast_agg, unit="km") / 10000
resampled_rast <- resample(cell_size, model_agreement_fut)
minmax(cell_size)
# cell size ranges from 0.4613605 m^2 to 0.7288025 m^2


area_zones <- zonal(resampled_rast, model_agreement_fut, sum, na.rm=TRUE)
area_zones
# pseudoabsence = 2833398.3
# bioclim present = 296816.6
# bioclim future = 172840.7
# agreement = 720630.8