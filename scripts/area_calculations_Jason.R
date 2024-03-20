## testing out area calculations by cell with Skeetchestn Territory

library(terra)
library(tidyverse)
library(tidyterra)
library(sf)

# read in raster with classified cells:
model_agreement_fut_skeetch <- rast("outputs/model_agreement_fut_skeetch.tif")
plot(model_agreement_fut_skeetch)
model_agreement_fut_skeetch # WGS84 

# get dimensions of raster:
model_agreement_fut_skeetch
# so total dimensions / area == 
139 * 159
# 22 101 cells
cell_counts <- freq(model_agreement_fut_skeetch)
cell_counts
# when I use Albers equal area projection here, there are 7 classes (0-6), 
  # when there should only be 0, 2, 4, 6
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = overlap between predictions


#how many cells in count column
sum(cell_counts$count)
#[1] 13 224 
# how many cells are NA?
global(model_agreement_fut_skeetch, fun="isNA")
# 8877

# add cells with values to calls that are NAs to check counts.
sum(cell_counts$count) + global(model_agreement_fut_skeetch, fun="isNA") 
# 22 101
# ^ matches the total cells from the dimensions

# Proportion of total area in each category
cell_counts$prop <- cell_counts$count/sum(cell_counts$count)
cell_counts$prop

# total area of study extent:
skeetch_area <- sum(cell_counts$count[1:4])
# 13 224 WRONG- double-dipping with overlap. Calculations with st_area give me 6996 km^2
# tried below code and skeetch_area became NA:
# skeetch_area <- (sum(cell_counts$count[1:4])) - (cell_counts$count[6])

# predicted total suitable habitat is the sum of present, future, and both
total_suitable <- sum(cell_counts$count[2:4])
# 5849

# present predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[2]/sum(cell_counts$count[2:4]))*100
# 55.12053

# did not proceed with below calculations for Skeetchestn, as skeetch_area is incorrect
# (values written below are from total study extent calculations)

# present predicted suitable habitat, as a percent of total study extent:
percent_suitable_present_total <- (cell_counts$count[2]/sum(cell_counts$count[1:4]))*100

# future predicted suitable habitat, as a percent of total predicted suitable habitat:
percent_suitable_future <- (cell_counts$count[3]/sum(cell_counts$count[2:4]))*100

# future predicted suitable habitat, as a percent of total study extent:
percent_suitable_future_total <- (cell_counts$count[3]/sum(cell_counts$count[1:4]))*100

# area of agreement between present and future predicted suitable habitat
# as a percent of total predicted suitable habitat
area_agreement_suitable <- (cell_counts$count[4]/sum(cell_counts$count[2:4]))*100

area_agreement_total <- (cell_counts$count[4]/sum(cell_counts$count[1:4]))*100


# now get range of cell sizes over our Study extent:
# aggregated for full study extent, probably don't need to for Skeetch:
# rast_agg <- aggregate(rast(model_agreement_fut_skeetch), 100)
# rast_agg
cell_size <- cellSize(model_agreement_fut_skeetch, unit="km") / 10000
resampled_rast <- resample(cell_size, model_agreement_fut_skeetch)
minmax(cell_size)
# cell size ranges from 5.363684e-05 m^2 (?) to 5.496545e-05 m^2 (?)


area_zones <- zonal(resampled_rast, model_agreement_fut_skeetch, sum, na.rm=TRUE)
area_zones # gives 10 areas for some reason? Should only be for classes 0, 2, 4, 6
