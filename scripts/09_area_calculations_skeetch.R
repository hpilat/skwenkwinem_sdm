# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 09/11
# This script calculates area predicted to be suitable for skwenkwinem
  # over Skeetchestn Territory
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R
# 08_area_calculations_full_extent.R


library(terra)
library(dplyr)

# import tifs

informed <- terra::rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim_pres <- terra::rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim_fut <- terra::rast("outputs/skwenkwinem_bioclim30s_predict_future_binary_585.tif")


# reproject to North America Albers equal-area conic for area calculation
# https://spatialreference.org/ref/esri/102008/
crs(informed) <- "epsg:4326"
crs(bioclim_pres) <- "epsg:4326"
crs(bioclim_fut) <- "epsg:4326"
# define CRS
new_crs <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

informed_albers <- terra::project(informed, new_crs, method = "near")
bioclim_pres_albers <- terra::project(bioclim_pres, new_crs, method = "near")
bioclim_fut_albers <- terra::project(bioclim_fut, new_crs, method = "near")


# Skeetchestn territory boundary vector for masking:
skeetch_vect_albers <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")

# reproject to new_crs:
skeetch_vect <- terra::project(skeetch_vect_albers, new_crs)
skeetch_vect

# crop and mask rasters to Skeetchestn boundary:
informed_skeetch <- crop(informed_albers, skeetch_vect)
informed_skeetch <- mask(informed_skeetch, skeetch_vect)
plot(informed_skeetch)
bioclim_pres_skeetch <- crop(bioclim_pres_albers, skeetch_vect)
bioclim_pres_skeetch <- mask(bioclim_pres_skeetch, skeetch_vect)
bioclim_fut_skeetch <- crop(bioclim_fut_albers, skeetch_vect)
bioclim_fut_skeetch <- mask(bioclim_fut_skeetch, skeetch_vect)



# Informed vs Bioclim Present:


informed_skeetch
# dimensions:
total_cells <- 166 * 108
total_cells
# 17928 cells

# resolution: 761.5899 m X 761.5899 m
cell_resolution_m <- 761.5899 * 761.5899
cell_resolution_km <- cell_resolution_m / 1000000 # 1 000 000 m^2 in 1 km^2

# number of cells in each category:
cell_counts_informed <- freq(informed_skeetch)
cell_counts_informed
# 9682 presence cells
# 2628 pseudoabsence cells

# number of cells classified as presence (row 1):
informed_presence_cells <- cell_counts_informed$count[1]
informed_presence_cells

# area of cells classified as presence:
area_informed <- informed_presence_cells * cell_resolution_km
area_informed
# 5615.746 km^2

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_informed$count)
# 12 310

# how many cells are NA?
global(informed_skeetch, fun="isNA")
# 5618

# total number of cells:
sum(cell_counts_informed$count) + global(informed_skeetch, fun="isNA") 
# 17928, matches total number of cells from dimensions


bioclim_pres_skeetch

# number of cells in each category:
cell_counts_bioclim_pres <- freq(bioclim_pres_skeetch)
cell_counts_bioclim_pres
# 8811 presence cells
# 3585 pseudoabsence cells

# number of cells classified as presence (row 1):
bioclim_pres_presence_cells <- cell_counts_bioclim_pres$count[1]
bioclim_pres_presence_cells
# 8811 cells

# area of cells classified as presence:
area_bioclim_pres <- bioclim_pres_presence_cells * cell_resolution_km
area_bioclim_pres
#  5110.549 km^2

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_bioclim_pres$count)
# 12396

# how many cells are NA?
global(bioclim_pres_skeetch, fun="isNA")
# 5532 - fewer NAs than informed raster

# total number of cells:
sum(cell_counts_bioclim_pres$count) + global(bioclim_pres_skeetch, fun="isNA") 
# 17 928, matches total number of cells from dimensions


# calculate difference in area predicted as presence by each model:
# row 1, column 4 is difference in presence area in km^2
diff_informed_bioclim_pres <- area_informed - area_bioclim_pres
diff_informed_bioclim_pres
#  505.1967 km^2, informed model predicts more suitable area than bioclim model


# Area of agreement between informed and bioclim_present models:

# here make new raster where 2 = both predicted, 1 = bioclim present only, 3 = informed only, 6 =  both none

agree_informed_bioclim <- rast(nrows = 166, ncols = 108, extent = ext(bioclim_pres_skeetch), crs = new_crs, vals = 0)

agree_informed_bioclim[bioclim_pres_skeetch == 1 & informed_skeetch == 1] <- 2
agree_informed_bioclim[bioclim_pres_skeetch == 1 & informed_skeetch == 2] <- 1
agree_informed_bioclim[bioclim_pres_skeetch == 2 & informed_skeetch == 1] <- 3
agree_informed_bioclim[bioclim_pres_skeetch == 2 & informed_skeetch == 2] <- 6

plot(agree_informed_bioclim)
writeRaster(agree_informed_bioclim, filename = "outputs/agreement_informed_bioclim_skeetch.tif", overwrite = TRUE)

cell_counts_informed_bioclim_pres <- freq(agree_informed_bioclim)
cell_counts_informed_bioclim_pres
sum(cell_counts_informed_bioclim_pres$count)
# 17 928

# number of cells classified as presence for both models (row 3):
agreement_present_cells <- cell_counts_informed_bioclim_pres$count[3]
agreement_present_cells
# 7226

# area of cells classified as presence for both models:
area_agreement_present <- agreement_present_cells * cell_resolution_km
area_agreement_present
# 4191.219 km^2



# Bioclim Present vs Future:

bioclim_fut_skeetch
# dimensions:
total_cells <- 166 * 108
# 17 928 cells

# number of cells in each category:
cell_counts_bioclim_fut <- freq(bioclim_fut_skeetch)
cell_counts_bioclim_fut
# 3561 presence cells SSP 1-2.6
# 9262 presence cells SSP 5-8.5
# 8835 pseudoabsence cells SSP 1-2.6
# 3134 pseudoabsence cells SSP 5-8.5

# number of cells classified as presence (row 1):
bioclim_fut_presence_cells <- cell_counts_bioclim_fut$count[1]
bioclim_fut_presence_cells
# 3561 cells SSP 126
# 9262 cells SSP 585

# area of cells classified as presence:
area_bioclim_fut <- bioclim_fut_presence_cells * cell_resolution_km
area_bioclim_fut
# 2065.448 km^2 SSP 126
# 5372.138 km^2 SSP 585

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_bioclim_fut$count)
# 12 396

# how many cells are NA?
global(bioclim_fut_skeetch, fun="isNA")
# 5532 - fewer NAs than informed raster

# total number of cells:
sum(cell_counts_bioclim_fut$count) + global(bioclim_fut_skeetch, fun="isNA") 
# 17 928, matches total number of cells from dimensions


# calculate difference in number of cells predicted as presence by each model:
diff_bioclim_pres_fut <- area_bioclim_fut - area_bioclim_pres
diff_bioclim_pres_fut
#  -3045.101, SSP 126 bioclim future model predicts less suitable area than bioclim present model
# 261.5886, SSP 585 bioclim future model predicts MORE suitable area than present

diff_bioclim_pres_fut <- diff_bioclim_pres_fut * cell_resolution_km
diff_bioclim_pres_fut
# -1766.217 km^2 SSP 126
# 151.7264 km^2 SSP 585


# Area of agreement between bioclim_present and bioclim_future models:

# here make new raster where 4 = both predicted, 1 = bioclim present only, 5 = bioclim future only, 6 =  both none

agree_bioclim_fut_pres <- rast(nrows = 166, ncols = 108, extent = ext(bioclim_pres_skeetch), crs = new_crs, vals = 0)

agree_bioclim_fut_pres[bioclim_pres_skeetch == 1 & bioclim_fut_skeetch == 1] <- 4
agree_bioclim_fut_pres[bioclim_pres_skeetch == 1 & bioclim_fut_skeetch == 2] <- 1
agree_bioclim_fut_pres[bioclim_pres_skeetch == 2 & bioclim_fut_skeetch == 1] <- 5
agree_bioclim_fut_pres[bioclim_pres_skeetch == 2 & bioclim_fut_skeetch == 2] <- 6 

agree_bioclim_fut_pres
plot(agree_bioclim_fut_pres)
##### SSP 126: no cells with future only prediction (no cells with value 5 as assigned above) #####
writeRaster(agree_bioclim_fut_pres, filename = "outputs/agreement_bioclim_fut_pres_skeetch_585.tif", overwrite = TRUE)

cell_counts_bioclim_fut_pres <- freq(agree_bioclim_fut_pres)
cell_counts_bioclim_fut_pres
sum(cell_counts_bioclim_fut_pres$count)
# 17 928


# number of cells classified as presence for both models (row 3):
agreement_future_cells <- cell_counts_bioclim_fut_pres$count[3]
agreement_future_cells
# 3561 cells SSP 126
# 7464 cells SSP 585

# area of cells classified as presence for both models (overlap only):
area_agreement_future <- agreement_future_cells * cell_resolution_km
area_agreement_future
# 2065.448 km^2 SSP 126
# 4329.263 km^2 SSP 585


# total suitable habitat by informed and bioclim_present models:
total_suitable_present <- sum(cell_counts_informed_bioclim_pres$count[2:4]) 
total_suitable_present
# 11 203 cells
area_total_suitable_present <- total_suitable_present * cell_resolution_km
area_total_suitable_present
# 6497.955 km^2


# total suitable habitat by bioclim present and future models:
# note: below was from rows 2:3 for SSP 126 calculations
total_suitable_future <- sum(cell_counts_bioclim_fut_pres$count[2:4])
total_suitable_future
# 8811 cells SSP 126
# 10 609 cells SSP 585
area_total_suitable_future <- total_suitable_future * cell_resolution_km
area_total_suitable_future
# 5110.549 km^2 SSP 126, same as bioclim only calculated above - no additional area predicted by bioclim future
# 6153.423 km^2 SSP 585

# total suitable habitat difference between future and present predictions:
area_change_pres_fut_suitable <- area_total_suitable_future - area_total_suitable_present
area_change_pres_fut_suitable
# -1387.406 km^2 SSP 126
# -344.5314

# total area of Skeetch:
# used bioclim_fut_pres because the informed model introduces NAs for water bodies
area_skeetch <- sum(cell_counts_bioclim_fut_pres$count[2:5])
area_skeetch # NOT including NA values
# 12 396 cells
area_skeetch <- area_skeetch * cell_resolution_km
area_skeetch
# 7189.918 km^2

# areas used below include area of overlap 
# total predicted suitable area by each respective model

# percent of Skeetch predicted as suitable by informed model:
percent_skeetch_informed <- (area_informed / area_skeetch) * 100
percent_skeetch_informed
# 78.10584

# percent of Skeetch predicted as suitable by bioclim present model:
percent_skeetch_bioclim_pres <- (area_bioclim_pres / area_skeetch) * 100
percent_skeetch_bioclim_pres
# 71.07938

# percent of Skeetch predicted as suitable by overlap between informed
# and bioclim present models:
percent_skeetch_overlap_present <- (area_agreement_present / area_skeetch) * 100
percent_skeetch_overlap_present
# 58.293

# percent of Skeetch predicted as suitable by bioclim future model:
percent_skeetch_bioclim_fut <- (area_bioclim_fut / area_skeetch) * 100
percent_skeetch_bioclim_fut
# 28.72701 SSP 126
# 74.71765 SSP 585

# percent of Skeetch predicted as suitable by overlap between bioclim 
# present and future models:
percent_skeetch_overlap_future <- (area_agreement_future / area_skeetch) * 100
percent_skeetch_overlap_future
# 28.72701 SSP 126
# 60.21297 SSP 585

# percent of Skeetch changed from present to future:
# ***** NOTE: might axe this and proceeding calculation, unsure if it makes sense*****
percent_change_pres_fut <- (diff_bioclim_pres_fut / area_skeetch) * 100
percent_change_pres_fut
# -24.56519 SSP 126
# + 2.110267 SSP 585

# percent of total suitable habitat changed from present to future:
percent_change_suitable_pres_fut <- (area_change_pres_fut_suitable / area_total_suitable_future) * 100
percent_change_suitable_pres_fut
# -27.14788 SSP 126
# -5.59902 SSP 585

# overlap as a percent of total suitable habitat predicted by informed and bioclim present:
percent_overlap_suitable_pres <- (area_agreement_present / area_total_suitable_present) * 100
percent_overlap_suitable_pres
# 64.50058 

# overlap as a percent of total suitable habitat predicted by bioclim present and future:
percent_overlap_suitable_fut <- (area_agreement_future / area_total_suitable_future) * 100
percent_overlap_suitable_fut
# 40.41539 SSP 126
# 70.35536 SSP 585