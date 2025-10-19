# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 08/10
# This script calculates area predicted to be suitable for skwenkwinem
# over our full study extent
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03a_cropped_extent.R
# [03b is not necessary to run]
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R
# [07_continuous_plots.R] not required

library(terra)
library(dplyr)
library(here)

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


# Informed vs Bioclim Present:


informed_albers
# dimensions:
total_cells <- 4661 * 4187
total_cells
# 19515607 cells

# resolution: 761.5899 m X 761.5899 m
cell_resolution_m <- 761.5899 * 761.5899
cell_resolution_km <- cell_resolution_m / 1000000 # 1 000 000 m^2 in 1 km^2

# number of cells in each category:
cell_counts_informed <- freq(informed_albers)
cell_counts_informed
# ORIGINAL:
# layer     value   count
# 1     1  presence 2189972
# 2     1 pseudoabs 4590676

# OCTOBER RUN:
# layer     value   count
# 1     1  presence 2249723
# 2     1 pseudoabs 4530925

# number of cells classified as presence (row 1):
informed_presence_cells <- cell_counts_informed$count[1]
informed_presence_cells
# 2339340
# updated: 2189972
# October:2249723

# area of cells classified as presence:
area_informed <- informed_presence_cells * cell_resolution_km
area_informed
# 1 356 8625 km^2
# updated: 1,270,226 km^2
# October: 1,304,882 km^2

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_informed$count)
# 6 781 110
# udpated/October: 6,780,648

# how many cells are NA?
global(informed_albers, fun="isNA")
# 12 734 497
# updated/October: 12,734,959

# total number of cells:
sum(cell_counts_informed$count) + global(informed_albers, fun="isNA") 
# 19 515 607, matches total number of cells from dimensions
# updated/October: 19,515,607

bioclim_pres_albers
# dimensions:
total_cells <- 4661 * 4187
total_cells
# 19 515 607 cells

# number of cells in each category:
cell_counts_bioclim_pres <- freq(bioclim_pres_albers)
cell_counts_bioclim_pres
# ORIGINAL:
# layer     value   count
# 1     1  presence 1921691
# 2     1 pseudoabs 5015229

# OCTOBER:
# layer     value   count
# 1     1  presence 1948317
# 2     1 pseudoabs 4988603

# number of cells classified as presence (row 1):
bioclim_pres_presence_cells <- cell_counts_bioclim_pres$count[1]
bioclim_pres_presence_cells
# 2 074 457 cells
# updated: 1,921,691
# October: 1,948,317

# area of cells classified as presence:
area_bioclim_pres <- bioclim_pres_presence_cells * cell_resolution_km
area_bioclim_pres
# 1 203 225 km^2
# updated: 1,114,618
# October: 1,130,061

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_bioclim_pres$count)
# 6 936 918
# updated/October: 6936920

# how many cells are NA?
global(bioclim_pres_albers, fun="isNA")
# 12 578 689 - fewer NAs than informed raster
# updated/October: 12,578,687

# total number of cells:
sum(cell_counts_bioclim_pres$count) + global(bioclim_pres_albers, fun="isNA") 
# 19 515 607, matches total number of cells from dimensions


# calculate difference in area predicted as presence by each model:
# row 1, column 4 is difference in presence area in km^2
diff_informed_bioclim_pres <- area_informed - area_bioclim_pres
diff_informed_bioclim_pres
# 153 637.2, informed model predicts more suitable area than bioclim model

# updated: 155,608.1
# October: 174,821.3

# Area of agreement between informed and bioclim_present models:

# here make new raster where 2 = both predicted, 1 = bioclim present only, 3 = informed only, 6 =  both none

agree_informed_bioclim <- rast(nrows = 4661, ncols = 4187, extent = ext(bioclim_pres_albers), crs = new_crs, vals = 0)

agree_informed_bioclim[bioclim_pres_albers == 1 & informed_albers == 1] <- 2
agree_informed_bioclim[bioclim_pres_albers == 1 & informed_albers == 2] <- 1
agree_informed_bioclim[bioclim_pres_albers == 2 & informed_albers == 1] <- 3
agree_informed_bioclim[bioclim_pres_albers == 2 & informed_albers == 2] <- 6

plot(agree_informed_bioclim)
writeRaster(agree_informed_bioclim, filename = "outputs/agreement_informed_bioclim.tif", overwrite = TRUE)

cell_counts_informed_bioclim_pres <- freq(agree_informed_bioclim)
cell_counts_informed_bioclim_pres
sum(cell_counts_informed_bioclim_pres$count)
# 19 515 607

# number of cells classified as presence for both models (row 3):
agreement_present_cells <- cell_counts_informed_bioclim_pres$count[3]
agreement_present_cells
# 1 555 793
# updated: 1,431,413
# October: 1,474,762

# area of cells classified as presence for both models:
area_agreement_present <- agreement_present_cells * cell_resolution_km
area_agreement_present
# 902 389.8 km^2
# updated: 830,247 
# October: 855,390.2


# Bioclim Present vs Future:

bioclim_fut_albers
# dimensions:
total_cells <- 4661 * 4187
total_cells
# 19 515 607 cells

# number of cells in each category:
cell_counts_bioclim_fut <- freq(bioclim_fut_albers)
cell_counts_bioclim_fut
# ORIGINAL:
# layer     value   count
# 1     1  presence 1884148
# 2     1 pseudoabs 5052772

# OCTOBER:
# layer     value   count
# 1     1  presence 1775216
# 2     1 pseudoabs 5161704

# number of cells classified as presence (row 1):
bioclim_fut_presence_cells <- cell_counts_bioclim_fut$count[1]
bioclim_fut_presence_cells
# 1 889 663 cells with SSP 1-2.6
# updated: 1,884,148
# October: 1,775,216 with SSP 5-8.5

# area of cells classified as presence:
area_bioclim_fut <- bioclim_fut_presence_cells * cell_resolution_km
area_bioclim_fut
# 1 096 041 km^2 with SSP 1-2.6
# updated: 1,092,842
# October: 1,029,659 km^2 with SSP 5-8.5

# total number of cells (presence and pseudoabsence, not including NAs):
sum(cell_counts_bioclim_fut$count)
# 6 936 918
# updated/October: 6,936,920

# how many cells are NA?
global(bioclim_fut_albers, fun="isNA")
# 12 578 689 - fewer NAs than informed raster
# updated/October: 12,578,687

# total number of cells:
sum(cell_counts_bioclim_fut$count) + global(bioclim_fut_albers, fun="isNA") 
# 19 515 607, matches total number of cells from dimensions


# calculate difference in number of cells predicted as presence by each model:
diff_bioclim_pres_fut <- area_bioclim_fut - area_bioclim_pres
diff_bioclim_pres_fut
# updated: -21775.66. ??
# October: -100,401.9 bioclim future predicts 100401.9 km^2 less than present

# -107 184.1, bioclim future model (SSP 1-2.6) predicts less suitable area than bioclim present model
# -24 240.16, bioclim future model (SSP 5-8.5) predicts less suitable area than bioclim present
# October: -100,401.9 bioclim future (SSP 5-8.5) predicts less suitable area than bioclim present

# Area of agreement between bioclim_present and bioclim_future models:

# here make new raster where 4 = both predicted, 1 = bioclim present only, 5 = bioclim future only, 6 =  both none

agree_bioclim_fut_pres <- rast(nrows = 4661, ncols = 4187, extent = ext(bioclim_pres_albers), crs = new_crs, vals = 0)

agree_bioclim_fut_pres[bioclim_pres_albers == 1 & bioclim_fut_albers == 1] <- 4
agree_bioclim_fut_pres[bioclim_pres_albers == 1 & bioclim_fut_albers == 2] <- 1
agree_bioclim_fut_pres[bioclim_pres_albers == 2 & bioclim_fut_albers == 1] <- 5
agree_bioclim_fut_pres[bioclim_pres_albers == 2 & bioclim_fut_albers == 2] <- 6 

agree_bioclim_fut_pres
plot(agree_bioclim_fut_pres)
writeRaster(agree_bioclim_fut_pres, filename = "outputs/agreement_bioclim_fut_pres_585.tif", overwrite = TRUE)

cell_counts_bioclim_fut_pres <- freq(agree_bioclim_fut_pres)
cell_counts_bioclim_fut_pres
sum(cell_counts_bioclim_fut_pres$count)
# 19 515 607

# number of cells classified as presence for both models (row 3):
agreement_future_cells <- cell_counts_bioclim_fut_pres$count[3]
agreement_future_cells
# 1 493 108 SSP 1-2.6
# 1 679 553 SSP 5-8.5
# update: 1,406,923
# October: 1,361,791

# area of cells classified as presence for both models (overlap only):
area_agreement_future <- agreement_future_cells * cell_resolution_km
area_agreement_future
# 866 031.3 km^2 SSP 1-2.6
# 974 172.9 km^2 SSP 5-8.5
# October: 789,864.9

#update: 816,042.3

# total suitable habitat by informed and bioclim_present models:
total_suitable_present <- sum(cell_counts_informed_bioclim_pres$count[2:4]) 
total_suitable_present
# 2 810 300 cells
# update: 2,652,934
# October: 2,693,740

area_total_suitable_present <- total_suitable_present * cell_resolution_km
area_total_suitable_present
# 1 630 028 km^2
# update: 1,538,753
# October: 1,562,421

# total suitable habitat by bioclim present and future models:
total_suitable_future <- sum(cell_counts_bioclim_fut_pres$count[2:4])
total_suitable_future
# 2 471 012 cells SSP 1-2.6
# 2 427 569 cells SSP 5-8.5
# update: 2,398,916
# October: 2,361,742

area_total_suitable_future <- total_suitable_future * cell_resolution_km
area_total_suitable_future
# 1 433 234 km^2 SSP 1-2.6
# 1 408 037 km^2 SSP 5-8.5
# update: 1,391,417
# October: 1,369,856

# total suitable habitat difference between future and present predictions:
area_change_pres_fut_suitable <- area_total_suitable_future - area_total_suitable_present
area_change_pres_fut_suitable
# -196 793.5 km^2 SSP 1-2.6
# -221 991.3 km^2 SSP 5-8.5
# update: -147,335.3
# October: -192,565.2

# total area of our study extent:
area_full_extent <- sum(cell_counts_bioclim_fut_pres$count[2:5])
area_full_extent # NOT including NA values
# 6 936 918 cells
# update/October: 6,936,920

area_full_extent <- area_full_extent * cell_resolution_km
area_full_extent
# 4 023 545 km^2
# update/October: 4,023,547

# areas used below include area of overlap 
# total predicted suitable area by each respective model

# percent of study extent predicted as suitable by informed model:
percent_full_informed <- (area_informed / area_full_extent) * 100
percent_full_informed
# 33.72305
# update: 31.5698
# October: 32.43115

# percent of study extent predicted as suitable by bioclim present model:
percent_full_bioclim_pres <- (area_bioclim_pres / area_full_extent) * 100
percent_full_bioclim_pres
# 29.90459
# update: 27.70237
# October: 28.0862

# percent of study extent predicted as suitable by overlap between informed
# and bioclim present models:
percent_full_overlap_present <- (area_agreement_present / area_full_extent) * 100
percent_full_overlap_present
# 22.42773
# update: 20.63471
# October: 21.25961

# percent of study extent predicted as suitable by bioclim future model:
percent_full_bioclim_fut <- (area_bioclim_fut / area_full_extent) * 100
percent_full_bioclim_fut
# 27.24067 SSP 1-2.6
# 29.30213 SSP 5-8.5
# update: 27.16116
# October: 25.59084

# percent of study extent predicted as suitable by overlap between bioclim 
# present and future models:
percent_full_overlap_future <- (area_agreement_future / area_full_extent) * 100
percent_full_overlap_future
# 21.52408 SSP 1-2.6
# 24.2118 SSP 5-8.5
# update: 20.28167
# October: 19.63106

# percent of study extent changed from present to future:
percent_change_pres_fut <- (diff_bioclim_pres_fut / area_full_extent) * 100
percent_change_pres_fut
# -2.663921 SSP 1-2.6
# -0.6024577 SSP 5-8.5
# update: -0.5412056
# October:-2.495358

# percent of total suitable habitat changed from present to future:
percent_change_suitable_pres_fut <- (area_change_pres_fut_suitable / area_total_suitable_future) * 100
percent_change_suitable_pres_fut
# -13.73073 SSP 1-2.6
# -15.76602 SSP 5-8.5
# update: -10.58887
# October: -14.05734

# overlap as a percent of total suitable habitat predicted by informed and bioclim present:
percent_overlap_suitable_pres <- (area_agreement_present / area_total_suitable_present) * 100
percent_overlap_suitable_pres
# 55.36039
# update: 53.95585
# October: 54.74775

# overlap as a percent of total suitable habitat predicted by bioclim present and future:
percent_overlap_suitable_fut <- (area_agreement_future / area_total_suitable_future) * 100
percent_overlap_suitable_fut
# 60.42496 SSP 1-2.6
# 69.18662 SSP 5-8.5
# update: 58.64828
# October: 57.66045
