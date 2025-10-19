# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 04/10
# This script prepares spatial extent, occurrence records, and predictor data
# for input into the tidysdm pipeline
# Please first run these scripts in the following order:
# 01_data_download_ranunculus.R
# 02_continental_divide.Rmd
# 03a_cropped_extent.R
# [03b is not necessary to run]

# goal is to have raster files in WGS84, with 0.0083 resolution, 
# cropped and masked to na_bound_vect
# order of operations is: 1- reproject to WGS84, 2- resample to 0.0083 resolution, 
# and 3- crop to na_bound_vect
# landcover data is very large and needed to be aggregated first in order to do 
# any of the above operations
# tidysdm also requires layer names to match object names, so we'll change those too
# tidysdm requires categorical rasters be coded as numeric, 
# so we'll code categorical raster values to numeric


library(tidyverse)
library(sf)
library(terra)
library(here)
library(purrr)

# import extent files (created in 03_cropped_extent.R)
na_bound_vect <- vect(here::here("data", "extents", "na_bound_vect.shp"))
na_bound_rast <- rast(here::here("data", "extents", "na_bound_rast.tif"))

# import files (already downloaded in 01_data_download.R)
# raw files are appended with _na or _global, cropped files are not

# ** Jason changed skwenkwinem_masked.shp to "skwenkwinem_sf.shp"
skwenkwinem_vect <- vect(here::here("data", "processed", "skwenkwinem_sf.shp"))

# ** Jason: this TIF file is different from what's referenced in preceding scripts
# Corrected

worldclim_future_na <- rast(here::here("data", "raw", "wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp585_2081-2100.tif"))

anth_biome_na <- rast(here::here("data", "raw", "anthropogenicbiomes_2008_tif", "anthropogenicbiomes", "data", "anthromes_EqArea.tif"))

#climate_zones_na <- vect(here::here("data", "raw", "North_America_Climate_Zones.shp")
#ecoregions_na <- vect(here::here("data", "raw", "na_terrestrial_ecoregions_v2_level_iii_shapefile/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_TerrestrialEcoregions_LIII/data/NA_Terrestrial_Ecoregions_v2_level3.shp")

elevation_na <- rast(here::here("data", "raw", "Elevation_TIF", "NA_Elevation", "data", "northamerica", "northamerica_elevation_cec_2023.tif"))

lndcvr_na <- rast(here::here("data", "raw", "land_cover_2020v2_30m_tif", "NA_NALCMS_landcover_2020v2_30m", "data", "NA_NALCMS_landcover_2020v2_30m.tif"))

soil_phh2o_0_5_global <- rast(here::here("data", "raw", "phh2o_0-5cm_mean_30s.tif"))

#soil_phh2o_5_15_global <- rast(here::here("data", "raw", "soil_world/phh2o_5-15cm_mean_30s.tif"))

soil_temp_0_5_global <- rast(here::here("data", "raw", "SBIO4_0_5cm_Temperature_Seasonality.tif"))

#soil_temp_5_15_global <- rast(here::here("data", "raw", "SBIO4_5_15cm_Temperature_Seasonality.tif"))
#watersheds_na <- vect(here::here("data", "raw", "watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp"))



### Occurrence Data ##

skwenkwinem_vect
# needs to be WGS84, cropped in 03_cropped_extent.R
skwenkwinem_vect <- terra::project(skwenkwinem_vect, na_bound_vect)
# Use na_bound_vect as a mask for ran_occ_vect
# already in WGS84 so we can mask at this point
# so observations outside of our study area are set to NA
skwenkwinem_masked <- mask(skwenkwinem_vect, na_bound_vect)

# write to file, ready for tidysdm so write to processed folder
terra::writeVector(skwenkwinem_masked, here::here("data", "processed", "skwenkwinem_masked.shp"), overwrite = TRUE)

## Predictor Data ##

## WorldClim data, for use in Bioclim model:
# start with WorldClim data to get our desired resolution and dimensions for all
# other rasters

# Function to import all bioclim layers and crop
load_and_crop_bioclim <- function(bio_nums = 1:19, boundary_vect, base_path = here("data", "raw", "worldclim_bio")) {
  map(bio_nums, function(i) {
    file_path <- file.path(base_path, sprintf("wc2.1_30s_bio_%d.tif", i))
    cropped_rast <- crop(rast(file_path), boundary_vect)
    return(cropped_rast)
  }) |> set_names(sprintf("bio%02d", bio_nums))
}

# run function:
worldclim_present_bio_list <- load_and_crop_bioclim(boundary_vect = na_bound_vect)

# stack layers
worldclim_present <- rast(worldclim_present_bio_list)


worldclim_present # CRS and resolution match what we need
# mask to the study area polygon
worldclim_present_masked <- mask(worldclim_present, na_bound_vect)

# change layer names to simpler name
names(worldclim_present_masked) <- c("bio01", "bio02", "bio03", "bio04", "bio05",
                                     "bio06", "bio07", "bio08", "bio09", "bio10", 
                                     "bio11", "bio12", "bio13", "bio14", "bio15", 
                                     "bio16", "bio17", "bio18", "bio19")
worldclim_present_masked

# write to file for reuse in 05_tidysdm_bioclim_30s.R
writeRaster(worldclim_present_masked, filename = here::here("data", "processed", "worldclim_present_masked.tif"), overwrite = TRUE)
#worldclim_present_masked <- rast(here::here("data", "processed", "worldclim_present_masked.tif"))

# Future Data:

worldclim_future_na # correct resolution and CRS, need to crop
worldclim_future_cropped <- crop(worldclim_future_na, na_bound_vect)
# now mask so values outside na_bound_vect are set to NA
worldclim_future_masked <- mask(worldclim_future_cropped, na_bound_vect)

# change layer names to simpler names, and to match present climate dataset (above)
names(worldclim_future_masked) <- c("bio01", "bio02", "bio03", "bio04", "bio05",
                                    "bio06", "bio07", "bio08", "bio09", "bio10", 
                                    "bio11", "bio12", "bio13", "bio14", "bio15", 
                                    "bio16", "bio17", "bio18", "bio19")

# write to file for reuse in 05_tidysdm_bioclim_30s
writeRaster(worldclim_future_masked, filename = here::here("data", "processed", "worldclim_future_585_masked.tif"), overwrite = TRUE)

# now that we have a layer with our goal resolution and extent, 
# can resample our empty raster (created in 03a_cropped_extent.R) 
# to have the correct resolution
na_bound_rast <- resample(na_bound_rast, worldclim_present_masked)
na_bound_rast
# write to file
writeRaster(na_bound_rast, filename = here::here("data", "extents", "na_bound_rast.tif"), overwrite = TRUE)


# Informed Data (for use in Informed Model)

## Numeric Rasters:

## Soil Temperature

soil_temp_0_5_global
#soil_temp_5_15_global
# correct CRS and resolution, just need to crop

# crop soil temperature SpatRaster to North American extent
soil_temp_0_5 <- crop(soil_temp_0_5_global, na_bound_vect)
#soil_temp_5_15 <- crop(soil_temp_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_temp_0_5) <- "soil_temp_0_5"
#names(soil_temp_5_15) <- "soil_temp_5_15"

# write processed data to file for faster computation
writeRaster(soil_temp_0_5, filename = here::here("data", "processed", "soil_temp_0_5.tif"), overwrite = TRUE)
#writeRaster(soil_temp_5_15, filename = here::here("data", "processed", "soil_temp_5_15.tif", overwrite = TRUE)


# Soil pH

soil_phh2o_0_5_global
#soil_phh2o_5_15_global
# correct WGS84 (** only if downloaded from UCDavis site, NOT Soilgrids)

# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5_global, na_bound_vect)
#soil_phh2o_5_15 <- crop(soil_phh2o_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_phh2o_0_5) <- "soil_phh2o_0_5"
#names(soil_phh2o_5_15) <- "soil_phh2o_5_15"

# write processed soil pH data to file for faster computation
writeRaster(soil_phh2o_0_5, filename = here::here("data", "processed", "soil_phh2o_0_5.tif"), overwrite = TRUE)
#writeRaster(soil_phh2o_5_15, filename = here::here("data", "processed", "soil_phh2o_5_15.tif", overwrite = TRUE)


# Elevation:

elevation_na
# incorrect CRS and resolution

# reproject elevation data to WGS84
elevation_naWGS <- terra::project(elevation_na, "EPSG:4326", method = "bilinear")

# resample elevation_na to change resolution
# use na_bound_rast from earlier as template, as it has our target specs
elevation_na <- terra::resample(elevation_naWGS, na_bound_rast)

# crop elevation data to study area extent
elevation <- crop(elevation_na, na_bound_vect)

# change the layer name to match object name
names(elevation) <- "elevation"

# write elevation to file for easier reuse
writeRaster(elevation, filename = here::here("data", "processed", "elevation.tif"), overwrite = TRUE)


## For each of the remaining layers, we need to (not necessarily in this order):

## 1. Reproject to correct CRS
## 2. Crop to study extent
## 3. Rasterize vector files
## 4. Resample to correct resolution
## 5. Ensure that categorical raster are properly categorical
##    - and ensure they have proper categorical levels
## 6. Mask rasters

## First deal with Vector layers

## Reproject to WGS84
# we can do this on vectors without first cropping, because vectors are fast
#climate_zones_wgs <- terra::project(climate_zones_na, "EPSG:4326")
#ecoregions_wgs <- terra::project(ecoregions_na, "EPSG:4326")
#watersheds_wgs <- terra::project(watersheds_na, "EPSG:4326")

# for rasters, it's best to clip to study region first, then reproject
# so let's reproject study region vector to appropriate CRS for each raster

na_bound_anth <- terra::project(na_bound_vect, crs(anth_biome_na))
na_bound_land <- terra::project(na_bound_vect, crs(lndcvr_na))
na_bound_elev <- terra::project(na_bound_vect, crs(elevation_na))

# now crop to study region extent
anth_biome_crop <- terra::crop(anth_biome_na, na_bound_anth)
elevation_crop <- terra::crop(elevation_na, na_bound_elev)
lndcvr_crop <- terra::crop(lndcvr_na, na_bound_land)

## REPROJECT rasters
# ensuring appropriate method depending on numerical vs categorical

# anthropogenic biomes data to WGS84
# this is categorical, so use "near"
anth_biome_wgs_crop <- terra::project(anth_biome_crop, "EPSG:4326", method = "near", threads = 4)

# elevation is numeric, so use "bilinear"
elevation_wgs_crop <- terra::project(elevation_crop, "EPSG:4326", method = "bilinear", threads = 4)

# For Landcover, we need to first aggregate before reprojecting, because big!
# selected a factor of 15 - reduce # of cells by 15 times
lndcvr_na_agg <- aggregate(lndcvr_crop, fun = "modal", fact = 15, cores = 4)
# reproject landcover North America data to WGS84
lndcvr_wgs_crop <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near", threads = 4)

## Now crop the vector files to study extent
#climate_zones_wgs_crop <- crop(climate_zones_wgs, na_bound_vect)
#ecoregions_wgs_crop <- crop(ecoregions_wgs, na_bound_vect)
#watersheds_wgs_crop <- crop(watersheds_wgs, na_bound_vect)

### NOW RASTERIZE vector files

# create raster from SpatVector and structure of na_bound_rast
# use na_bound_rast as template
#climate_zones_rast <- terra::rasterize(climate_zones_wgs_crop, na_bound_rast, field = "Climate")
#ecoregions_rast <- rasterize(ecoregions_wgs_crop, na_bound_rast, field = "NameL3_En")
#watersheds_rast <- rasterize(watersheds_wgs_crop, na_bound_rast, field = "NAW4_EN")

# NOTE that the above rasters are all properly categorical (factor) rasters

## RESAMPLE RASTERS to common CRS
## being sure to use proper "method" depending on data type (categorical vs numerical)

elevation <- terra::resample(elevation_wgs_crop, na_bound_rast, method = "bilinear", threads = 4)
landcover <- terra::resample(lndcvr_wgs_crop, na_bound_rast, method = "mode", threads = 4)
anth_biome <- terra::resample(anth_biome_wgs_crop, na_bound_rast, method = "mode", threads = 4)

#climate_zones <- resample(climate_zones_rast, na_bound_rast, method = "mode", threads = 4)
#ecoregions <- terra::resample(ecoregions_rast, na_bound_rast, method = "mode", threads = 4)
#watersheds <- terra::resample(watersheds_rast, na_bound_rast, method = "mode", threads = 4)

## Ensure CATEGORIES are named (all but the elevation layer)

## LANDCOVER
# first, convert this to categorical:
landcover_cat <- as.factor(landcover)

# assign colour table to landcover
coltab(landcover_cat) <- coltab(lndcvr_na_agg)

# get category names for land covers
# can get these from the "lndcvr_na_agg" object

library(dplyr)

# Assuming your dataframes are named:
# lookup_df = first dataframe (with Value and Class_EN)
# main_df = second dataframe (with ID and Class_EN to be replaced)

# First, rename the Class_EN column in the second dataframe to avoid confusion
levels(landcover_cat)[[1]] <- levels(landcover_cat)[[1]] %>%
  left_join(levels(lndcvr_na_agg)[[1]], by = c("ID" = "Value")) %>%
  rename(Class_Code = Class_EN.x, Class_EN = Class_EN.y) %>%
  select(ID, Class_EN)

# To check that categories worked, uncomment this:

# # Define a smaller extent (adjust as needed)
# small_extent <- ext(-120, -119, 50, 51)
# # Crop the raster
# landcover_small <- crop(landcover_cat, small_extent)
# # Plot the result
# plot(landcover_small, main = "Subset of Landcover Raster")


## ANTH BIOME
anth_biome_cat <- as.factor(anth_biome)
levels(anth_biome_cat) <- levels(anth_biome_na)

## CLIMATE ZONES

#climate_zones_cat <- as.factor(climate_zones)
#levels(climate_zones_cat) <- levels(climate_zones_rast)

## ECOREGIONS
#ecoregions_cat <- as.factor(ecoregions)
#levels(ecoregions_cat) <- levels(ecoregions_rast)

## WATERSHEDS
#watersheds_cat <- as.factor(watersheds)
#levels(watersheds_cat) <- levels(watersheds_rast)


# change the layer name to match object name
names(elevation) <- "elevation"
names(landcover_cat) <- "landcover"
names(anth_biome_cat) <- "anth_biome"
#names(climate_zones_cat) <- "climate_zones"
#names(ecoregions_cat) <- "ecoregions"
#names(watersheds_cat) <- "watersheds"

## WRITE FILES

writeRaster(elevation, filename = here::here("data", "processed", "elevation.tif"), overwrite = TRUE)

writeRaster(landcover_cat, filename = here::here("data", "processed", "landcover_cat.tif"), overwrite = TRUE)

writeRaster(anth_biome_cat, filename = here::here("data", "processed", "anth_biome_cat.tif"), overwrite = TRUE)

#writeRaster(climate_zones_cat, filename = here::here("data", "processed", "climate_zones_cat.tif", overwrite = TRUE)

#writeRaster(ecoregions_cat, filename = here::here("data", "processed", "ecoregions_cat.tif", overwrite = TRUE)

#writeRaster(watersheds_cat, filename = here::here("data", "processed", "watersheds_cat.tif", overwrite = TRUE)


# now create a multilayer SpatRaster with all the above rasters

predictors_multi <- c(anth_biome_cat, 
                     #climate_zones_cat, 
                     #ecoregions_cat, 
                      elevation, 
                      landcover_cat,
                      soil_phh2o_0_5, 
                      #soil_phh2o_5_15, 
                      soil_temp_0_5)#, 
                      #soil_temp_5_15,  
                      #watersheds_cat)

# mask the multilayer raster so the values outside of na_bound are NA
predictors_multi <- mask(predictors_multi, na_bound_vect)


# First change names of variables in predictors

names(predictors_multi) <- c("anth_biome", "elevation", "landcover", "soil_phh2o_0_5", "soil_temp_0_5")

# convert soil_temp_0_5 to °C (values are standard deviation X 100)
predictors_multi$soil_temp_0_5 <- predictors_multi$soil_temp_0_5/100


# write the multilayer raster to file for reuse
writeRaster(predictors_multi, 
            filename = here::here("data", "processed", "predictors_multi.tif"), 
            overwrite = TRUE)