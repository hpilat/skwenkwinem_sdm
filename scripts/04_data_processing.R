# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 04/07
# This script prepares spatial extent, occurrence records, and predictor data
# for input into the tidysdm pipeline
# Please first run these scripts in the following order:
# 01_data_download_ranunculus.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R

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


# import extent files (created in 03_cropped_extent.R)
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
na_bound_rast <- rast("data/extents/na_bound_rast.tif")

# import files (already downloaded in 01_data_download.R)
# raw files are appended with _na or _global, cropped files are not
skwenkwinem_vect <- vect("data/processed/skwenkwinem_masked.shp")
worldclim_future_na <- rast("data/raw/wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp126_2081-2100.tif")
anth_biome_na <- rast("data/raw/anthromes_EqArea.tif")
#climate_zones_na <- vect("data/raw/North_America_Climate_Zones.shp")
#ecoregions_na <- vect("data/raw/na_terrestrial_ecoregions_v2_level_iii_shapefile/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_TerrestrialEcoregions_LIII/data/NA_Terrestrial_Ecoregions_v2_level3.shp")
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")
soil_phh2o_0_5_global <- rast("data/raw/soil_world/phh2o_0-5cm_mean_30s.tif")
#soil_phh2o_5_15_global <- rast("data/raw/soil_world/phh2o_5-15cm_mean_30s.tif")
soil_temp_0_5_global <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
#soil_temp_5_15_global <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")
#watersheds_na <- vect("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")



### Occurrence Data ##

skwenkwinem_vect # WGS84, cropped in 03_cropped_extent.R
# Use na_bound_vect as a mask for ran_occ_vect
# already in WGS84 so we can mask at this point
# so observations outside of our study area are set to NA
skwenkwinem_masked <- mask(skwenkwinem_vect, na_bound_vect)

# write to file, ready for tidysdm so write to processed folder
writeVector(skwenkwinem_masked, "data/processed/skwenkwinem_masked.shp", overwrite = TRUE)



## Predictor Data ##



## WorldClim data, for use in Bioclim model:
# start with WorldClim data to get our desired resolution and dimensions for all
# other rasters


# Present Data:

worldclim_present_na_bio01 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
worldclim_present_bio01 <- crop(worldclim_present_na_bio01, na_bound_vect)

worldclim_present_na_bio02 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_2.tif")
worldclim_present_bio02 <- crop(worldclim_present_na_bio02, na_bound_vect)

worldclim_present_na_bio03 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_3.tif")
worldclim_present_bio03 <- crop(worldclim_present_na_bio03, na_bound_vect)

worldclim_present_na_bio04 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_4.tif")
worldclim_present_bio04 <- crop(worldclim_present_na_bio04, na_bound_vect)

worldclim_present_na_bio05 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_5.tif")
worldclim_present_bio05 <- crop(worldclim_present_na_bio05, na_bound_vect)

worldclim_present_na_bio06 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_6.tif")
worldclim_present_bio06 <- crop(worldclim_present_na_bio06, na_bound_vect)

worldclim_present_na_bio07 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_7.tif")
worldclim_present_bio07 <- crop(worldclim_present_na_bio07, na_bound_vect)

worldclim_present_na_bio08 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_8.tif")
worldclim_present_bio08 <- crop(worldclim_present_na_bio08, na_bound_vect)

worldclim_present_na_bio09 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_9.tif")
worldclim_present_bio09 <- crop(worldclim_present_na_bio09, na_bound_vect)

worldclim_present_na_bio10 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_10.tif")
worldclim_present_bio10 <- crop(worldclim_present_na_bio10, na_bound_vect)

worldclim_present_na_bio11 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_11.tif")
worldclim_present_bio11 <- crop(worldclim_present_na_bio11, na_bound_vect)

worldclim_present_na_bio12 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")
worldclim_present_bio12 <- crop(worldclim_present_na_bio12, na_bound_vect)

worldclim_present_na_bio13 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_13.tif")
worldclim_present_bio13 <- crop(worldclim_present_na_bio13, na_bound_vect)

worldclim_present_na_bio14 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_14.tif")
worldclim_present_bio14 <- crop(worldclim_present_na_bio14, na_bound_vect)

worldclim_present_na_bio15 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_15.tif")
worldclim_present_bio15 <- crop(worldclim_present_na_bio15, na_bound_vect)

worldclim_present_na_bio16 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_16.tif")
worldclim_present_bio16 <- crop(worldclim_present_na_bio16, na_bound_vect)

worldclim_present_na_bio17 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_17.tif")
worldclim_present_bio17 <- crop(worldclim_present_na_bio17, na_bound_vect)

worldclim_present_na_bio18 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_18.tif")
worldclim_present_bio18 <- crop(worldclim_present_na_bio18, na_bound_vect)

worldclim_present_na_bio19 <- rast("data/raw/wc2.1_30s_bio/wc2.1_30s_bio_19.tif")
worldclim_present_bio19 <- crop(worldclim_present_na_bio19, na_bound_vect)

# create a multilayer raster
worldclim_present <- c(worldclim_present_bio01,
                       worldclim_present_bio02,
                       worldclim_present_bio03,
                       worldclim_present_bio04,
                       worldclim_present_bio05,
                       worldclim_present_bio06,
                       worldclim_present_bio07,
                       worldclim_present_bio08,
                       worldclim_present_bio09,
                       worldclim_present_bio10,
                       worldclim_present_bio11,
                       worldclim_present_bio12,
                       worldclim_present_bio13,
                       worldclim_present_bio14,
                       worldclim_present_bio15,
                       worldclim_present_bio16,
                       worldclim_present_bio17,
                       worldclim_present_bio18,
                       worldclim_present_bio19)


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
writeRaster(worldclim_present_masked, filename = "data/processed/worldclim_present_masked.tif", overwrite = TRUE)
worldclim_present_masked <- rast("data/processed/worldclim_present_masked.tif")

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
writeRaster(worldclim_future_masked, filename = "data/processed/worldclim_future_masked.tif", overwrite = TRUE)

# now that we have a layer with our goal resolution and extent, 
# can resample our empty raster (created in 03_cropped_extent.R) 
# to have the correct resolution
na_bound_rast <- resample(na_bound_rast, worldclim_present_masked)
na_bound_rast
# write to file
#writeRaster(na_bound_rast, filename = "data/extents/na_bound_rast.tif", overwrite = TRUE)


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
writeRaster(soil_temp_0_5, filename = "data/processed/soil_temp_0_5.tif", overwrite = TRUE)
#writeRaster(soil_temp_5_15, filename = "data/processed/soil_temp_5_15.tif", overwrite = TRUE)


# Soil pH

soil_phh2o_0_5_global
#soil_phh2o_5_15_global
# correct CRS and resolution, just need to crop

# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5_global, na_bound_vect)
#soil_phh2o_5_15 <- crop(soil_phh2o_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_phh2o_0_5) <- "soil_phh2o_0_5"
#names(soil_phh2o_5_15) <- "soil_phh2o_5_15"

# write processed soil pH data to file for faster computation
writeRaster(soil_phh2o_0_5, filename = "data/processed/soil_phh2o_0_5.tif", overwrite = TRUE)
#writeRaster(soil_phh2o_5_15, filename = "data/processed/soil_phh2o_5_15.tif", overwrite = TRUE)


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
writeRaster(elevation, filename = "data/processed/elevation.tif", overwrite = TRUE)



## For each of the remaining layers, we need to (not necesarily in this order):

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
# I created a CSV file from the metadata document
landcover_categories <- read.csv("data/raw/landcover_categories.csv", header = T, strip.white = TRUE)

# note there is no category 4, so remove this row from names.
levels(landcover_cat) <- landcover_categories[-4,]

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

writeRaster(elevation, filename = "data/processed/elevation.tif", overwrite = TRUE)

writeRaster(landcover_cat, filename = "data/processed/landcover_cat.tif", overwrite = TRUE)

writeRaster(anth_biome_cat, filename = "data/processed/anth_biome_cat.tif", overwrite = TRUE)

#writeRaster(climate_zones_cat, filename = "data/processed/climate_zones_cat.tif", overwrite = TRUE)

#writeRaster(ecoregions_cat, filename = "data/processed/ecoregions_cat.tif", overwrite = TRUE)

#writeRaster(watersheds_cat, filename = "data/processed/watersheds_cat.tif", overwrite = TRUE)


# now create a multilayer SpatRaster with all the above rasters

predictors_multi <- c(anth_biome_cat, 
                     # climate_zones_cat, 
                     # ecoregions_cat, 
                      elevation, 
                      landcover_cat,
                      soil_phh2o_0_5, 
                      #soil_phh2o_5_15, 
                      soil_temp_0_5)#, 
                      # soil_temp_5_15,  
                     # watersheds_cat)

# mask the multilayer raster so the values outside of na_bound are NA
predictors_multi <- mask(predictors_multi, na_bound_vect)

# write the multilayer raster to file for reuse
writeRaster(predictors_multi, 
            filename = "data/processed/predictors_multi.tif", 
            overwrite = TRUE)