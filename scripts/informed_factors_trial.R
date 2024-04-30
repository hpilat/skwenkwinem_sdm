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
climate_zones_na <- vect("data/raw/North_America_Climate_Zones.shp")
ecoregions_na <- vect("data/raw/na_terrestrial_ecoregions_v2_level_iii_shapefile/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_TerrestrialEcoregions_LIII/data/NA_Terrestrial_Ecoregions_v2_level3.shp")
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")
soil_phh2o_0_5_global <- rast("data/raw/soil_world/phh2o_0-5cm_mean_30s.tif")
soil_phh2o_5_15_global <- rast("data/raw/soil_world/phh2o_5-15cm_mean_30s.tif")
soil_temp_0_5_global <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
soil_temp_5_15_global <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")
watersheds_na <- vect("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")



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
writeRaster(na_bound_rast, filename = "data/extents/na_bound_rast.tif", overwrite = TRUE)

na_bound_rast <- rast("data/extents/na_bound_rast.tif")


# Informed Data (for use in Informed Model)


## Numeric Rasters:


## Soil Temperature

soil_temp_0_5_global
soil_temp_5_15_global
# correct CRS and resolution, just need to crop

# crop soil temperature SpatRaster to North American extent
soil_temp_0_5 <- crop(soil_temp_0_5_global, na_bound_vect)
soil_temp_5_15 <- crop(soil_temp_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_temp_0_5) <- "soil_temp_0_5"
names(soil_temp_5_15) <- "soil_temp_5_15"

# write processed data to file for faster computation
writeRaster(soil_temp_0_5, filename = "data/processed/soil_temp_0_5.tif", overwrite = TRUE)
writeRaster(soil_temp_5_15, filename = "data/processed/soil_temp_5_15.tif", overwrite = TRUE)


# Soil pH

soil_phh2o_0_5_global
soil_phh2o_5_15_global
# correct CRS and resolution, just need to crop

# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5_global, na_bound_vect)
soil_phh2o_5_15 <- crop(soil_phh2o_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_phh2o_0_5) <- "soil_phh2o_0_5"
names(soil_phh2o_5_15) <- "soil_phh2o_5_15"

# write processed soil pH data to file for faster computation
writeRaster(soil_phh2o_0_5, filename = "data/processed/soil_phh2o_0_5.tif", overwrite = TRUE)
writeRaster(soil_phh2o_5_15, filename = "data/processed/soil_phh2o_5_15.tif", overwrite = TRUE)


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



## Categorical Rasters


# Create function to create multiple binary rasters from each level of the
  # categorical raster
create_multilayer_raster <- function(raster_name) {
  # "raster_name" is the name of the categorical raster object
  require(terra)
  # Check if the raster is a factor (categorical raster)
  if (!is.factor(raster_name[names(raster_name)])) {
    stop("The input raster is not a categorical raster.")
  }
  
  # Get the names and number of levels (categories) in the raster
  level_names <- levels(raster_name[[names(raster_name)]])[[1]][,2]
  levels_count <- length(level_names)
  
  # create starting empty list
  templist <- list()
  # loop through categories and create binary raster where "1"s are where the
  # focal category exists
  for (i in 1:levels_count) {
    templist[[i]] <- rast(ncol=ncol(raster_name), 
                          nrow=nrow(raster_name), ext=ext(raster_name), names = level_names[i],
                          crs=crs(raster_name), vals = values(as.numeric(raster_name == level_names[i])))
  }
  # now create multilayer raster from those individual rasters
  multilayer_raster <- rast(templist)
  # Return the multilayer raster object
  return(multilayer_raster)
}



# Landcover:

lndcvr_na # wrong CRS, resolution
# this is a huge file, so we need to aggregate the cells before 
# it can be reprojected and cropped
# selected a factor of 15 - reduce # of cells by 15 times

lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
lndcvr_na_agg <-writeRaster(lndcvr_na_agg, 
                            filename = "data/processed/lndcvr-north-america_agg.tif", 
                            overwrite = TRUE)

# read in aggregated landcover data from new file
lndcvr_na_agg <- rast("data/processed/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# resample landcover data to change resolution
landcover <- resample(lndcvr_na_agg, na_bound_rast)

# crop landcover North America data to study extent
landcover <- crop(landcover, na_bound_vect)

# label as.factor
landcover <- as.factor(landcover)
is.factor(landcover)

# change layer name to match object name
names(landcover) <- "landcover"

# create file of processed landcover data for re-use
#writeRaster(landcover, filename = "data/processed/landcover.tif", overwrite = TRUE)


# use function to create binary layers:
landcover_binary <- create_multilayer_raster(landcover)
landcover_binary



# Anthropogenic Biomes

anth_biome_na
# wrong CRS and resolution

# reproject anthropogenic biomes data to WGS84
anth_biomeWGS <- terra::project(anth_biome_na, "EPSG:4326", method = "near")

# resample anth_biome to change resolution
anth_biome <- resample(anth_biomeWGS, na_bound_rast)

# crop anth_biome to study extent
anth_biome <- crop(anth_biome, na_bound_vect)

# label as.factor:
anth_biome <-as.factor(anth_biome)
is.factor(anth_biome)

# change layer name to match object name
names(anth_biome) <- "anth_biome"

# write anth_biome to file for reuse
# writeRaster(anth_biome, filename = "data/processed/anth_biome.tif", overwrite = TRUE)


create_multilayer_raster <- function(raster_name) {
  # "raster_name" is the name of the categorical raster object
  require(terra)
  # Check if the raster is a factor (categorical raster)
  if (!is.factor(raster_name[names(raster_name)])) {
    stop("The input raster is not a categorical raster.")
  }
  
  # Get the names and number of levels (categories) in the raster
  level_names <- levels(raster_name[[names(raster_name)]])[[1]][,2]
  levels_count <- length(level_names)
  
  # create starting empty list
  templist <- list()
  # loop through categories and create binary raster where "1"s are where the
  # focal category exists
  for (i in 1:levels_count) {
    templist[[i]] <- rast(ncol=ncol(raster_name), 
                          nrow=nrow(raster_name), ext=ext(raster_name), names = level_names[i],
                          crs=crs(raster_name), vals = values(as.numeric(raster_name == level_names[i])))
  }
  # now create multilayer raster from those individual rasters
  multilayer_raster <- rast(templist)
  # Return the multilayer raster object
  return(multilayer_raster)
}



# use function to create binary layers:
anth_biome_binary <- create_multilayer_raster(anth_biome)
anth_biome_binary


# Categorical Data from Shapefiles:
# need to create a temporary raster so the Shapefiles can take on its structure
# tried using na_bound_rast which worked, except for changing categorical values to numeric


# Climate Zones

climate_zones_na 
# SpatVector, equal area projection

# reproject to WGS84
climate_zones <- terra::project(climate_zones_na, "EPSG:4326")

# create raster from SpatVector and structure of na_bound_rast
climate_zones <- rasterize(climate_zones, na_bound_rast, field = "Climate")
climate_zones

# label as.factor
climate_zones <- as.factor(climate_zones)
is.factor(climate_zones)

# change the name to match the object
names(climate_zones) <- "climate_zones"
climate_zones

# use function to create binary layers:
climate_zones_binary <- create_multilayer_raster(climate_zones)
climate_zones_binary

# write to file for reuse
# writeRaster(climate_zones, filename = "data/processed/climate_zones.tif", overwrite = TRUE)


# Ecoregions
# repeat steps above for ecoregions
# select "NameL3_En" column, names in English
# Climate Zones

ecoregions_na 
# SpatVector, equal area projection

# reproject to WGS84
ecoregions <- terra::project(ecoregions_na, "EPSG:4326")
ecoregions

# create raster from SpatVector and structure of na_bound_rast
ecoregions <- rasterize(ecoregions, na_bound_rast, field = "NameL3_En")
ecoregions

# label as.factor
ecoregions <- as.factor(ecoregions)
is.factor(ecoregions)

# change the name to match the object
names(ecoregions) <- "ecoregions"
ecoregions

# use function to create binary layers:
ecoregions_binary <- create_multilayer_raster(ecoregions)
ecoregions_binary


# Watersheds
# repeat above steps for watersheds raster
# Climate Zones

watersheds_na 
# SpatVector, equal area projection

# reproject to WGS84
watersheds <- terra::project(watersheds_na, "EPSG:4326")
watersheds

# create raster from SpatVector and structure of na_bound_rast
watersheds <- rasterize(watersheds, na_bound_rast, field = "NAW4_EN")
watersheds

# label as.factor
watersheds <- as.factor(watersheds)
is.factor(watersheds)

# change the name to match the object
names(watersheds) <- "watersheds"
watersheds

# use function to create binary layers:
watersheds_binary <- create_multilayer_raster(watersheds)
watersheds_binary



# now create a multilayer SpatRaster with all the above rasters

predictors_multi_factors <- c(#anth_biome_binary, 
                      climate_zones_binary, 
                      ecoregions_binary, 
                      elevation, 
                      #landcover_binary,
                      soil_phh2o_0_5, 
                      soil_phh2o_5_15, 
                      soil_temp_0_5, 
                      soil_temp_5_15) 
                      #watersheds_binary)


# mask the multilayer raster so the values outside of na_bound are NA
predictors_multi_factors <- mask(predictors_multi_factors, na_bound_vect)

# write the multilayer raster to file for reuse
writeRaster(predictors_multi_factors, 
            filename = "data/processed/predictors_multi_factors.tif", 
            overwrite = TRUE)