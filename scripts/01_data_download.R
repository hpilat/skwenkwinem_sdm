# This script is 01/08 
# Here, we download the occurrence and predictor data required to run tidysdm

# dir.create("data/")
# dir.create("data/raw")
# dir.create("data/extents")
# dir.create("data/processed")
# dir.create("scripts/")
# dir.create("outputs/")


library(tidyverse)
library(geodata) # won't work when on AAFC VPN
library(terra)
# library(raster)
library(sf)
library(rgbif)
# library(CoordinateCleaner) unsure if I'll be using this
# note: cannot load rgdal and terra at the same time 
# if using project function from terra (call terra::project)


## Occurrence Data ##


# set up GBIF credentials
# install.packages("usethis")
# usethis::edit_r_environ()

# download occurrence data for skwenkwinem (Claytonia lanceolata)
# within North America, between 1950-2024
rgbif::occ_download(
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE),
  pred("continent", "north_america"),
  pred("year", "1940,2024"),
  pred("taxonKey", 3084746), 
  format = "SIMPLE_CSV")

# to check status of download:
occ_download_wait('0030926-240229165702484')

# to access download when it's finished
skwenkwinem_download <- occ_download_get('0030926-240229165702484') %>%
  occ_download_import(key = '0030926-240229165702484', 
                      path = "data/raw")

# write to file for reuse
write.csv(skwenkwinem_download, file = "data/raw/skwenkwinem_download.csv")


## Predictor Data ##


# Present Climate Data:

# highest resolution from geodata is 0.5 minutes of a degree 
# tidysdm tutorial uses pastclim to access WorldClim data, but only low resolution
# datasets are available
# https://www.worldclim.org/data/worldclim21.html 
# 9.7 GB file, moved to data processing script in order to crop layers prior to 
# joining in a multilayer raster


# Future Climate Data
# note: 8.5 GB file
# https://www.worldclim.org/data/cmip6/cmip6_clim30s.html#google_vignette
worldclim_future_na <- rast("data/raw/wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp126_2081-2100.tif")



# Informed Predictor Data:


# read in anthropogenic biome data
anth_biome <- rast("data/raw/anthromes_EqArea.tif")

# read in North American Climate Zones data
climate_zones_sf <- read_sf("data/raw/North_America_Climate_Zones.shp")
climate_zones_vect <- vect(climate_zones_sf)

# read in North American Ecoregions (level III) data
ecoregions_sf <- read_sf("data/raw/na_terrestrial_ecoregions_v2_level_iii_shapefile/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_TerrestrialEcoregions_LIII/data/NA_Terrestrial_Ecoregions_v2_level3.shp")
ecoregions_vect <- vect(ecoregions_sf)

# elevation data for North America
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")

# read in landcover data for North America
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")

# soil pH data:
soil_phh2o_0_5 <- rast("data/raw/soil_world/phh2o_0-5cm_mean_30s.tif")
soil_phh2o_5_15 <- rast("data/raw/soil_world/phh2o_5-15cm_mean_30s.tif")

# soil temperature data:
soil_temp_0_5 <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
soil_temp_5_15 <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")

# read in watersheds data
watersheds_sf <- read_sf("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")
watersheds_vect <- vect(watersheds_sf)

