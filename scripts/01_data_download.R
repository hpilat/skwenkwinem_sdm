# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024
# Updated by Jason Pither May 21, 2025

# This script downloads the occurrence and predictor data required to run tidysdm

# increase file download timeout options for downloading large files
options(timeout = max(6000, getOption("timeout")))

# assumed directory structure:

# root_project/
# ├── data/
# │   ├── raw
# │   ├── extents
# │   └── processed
# ├── scripts
# ├── outputs
# └── _README.md

# Uncomment and run the following if necessary to make the above directory structure:
#data_dir <- here("data")
#data_raw <- here("data", "raw")
#data_extents <- here("data", "extents")
#data_processed <- here("data", "processed")
#data_outputs <- here("data", "outputs")

#dir.create(data_dir, recursive = TRUE)
#dir.create(data_raw, recursive = TRUE)
#dir.create(data_extents, recursive = TRUE)
#dir.create(data_processed, recursive = TRUE)
#dir.create(data_outputs, recursive = TRUE)

# Get session info

sessionInfo()
# R version 4.3.2 (2023-10-31)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 15.7.1

# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

# locale:
 # [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# time zone: America/Regina
# tzcode source: internal

# attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#loaded via a namespace (and not attached):
#  [1] compiler_4.3.2    tools_4.3.2       rstudioapi_0.17.1
# required packages

# we use the 'here' package to help with paths
library(here)

library(tidyverse)
library(geodata) 
library(terra)
library(sf)
library(rgbif)
library(usethis)
library(here)

## Occurrence Data ##

# set up GBIF credentials
install.packages("usethis")
usethis::edit_r_environ()
# you'll need to input the following, with "_" replaced by your credentials:
# GBIF_USER="_"
# GBIF_PWD="_"
# GBIF_EMAIL="_"

# download occurrence data for skwenkwinem (Claytonia lanceolata)
# within North America, between 1940-2024
## **NOTE** this was the code for the query; the key for this is shown below
# uncomment if you wish to run new query, but then you'll need a new key

# rgbif::occ_download(
#   pred("hasGeospatialIssue", FALSE), 
#   pred("hasCoordinate", TRUE),
#   pred("continent", "north_america"),
#   pred("year", "1940,2024"),
#   pred("taxonKey", 3084746), 
#   format = "SIMPLE_CSV")


# to check status of download (using original query key):
occ_download_wait('0030926-240229165702484')
# 3541 occurrences

# to access download when it's finished
skwenkwinem_download <- occ_download_get('0030926-240229165702484',
                        path = here::here("data", "raw"),
                        overwrite = TRUE) 
skwenkwinem_download_csv <-  occ_download_import(skwenkwinem_download)

# write to file for reuse
write.csv(skwenkwinem_download_csv, file = here::here("data", "raw", "skwenkwinem_download.csv"))

###################
## Predictor Data ##
###################

# Present Climate Data (1971-2000):

# https://www.worldclim.org/data/worldclim21.html 

# **NOTE** uses 10.96GB storage
# USE 30s bio data
download.file('https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_bio.zip', destfile = here::here("data", "raw", "wc2.1_30s_bio.zip"))
unzip(here::here("data", "raw", "wc2.1_30s_bio.zip"), 
      exdir = here::here("data", "raw", "worldclim_bio"),
      overwrite = TRUE)

# Future Climate Data (climate scenario SSP 5-8.5, 2081-2100)
# **NOTE** uses 9.2GB storage

# https://www.worldclim.org/data/cmip6/cmip6_clim30s.html#google_vignette

download.file("https://geodata.ucdavis.edu/cmip6/30s/HadGEM3-GC31-LL/ssp585/wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp585_2081-2100.tif", destfile = here::here("data", "raw", "wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp585_2081-2100.tif"))

# Informed Predictor Data:
# note: we had intended to include more predictors, but were constrained by 
# computational limits. Less important predictors have been annotated but still 
# included in case higher powered computing is possible for an informed model 
# with more predictors. 

# Anthropogenic biome data

download.file("https://www.cec.org/files/atlas_layers/4_human_influence/4_02_anthropogenic_biomes_2008/anthropogenicbiomes_2008_tif.zip", destfile = here::here("data", "raw", "anthromes_EqArea_tif.zip"))
unzip(here::here("data", "raw", "anthromes_EqArea_tif.zip"), 
      exdir = here::here("data", "raw"),
      overwrite = TRUE)

# North American Climate Zones data

#download.file("https://www.cec.org/files/atlas_layers/5_climate/5_01_climate_zones/na_climatezones_shapefile.zip", destfile = here::here("data", "raw", "na_climatezones_shapefile.zip"))
#unzip(here::here("data", "raw", "na_climatezones_shapefile.zip"), overwrite = TRUE)
#climate_zones_sf <- read_sf("data/raw/North_America_Climate_Zones.shp")
#climate_zones_vect <- vect(climate_zones_sf)

# North American Ecoregions (level III) data

#download.file("https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_06_3_terrestrial_ecoregions_level_iii/na_terrestrial_ecoregions_v2_level_iii_shapefile.zip", destfile = here::here("data", "raw", "NA_Terrestrial_Ecoregions_v2_level3.zip"))
#unzip(here::here("data", "raw", "NA_Terrestrial_Ecoregions_v2_level3.zip"), overwrite = TRUE)
#climate_zones_sf <- read_sf("data/raw/NA_Terrestrial_Ecoregions_v2_level3.shp")
#climate_zones_vect <- vect(NA_Terrestrial_Ecoregions_v2_level3.shp)

# Elevation data for North America
download.file("https://www.cec.org/files/atlas_layers/0_reference/0_03_elevation/elevation_tif.zip", 
              destfile = here::here("data", "raw", "northamerica_elevation_cec_2023_tif.zip"))
unzip(here::here("data", "raw", "northamerica_elevation_cec_2023_tif.zip"), 
      exdir = here::here("data", "raw"),
      overwrite = TRUE)

# Landcover data for North America
download.file("https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/land_cover_2020v2_30m_tif.zip", 
              destfile = here::here("data", "raw", "NA_NALCMS_landcover_2020_30m_tif.zip"))
unzip(here::here("data", "raw", "NA_NALCMS_landcover_2020_30m_tif.zip"), 
      exdir = here::here("data", "raw"),
      overwrite = TRUE)

# Soil pH data: **NOTE don't download from SoilGrids website... use ucdavis
download.file("https://geodata.ucdavis.edu/geodata/soil/soilgrids/phh2o_0-5cm_mean_30s.tif", 
              destfile = here::here("data", "raw", "phh2o_0-5cm_mean_30s.tif"))

#download.file("https://files.isric.org/soilgrids/latest/data_aggregated/1000m/phh2o/phh2o_5-15cm_mean_1000.tif", destfile = here::here("data", "raw", "phh2o_5-15cm_mean_30s.tif"))

# Soil temperature data:
download.file("https://zenodo.org/records/7134169/files/SBIO4_0_5cm_Temperature_Seasonality.tif", destfile = here::here("data", "raw", "SBIO4_0_5cm_Temperature_Seasonality.tif"))

#download.file("https://zenodo.org/records/7134169/files/SBIO4_5_15cm_Temperature_Seasonality.tif", destfile = here::here("data", "raw", "SBIO4_5_15cm_Temperature_Seasonality.tif"))

# Watersheds data (for generating study extent layers)
# arctic watersheds
download.file("https://data.hydrosheds.org/file/hydrobasins/standard/hybas_ar_lev01-12_v1c.zip", destfile = here::here("data", "raw", "watershed_arctic_v2.zip"))
unzip(here::here("data", "raw", "watershed_arctic_v2.zip"), 
      exdir = here::here("data", "raw", "arctic_hydro"),
      overwrite = TRUE)

# North America watersheds
download.file("https://data.hydrosheds.org/file/hydrobasins/standard/hybas_na_lev01-12_v1c.zip", destfile = here::here("data", "raw", "watershed_northamer_v2.zip"))
unzip(here::here("data", "raw", "watershed_northamer_v2.zip"), 
      exdir = here::here("data", "raw", "north_america_hydro"),
      overwrite = TRUE)

## DONE