# This is script 03/07
# This script prepares our spatial extent for input into the tidysdm pipeline
# Please first run these scripts in the following order:
# 01_data_download_ranunculus.R
# 02_continental_divide.Rmd


library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)

### Spatial Extents ###

# original extent created in 02_continental_divide.R
# original extent = too large of files for input, huge RAM requirements
# have to reduce our spatial extent from west coast of North America
# new spatial extent covers occurrence records + buffer
# required in order for projection code to run for tidysdm_ranunculus_multirast.R



### North American extent (original, west coast to continental divide) ###



# Geographic extent created in continental_divide.Rmd
# read in shapefile so we can reduce the extent
na_bound_vect <- vect("data/extents/continental_divide_buffer_boundary.shp")

# read in same file as an sf object so we can calculate our study area
na_bound_sf <- read_sf("data/extents/continental_divide_buffer_boundary.shp")

# created an empty raster based on study extent to use as a basemap for TidySDM, 
  # to be cropped down below
temprast <- rast(na_bound_vect, ncols = 12247, nrows = 8024)
na_bound_rast <- rasterize(na_bound_vect, temprast)

# create new bounds for reduced extent, based on our realized niche area
# first need to bring in occurrence data
skwenkwinem_download <- read.csv(file = "data/raw/skwenkwinem_download.csv")

# select only the relevant columns: ID column, longitude, latitude
skwenkwinem_occ <- dplyr::select(skwenkwinem_download, gbifID, 
                         decimalLongitude, decimalLatitude) # dataframe

# create a SpatVector object for the occurrence data
skwenkwinem_vect <- vect(skwenkwinem_occ, geom = c("decimalLongitude", "decimalLatitude"), 
                     crs = "EPSG:4326", keepgeom = FALSE)

# cast coordinates into an sf object and set its CRS to WGS84
skwenkwinem_sf <- st_as_sf(skwenkwinem_occ, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(skwenkwinem_sf) <- 4326


ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = skwenkwinem_sf)

# looks like we have an outlier (up in Alaska) - let's crop it out

# extend limits with 2 degree buffer in all directions, but shrink extent at ymax
xlims <- c(ext(skwenkwinem_sf)$xmin - 2, ext(skwenkwinem_sf)$xmax + 2)
ylims <- c(ext(skwenkwinem_sf)$ymin - 2, ext(skwenkwinem_sf)$ymax -3)

# now crop all layers:
extent.test <- terra::ext(xlims, ylims)
na_bound_rast <- crop(na_bound_rast, extent.test)
na_bound_vect <- crop(na_bound_vect, extent.test)
na_bound_sf <- st_crop(na_bound_sf, extent.test)
skwenkwinem_sf <- st_crop(skwenkwinem_sf, extent.test)

# plot occurrences on background raster again to check if extent looks correct:
ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = skwenkwinem_sf)
# overall extent looks good

# write na_bound_rast to file for reuse
writeRaster(na_bound_rast, filename = "data/extents/na_bound_rast.tif", overwrite = TRUE)

# write na_bound_vect to file for use in tidysdm as a mask
writeVector(na_bound_vect, filename = "data/extents/na_bound_vect.shp", overwrite = TRUE)

# write sf object to file for calculating area in tidysdm
st_write(na_bound_sf, dsn = "data/extents/na_bound_sf.shp", append = FALSE)

# write ran_occ_sf to file
st_write(skwenkwinem_sf, dsn = "data/processed/skwenkwinem_sf.shp", append = FALSE)



### Skeetchestn territory: ###



# SNRC provided shapefile of Skeetchestn traditional territory
# Read in Skeetchestn territory shapefile
skeetch_sf <- st_read("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)

