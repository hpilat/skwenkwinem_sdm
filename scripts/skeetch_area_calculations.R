# This is script 10/10 (?)
# This script calculates are predicted to be suitable by our models, within 
  # Skeetchestn Territory

# Read in binary prediction rasters:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")

# Skeetchestn territory boundary vector for masking:
skeetch_vect <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# transform to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect, "EPSG:4326")

# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extent <- ext(-121.6, -120.1, 50.3, 51.6) # xmin, xmax, ymin, ymax


# crop and mask prediction rasters to Skeetchestn boundary:
informed_present_skeetch <- crop(informed_present_binary, skeetch_vectWGS84)
informed_present_skeetch <- mask(informed_present_skeetch, skeetch_vectWGS84)

bioclim_present_skeetch <- crop(bioclim30s_present_binary, skeetch_vectWGS84)
bioclim_present_skeetch <- mask(bioclim_present_skeetch, skeetch_vectWGS84)

bioclim_future_skeetch <- crop(bioclim30s_future_binary, skeetch_vectWGS84)
bioclim_future_skeetch <- mask(bioclim_future_skeetch, skeetch_vectWGS84)

# Informed Prediction:
informed_present_skeetch

# reproject raster to Albers equal area projection:
informed_skeetch_albers <- project(informed_present_skeetch, "EPSG:4326")
plot(informed_skeetch_albers)

# filter out cells classified as "presence"
informed_skeetch_filt <- informed_skeetch_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(informed_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
informed_skeetch_polygons <- as.polygons(informed_skeetch_filt)

# convert to sf object so we can calculate area:
informed_skeetch_sf <- st_as_sf(informed_skeetch_polygons)
informed_skeetch_sf

# project to Albers equal area:
informed_skeetch_sf <- st_transform(informed_skeetch_sf, "EPSG:3005")
informed_skeetch_sf

# calculate area:
informed_skeetch_area <- st_area(informed_skeetch_sf) # 4.14e+09
# convert from m^2 to km^2
informed_skeetch_area <- units::set_units(st_area(informed_skeetch_sf), km^2) 
# 4142 km^2 of suitable habitat


# Bioclim30s present prediction:
bioclim_present_skeetch

# reproject raster to Albers equal area projection:
bioclim_skeetch_albers <- project(bioclim_present_skeetch, "EPSG:4326")
plot(bioclim_skeetch_albers)

# filter out cells classified as "presence"
bioclim_skeetch_filt <- bioclim_skeetch_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_skeetch_polygons <- as.polygons(bioclim_skeetch_filt)

# convert to sf object so we can calculate area:
bioclim_skeetch_sf <- st_as_sf(bioclim_skeetch_polygons)
bioclim_skeetch_sf

# project to Albers equal area:
bioclim_skeetch_sf <- st_transform(bioclim_skeetch_sf, "EPSG:3005")
bioclim_skeetch_sf

# calculate area:
bioclim_skeetch_area <- st_area(bioclim_skeetch_sf) # 3.18e+09
# convert from m^2 to km^2
bioclim_skeetch_area <- units::set_units(st_area(bioclim_skeetch_sf), km^2) 
# 3181 km^2 of suitable habitat

# get the intersection of the informed and bioclim predicted suitable habitat:
agreement_present_skeetch <- terra::intersect(informed_skeetch_filt, bioclim_skeetch_filt)
agreement_present_skeetch
plot(agreement_present_skeetch)

# reproject to Albers equal area projection:
agreement_present_skeetch_albers <- project(agreement_present_skeetch, "EPSG:4326")

# filter out cells classified as "TRUE" (where the models intersect)
agreement_present_skeetch_filt <- agreement_present_skeetch_albers %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_present_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_present_skeetch_polygons <- as.polygons(agreement_present_skeetch_filt)

# convert to sf object so we can calculate area:
agreement_present_skeetch_sf <- st_as_sf(agreement_present_skeetch_polygons)
agreement_present_skeetch_sf

# project to Albers equal area:
agreement_present_skeetch_sf <- st_transform(agreement_present_skeetch_sf, "EPSG:3005")
agreement_present_skeetch_sf

# calculate area:
agreement_present_skeetch_area <- st_area(agreement_present_skeetch_sf) # 2.76e+09
# convert from m^2 to km^2
agreement_present_skeetch_area <- units::set_units(st_area(agreement_present_skeetch_sf), km^2) 
# 2756 km^2 of suitable habitat


# Bioclim30s Future Prediction:

bioclim_future_skeetch

# reproject raster to Albers equal area projection:
bioclim_future_skeetch_albers <- project(bioclim_future_skeetch, "EPSG:4326")
plot(bioclim_future_skeetch_albers)

# filter out cells classified as "presence"
bioclim_future_skeetch_filt <- bioclim_future_skeetch_albers %>% 
  dplyr::filter(binary_mean == "presence")
plot(bioclim_future_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
bioclim_future_skeetch_polygons <- as.polygons(bioclim_future_skeetch_filt)

# convert to sf object so we can calculate area:
bioclim_future_skeetch_sf <- st_as_sf(bioclim_future_skeetch_polygons)
bioclim_future_skeetch_sf

# project to Albers equal area:
bioclim_future_skeetch_sf <- st_transform(bioclim_future_skeetch_sf, "EPSG:3005") 

# calculate area:
bioclim_future_skeetch_area <- st_area(bioclim_future_skeetch_sf) # 1.43e+09
# convert from m^2 to km^2
bioclim_future_skeetch_area <- units::set_units(st_area(bioclim_future_skeetch_sf), km^2) 
# 1434 km^2 of suitable habitat


# calculate difference in suitable area from present to future:
difference_skeetch_area <- bioclim_future_skeetch_area - bioclim_skeetch_area
# -1746 km^2

# calculate proportion of suitable habitat changed from present to future:
proportion_diff_skeetch_area <- difference_skeetch_area / bioclim_skeetch_area
# -0.549


# get the intersection of the informed and bioclim predicted suitable habitat:
agreement_future_skeetch <- terra::intersect(bioclim_skeetch_filt, bioclim_future_skeetch_filt)
agreement_future_skeetch
plot(agreement_future_skeetch)

# reproject to Albers equal area projection:
agreement_future_skeetch_albers <- project(agreement_future_skeetch, "EPSG:4326")

# filter out cells classified as "TRUE" (where the models intersect)
agreement_future_skeetch_filt <- agreement_future_skeetch_albers %>% 
  dplyr::filter(binary_mean == TRUE)
plot(agreement_future_skeetch_filt)

# convert raster cells to polygons so we can convert to an sf object:
agreement_future_skeetch_polygons <- as.polygons(agreement_future_skeetch_filt)

# convert to sf object so we can calculate area:
agreement_future_skeetch_sf <- st_as_sf(agreement_future_skeetch_polygons)
agreement_future_skeetch_sf

# project to Albers equal area:
agreement_future_skeetch_sf <- st_transform(agreement_future_skeetch_sf, "EPSG:3005")
agreement_future_skeetch_sf

# calculate area:
agreement_future_skeetch_area <- st_area(agreement_future_skeetch_sf) # 1.43e+09
# convert from m^2 to km^2
agreement_future_skeetch_area <- units::set_units(st_area(agreement_future_skeetch_sf), km^2) 
# 1433 km^2 of suitable habitat


# Skeetchestn Area:

# convert to sf object:
skeetch_sf <- st_as_sf(skeetch_vect)
skeetch_sf # NAD83/BC Albers

# calculate area:
skeetch_area <- st_area(skeetch_sf) # 7e+09
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) 
# 6996 km^2 of suitable habitat

# proportion of total area predicted suitable by informed model:
proportion_informed_skeetch <- informed_skeetch_area / skeetch_area
# 0.592

# proportion of total area predicted suitable by bioclim present model:
proportion_bioclim_present_skeetch <- bioclim_skeetch_area / skeetch_area
# 0.455

# proportion of total area predicted suitable by bioclim future model:
proportion_bioclim_future_skeetch <- bioclim_future_skeetch_area / skeetch_area
# 0.205

# proportion of total area predicted suitable by both 
# informed and bioclim present models (agreement area)
proportion_agree_present_skeetch <- agreement_present_skeetch_area / skeetch_area
# 0.394

# proportion of total area predicted suitable by both bioclim 
# present and future models (agreement area)
proportion_agree_future_skeetch <- agreement_future_skeetch_area / skeetch_area
# 0.205

# calculate percent overlap of total suitable area for 
  # informed and bioclim present models
overlap_informed_bioclim_skeetch <- agreement_present_skeetch_area / (informed_skeetch_area + bioclim_skeetch_area - agreement_present_skeetch_area)
# 0.376 without subtracting agreement area in denominator
# 0.603 when subtracting agreement area in denominator

# calculate percent overlap of total suitable area for 
  # bioclim present and future models
overlap_bioclim_pres_fut_skeetch <- agreement_future_skeetch_area / (bioclim_skeetch_area + bioclim_future_skeetch_area - agreement_future_skeetch_area)
# 0.311 without subtracting agreement area in denominator
# 0.451 when subtracting agreement area in denominator