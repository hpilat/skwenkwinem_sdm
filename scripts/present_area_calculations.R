# Skeetchestn Territory:
skeetch_sf <- st_read("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
skeetch_sf

# convert to SpatVector for masking our rasters:
skeetch_vect <- vect(skeetch_sf)
skeetch_vect
# create extent object slightly larger than vector for cropping our rasters:
skeetch_extent <- ext(1314424, 1398550, 609867, 732906) # xmin, xmax, ymin, ymax


# calculate area:
skeetch_area <- st_area(skeetch_sf) # 7e+09
# convert from m^2 to km^2
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) 
# 6996 km^2 of suitable habitat

# read in binary prediction raster:
model_agreement_pres <- rast("outputs/model_agreement.tif")
model_agreement_fut <- rast("outputs/model_agreement_future.tif")

# project to Albers equal area:
model_agreement_pres_albers <- project(model_agreement_pres, "EPSG:3005")
model_agreement_pres_albers
model_agreement_fut_albers <- project(model_agreement_fut, "EPSG:3005")
model_agreement_fut_albers

# crop and mask to Skeetchestn Territory:
model_agreement_pres_skeetch <- crop(model_agreement_pres_albers, skeetch_extent)
model_agreement_pres_skeetch <- mask(model_agreement_pres_skeetch, skeetch_vect)

# Start with model_agreement_pres
model_agreement_pres_albers
# dimensions are:
4123 * 4461
# 18392703 cells

cell_counts <- freq(model_agreement_pres_albers)
cell_counts
# from 08 script:
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

# how many cells in count column
sum(cell_counts$count)
# 6938093 # !half the total dimensions
# how many cells are NA?
global(model_agreement_pres_albers, fun="isNA")
#binary_mean 11454610 
# again, about half, 

# add cells with values to calls that are NAs to check counts.
sum(cell_counts$count) + global(model_agreement_pres_albers, fun="isNA") 
# 18392703
# good, this matches the total cells from the dimensions

# Proportion of total area in each category
cell_counts$prop <- cell_counts$count/sum(cell_counts$count)
cell_counts$prop
# 0.67982067 0.06593267 0.07092410 0.18332256
# predicted total is sum of present, future, and both
sum(cell_counts$count[2:4])
# 2221434

# Informed model: value = 1 but it's the 2nd row in our cell_counts dataframe
# present predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[2]/sum(cell_counts$count[2:4]))*100
# 20.59242

# present predicted suitable habitat as a percent of total study area:
(cell_counts$count[2]/sum(cell_counts$count[1:4]))*100
# 6.593267

# Bioclim model: value = 2 but its's the 3rd row in our cell_counts dataframe
# present predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[3]/sum(cell_counts$count[2:4]))*100
# 22.15137

# present predicted suitable habitat, as a percent of total study area:
(cell_counts$count[3]/sum(cell_counts$count[1:4]))*100
# 7.09241

# Agreement between Informed and Bioclim present models:
# area of agreement as a percent of total predicted suitable habitat:
(cell_counts$count[4]/sum(cell_counts$count[2:4]))*100
# 57.25621

# present predicted suitable habitat, as a percent of total study area:
(cell_counts$count[4]/sum(cell_counts$count[1:4]))*100
# 18.33226

# now get range of cell sizes over our Study extent:
rast_agg <- aggregate(rast(model_agreement_pres_albers), 100)
rast_agg
cell_size <- cellSize(rast_agg, unit="km") / 10000
resampled_rast <- resample(cell_size, model_agreement_pres_albers)
minmax(cell_size)
# cell size ranges from 0.5612499 m^2 to 0.5612790 m^2


area_zones <- zonal(resampled_rast, model_agreement_pres_albers, sum, na.rm=TRUE)
area_zones
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence
