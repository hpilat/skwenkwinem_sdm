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
model_agreement_fut <- rast("outputs/model_agreement_future.tif")

# crop and mask to Skeetchestn Territory:
model_agreement_fut_skeetch <- crop(model_agreement_fut_albers, skeetch_extent)
model_agreement_fut_skeetch <- mask(model_agreement_fut_skeetch, skeetch_vect)

# get dimensions of raster:
model_agreement_fut
# so total dimensions / area == 
3143 * 4084
# 12836012 cells
cell_counts <- freq(model_agreement_fut)
cell_counts
#how many cells in count column
sum(cell_counts$count)
#[1] 6656291 # !half the total dimensions
# how many cells are NA?
global(model_agreement_fut, fun="isNA")
#binary_mean 6179721 # again, about half, 
# add cells with values to calls that are NAs to check counts.
sum(cell_counts$count) + global(model_agreement_fut, fun="isNA") # good this matches the total cells from the dimensions.
#                 isNA
# binary_mean 12836012
# Proportion of total area in each category
cell_counts$prop <- cell_counts$count/sum(cell_counts$count)
cell_counts$prop
# predicted total is sum of present, future, and both
sum(cell_counts$count[2:4])
# 2037928
# present predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[2]/sum(cell_counts$count[2:4]))*100
#[1] 26.77548
# future predicted suitable habitat, as a percent of total predicted suitable habitat:
(cell_counts$count[3]/sum(cell_counts$count[2:4]))*100
# [1] 13.81997
# area of agreement between present and future predicted suitable habitat
# as a percent of total predicted suitable habitat
(cell_counts$count[4]/sum(cell_counts$count[2:4]))*100
# [1] 59.40455

# now get range of cell sizes over our Study extent:
rast_agg <- aggregate(rast(model_agreement_fut), 100)
rast_agg
cell_size <- cellSize(rast_agg, unit="km") / 10000
resampled_rast <- resample(cell_size, model_agreement_fut)
minmax(cell_size)
# cell size ranges from 0.4613605 m^2 to 0.7288025 m^2


area_zones <- zonal(resampled_rast, model_agreement_fut, sum, na.rm=TRUE)
area_zones