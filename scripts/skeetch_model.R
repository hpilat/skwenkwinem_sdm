# May 16, 2024
# Hannah Pilat

library(tidyverse)
library(geodata) # won't work when on AAFC VPN
library(terra)
library(tidyterra)
library(sf)
library(geodata)
library(tidysdm)
library(overlapping)
library(xgboost)
# note: cannot load rgdal and terra at the same time 
# if using project function from terra (call terra::project)



# Occurrence Data



# read in occurrence data collected within Skeetchestn Territory:
skeetch_occ <- read.csv("data/raw/skwenkwinem-occurrence_data_2022.csv", header = TRUE)
skeetch_occ

# select only relevant columns:
skeetch_occ <- skeetch_occ %>% 
  dplyr::select("latitude", "longitude", "occurrence")
skeetch_occ

# need to add negative to longitude values:
skeetch_occ$longitude <- skeetch_occ$longitude * -1
skeetch_occ

# select only presence points:
skeetch_presence <- skeetch_occ %>% 
  dplyr::filter(skeetch_occ$occurrence == 1) %>% 
  dplyr::select("latitude", "longitude")
skeetch_presence

# select only absence points:
skeetch_absence <- skeetch_occ %>% 
  dplyr::filter(skeetch_occ$occurrence == 0) %>% 
  dplyr::select("latitude", "longitude")

# convert both to SpatVectors:
skeetch_presence <- vect(skeetch_presence, geom = c("longitude", "latitude"), 
                         crs = "EPSG:4326", keepgeom = FALSE)
skeetch_presence

skeetch_absence <- vect(skeetch_absence, geom = c("longitude", "latitude"), 
                        crs = "EPSG:4326", keepgeom = FALSE)
skeetch_absence



## Extent##

# read in the boundaries for British Columbia (BC)
bc_bound_sf <- bcmaps::bc_bound(ask = interactive(), force = FALSE)

# create a SpatVector for the BC boundary
bc_bound <- vect(bc_bound_sf)
bc_bound #NAD83

# read in Skeetchestn Territory boundary
skeetch_vect <- vect("C:/Users/PilatH/OneDrive - AGR-AGR/Desktop/skwenkwinem_sdm/data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
skeetch_vect # NAD83

# create WGS84 Skeetchestn Boundary:
skeetch_WGS <- terra::project(skeetch_vect, "EPSG:4326")
plot(skeetch_WGS)

# create an extent object slightly larger than Skeetch polygon
extent <- c(-121.8, -120, 50.2, 51.7)
skeetch_extent <- terra::ext(extent)

# create bounding box (sf object) for downloading elevation tiles
skeetch_bbox <- st_bbox(c(xmin = -121.8, xmax = -120, ymin = 50.2, ymax = 51.7), 
                        crs = st_crs(4326))

# create a lines object with the Skeetchestn vector
skeetch_lines <- as.lines(skeetch_WGS)

plot(skeetch_extent)
lines(skeetch_lines)

# create a plot with Skeetchestn Territory in the context of British Columbia:
skeetch_context <- ggplot() +
  geom_spatvector(data = bc_bound, aes(fill = NULL), fill = "white", show.legend = FALSE) +
  geom_spatvector(data = skeetch_vect, fill = "skyblue1", show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent")) +
  scale_x_continuous(name = "Longitude (°W)",
                     expand = c(0,0)) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_y_continuous(name = "Latitude (°N)",
                     expand = c(0, 0)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank()) 
skeetch_context
ggsave(filename = "outputs/skeetch_context.png", skeetch_context, bg = "transparent")

# our raster structure will be based on:
# landcover
landcover_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")
landcover_na # WGS1984 Lambert Azimuthal Equal area

eq_area_crs <- crs(landcover_na)

# create a rough bounding box around BC so we can crop landcover:
bc_eq_area <- terra::project(bc_bound, eq_area_crs)
bc_eq_area 

# crop landcover to Skeetchestn boundary:
landcover <- crop(landcover_na, bc_eq_area)
landcover

# project to WGS84:
landcover <- terra::project(landcover, "EPSG:4326", method = "near")
landcover

# crop to Skeetchestn Territory:
landcover <- crop(landcover, skeetch_extent)
landcover
landcover_cat <- as.factor(landcover)
levels(landcover_cat) <- levels(landcover)
names(landcover_cat) <- "landcover"

# create an empty raster to use as a template:
skeetch_rast <- rast(skeetch_WGS, ncols = 2538, nrows = 3497)
skeetch_rast

# resample to match landcover resolution:
skeetch_rast <- resample(skeetch_rast, landcover)
skeetch_rast



## Predictor Data ##



# read in BEC map from bcmaps
bc_bec <- bcmaps::bec(ask = interactive(), force = FALSE)
bc_bec

# convert to SpatVector so we can make it a SpatRaster:
# rasterize bc_bec:
bc_bec <- vect(bc_bec)
bc_bec <- terra::project(bc_bec, "EPSG:4326")
skeetch_bec <- crop(bc_bec, skeetch_extent)
bec_rast <- rasterize(skeetch_bec, skeetch_rast, field = "BGC_LABEL")
bec_rast
bec_cat <- as.factor(bec_rast)
levels(bec_cat) <- levels(bec_rast)
names(bec_cat) <- "bec"
plot(bec_cat)

# elevation data for North America
elevation_bc <- bcmaps::cded_terra(aoi = skeetch_bbox)
# reproject to WGS84:
elevation <- terra::project(elevation_bc, "EPSG:4326")
elevation
# resample to match landcover
elevation <- resample(elevation, skeetch_rast, method = "bilinear")
elevation
plot(elevation)

# ecoregions within BC
ecoregions <- bcmaps::ecoregions(ask = interactive())
ecoregions

# convert to SpatVector so we can make it a SpatRaster:
ecoregions <- vect(ecoregions)
ecoregions <- terra::project(ecoregions, "EPSG:4326")
ecoregions <- crop(ecoregions, skeetch_extent)
ecoregions_rast <- rasterize(ecoregions, skeetch_rast, field = "ECOREGION_NAME")
ecoregions_rast
ecoregions_cat <- as.factor(ecoregions_rast)
levels(ecoregions_cat) <- levels(ecoregions_rast)
names(ecoregions_cat) <- "ecoregions"

# human footprint
footprint <- geodata::footprint(year = "2009", path = "data/raw/")
footprint
footprint <- crop(footprint, skeetch_extent)
footprint <- resample(footprint, skeetch_rast)
footprint_cat <- as.factor(footprint)
levels(footprint_cat) <- levels(footprint)
names(footprint_cat) <- "footprint"
summary(footprint_cat)
# reassign NA values to (Other)
footprint_cat <- subst(footprint_cat, "NA", "(Other)")

hydrozones <- bcmaps::hydrozones(ask = interactive())
hydrozones
hydrozones <- vect(hydrozones)
hydrozones <- terra::project(hydrozones, "EPSG:4326")
hydrozones <- crop(hydrozones, skeetch_extent)
hydrozones <- rasterize(hydrozones, skeetch_rast, field = "HYDROLOGICZONE_NAME")
hydrozones_cat <- as.factor(hydrozones)
levels(hydrozones_cat) <- levels(hydrozones)
names(hydrozones_cat) <- "hydrozones"

# reserves
reserves <- read_sf("data/raw/AL_TA_BC_SHP_eng/AL_TA_BC_2_160_eng.shp")
reserves <- vect(reserves)
reserves
reserves <- terra::project(reserves, "EPSG:4326")
reserves <- crop(reserves, skeetch_extent)
reserves <- rasterize(reserves, skeetch_rast, field = "NAME1")
summary(reserves)

# try to reassign NA values
# reserves[is.na(reserves)] <- "(Other)"
# reserves1 <- terra::classify(reserves, "NA", "(Other)")

reserves_cat <- as.factor(reserves)
levels(reserves_cat) <- levels(reserves)
names(reserves_cat) <- "reserves"
summary(reserves_cat)
cats(reserves_cat)

# land use
# land_use <- read_sf("data/raw/WHSE_BASEMAPPING.BTM_PRESENT_LAND_USE_V1_SVW_loader.kml")


# multilayer raster:
skeetch_multiraster <- c(bec_cat,
                         ecoregions_cat, 
                         elevation, 
                         #footprint_cat,
                         landcover_cat,
                         hydrozones_cat)
                         #reserves_cat)

#skeetch_multiraster[is.na(skeetch_multiraster)] <- -9999

summary(skeetch_multiraster)

# convert presence and absences to sf objects:
skwenkwinem_presence <- st_as_sf(skeetch_presence)
# add column indicating presence
skwenkwinem_presence <- skwenkwinem_presence %>% 
  dplyr::mutate(occurrence = "presence")
skwenkwinem_presence$occurrence <- as.factor(skwenkwinem_presence$occurrence)
# convert to sf object
skwenkwinem_absence <- st_as_sf(skeetch_absence)
# add column indicating absence
skwenkwinem_absence <- skwenkwinem_absence %>% 
  dplyr::mutate(occurrence = "absence")
skwenkwinem_absence$occurrence <- as.factor(skwenkwinem_absence$occurrence)




### Thinning Occurrences ###



# thin the occurrences to have one per cell in the skeetch_rast raster
set.seed(1234567)
thin_cell <- thin_by_cell(skwenkwinem_presence, raster = skeetch_rast)
nrow(thin_cell) # 55

# thin further to remove points closer than 250 m
# default is metres
set.seed(1234567)
thin_dist <- thin_by_dist(thin_cell, dist_min = 250)
nrow(thin_dist) # 33

ggplot() +
  geom_spatraster(data = skeetch_rast, aes()) +
  geom_sf(data = thin_dist) # thinned occurrences

# thin absences to one per cell
set.seed(1234567)
thin_cell_abs <- thin_by_cell(skwenkwinem_absence, raster = skeetch_rast)
nrow(thin_cell_abs) # 180

# thin absences to minimum distance of 250 m
set.seed(1234567)
thin_dist_abs <- thin_by_dist(thin_cell_abs, dist_min = 250)
nrow(thin_dist_abs) # 169

# join thinned presences and absences
pres_abs <- full_join(as.data.frame(thin_dist), 
                      as.data.frame(thin_dist_abs))
pres_abs <- st_as_sf(pres_abs)

# plot presences and absences
ggplot() +
  geom_spatraster(data = skeetch_rast, aes()) +
  geom_sf(data = pres_abs, aes(col = occurrence))


# Extract variables from predictors_multirast for all presences and pseudoabsences
summary(skeetch_multiraster)
nrow(pres_abs) # 235 with no distance thinning, 202 with thinning
nrow(skeetch_multiraster) # 3609

pres_abs_pred <- pres_abs %>% 
  bind_cols(terra::extract(skeetch_multiraster, pres_abs, ID = FALSE, na.rm = TRUE))
nrow(pres_abs_pred) # 235 with no distance thinning, 202 with thinning

# inspect the variables for collinearity
pairs(skeetch_multiraster) 

predictors_uncorr <- filter_high_cor(skeetch_multiraster, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
pres_abs_pred <- pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "occurrence")))
pres_abs_pred

# now subset the uncorrelated predictors within the multiraster
predictors_input <- skeetch_multiraster[[predictors_uncorr]]
predictors_input



#### Fit the model by cross-validation ####



# use a recipe to define how to handle our dataset
# need to define the formula (class is the outcome, all other variables are predictors)
# for sf objects, geometry is auto-replaced by X and Y and assigned as coords, therefore not used as predictors
model_recipe <- recipe(pres_abs_pred, formula = occurrence ~ .)
model_recipe

# tidymodels assumes the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted
pres_abs_pred %>% check_sdm_presence(occurrence)

# build a workflow_set of different models, defining which hyperparameters we want to tune
# for most commonly used models, tidysdm auto chooses the most important parameters
skwenkwinem_models <- 
  workflow_set(
    preproc = list(default = model_recipe), 
    models = list(
      #glm = sdm_spec_glm(), # standard GLM specs
      rf = sdm_spec_rf(), # rf specs with tuning
      #gbm = sdm_spec_boost_tree(), # boosted tree specs with tuning
      maxent = sdm_spec_maxent() # maxent specs with tuning
    ), 
    # make all combos of preproc and models:
    cross = TRUE
  ) %>% 
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())

# set up spatial block cross-validation to tune and assess models:
# 80:20 split with 5 folds (v = 5) (supported by literature review)
set.seed(100)
cross_val <- spatial_block_cv(pres_abs_pred, v = 5)
autoplot(cross_val)

# use block CV folds to tune and assess models
# tutorial uses 3 combos of hyperparameters and says this is far too few for real life
# 10 combos of hyperparameters = ~ 3 mins computation time, less crowded plots
# 20 combos of hyperparameters = ~ 3 mins computation time, crowded plots
# 10 combos = 25 minute computation time on February 14th
set.seed(1234567)
skwenkwinem_models <- 
  skwenkwinem_models %>% 
  workflow_map("tune_grid", 
               resamples = cross_val, grid = 10, # attempting 10 combos of hyperparameters
               metrics = sdm_metric_set(), verbose = TRUE
  ) 

# want workflow_set to correctly detect no tuning parameters for GLM
# inspect performance of models:
autoplot(skwenkwinem_models)
model_metrics <- collect_metrics(skwenkwinem_models)
model_metrics



#### Ensemble ####



# select the best set of parameters for each model
# algorithms in the ensemble are auto-fitted to the full training dataset
# therefore ready to make predictions
skwenkwinem_ensemble <- simple_ensemble() %>% 
  add_member(skwenkwinem_models, metric = "roc_auc") # or boyce_cont or tss_max
skwenkwinem_ensemble
autoplot(skwenkwinem_ensemble)

# a tabular form of the model metrics:
skwenkwinem_ensemble_metrics <-  collect_metrics(skwenkwinem_ensemble) 
# need tidysdm version > 0.9.3 for this to work

# write to file:
# write.csv(skwenkwinem_ensemble_metrics, file = "outputs/skwenkwinem_informed_ensemble_metrics.csv")

# subset the ensemble to only use the best models (AUC > 0.8)
# note: had to subset to 0.7 to include any models when pseudoabsence discs 
# are closer than 50km from presence points
# switched to 0.8 threshold for 50-75km pseudoabsence discs
# take the mean of the available model predictions (default is the mean)
prediction_present_best <- predict_raster(skwenkwinem_ensemble, predictors_input, 
                                          metric_thresh = c("roc_auc", 0.6), 
                                          fun = "mean")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Skwenkwinem Present Prediction", subtitle = "Informed Model", xlab = "Longitude", ylab = "Latitude")# +
#  geom_sf(data = pres_abs_pred %>% filter(class == "presence"))
