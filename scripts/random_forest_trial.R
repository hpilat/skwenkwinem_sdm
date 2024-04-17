# Following tidysdm tutorial, we input skwenkwinem (Claytonia lanceolata) 
# occurrence records and informed predictors into the tidysdm pipeline
# Please first run scripts in the following order: 
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing.R

library(tidysdm) # version >= 0.9.3
library(tidyterra)
library(terra)
library(sf)
library(ggplot2)
library(overlapping)
library(xgboost)

# North American extent (west coast to continental divide)
# new geographic extent created in 02_continental_divide.Rmd
# extent cropped to smaller extent in 04_data_processing.R
# read in extent objects:
# raster to use as a basemap
na_bound_rast <- rast("data/extents/na_bound_rast.tif")
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")

# read in skwenkwinem occurrences:
# cropped to proper study extent in 04_data_processing.R
skwenkwinem_vect <- vect("data/processed/skwenkwinem_masked.shp")
# mask to study area (all occurrences outside bounds set to NA)
skwenkwinem_vect <- mask(skwenkwinem_vect, na_bound_vect)
# cast to sf object
skwenkwinem_sf <- st_as_sf(skwenkwinem_vect)

# read in multilayer raster with predictor data, created in
# 04_data_processing.R
predictors_multi <- rast("data/processed/predictors_multi.tif")


# plot occurrences directly on raster with predictor variables

# use tidyterra package for plotting so ggplot can be used with terra rasters
# aes(fill = layer) refers to column name in na_bound_rast
ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = skwenkwinem_sf) # sf object with coordinates



### Thinning Occurrences ###



# thin the occurrences to have one per cell in the na_bound_rast raster
set.seed(1234567)
thin_cell <- thin_by_cell(skwenkwinem_sf, raster = na_bound_rast)
nrow(thin_cell) # 2564

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = thin_cell) # thinned occurrences

# thin further to remove points closer than 5km
# default is metres, could input 5000 or use km2m(5)
# attempted 5km, filter_high_cor below wouldn't run, so try 10
# 10 still didn't work, try 15?
set.seed(1234567)
thin_dist <- thin_by_dist(skwenkwinem_sf, dist_min = km2m(15))
nrow(thin_dist) # 859

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = thin_dist)



### Pseudoabsences ###



# sample pseudoabsences/background points
# constrain pseudoabsences to be between 50 and 75km from any presences
# choice of 5 and 15km is arbitrary
# select 10 times as many pseudoabsences as presences 
# (recommended 10 000 pseudoabsences by lit review)
# pres_abs will then have presences and pseudoabsences
set.seed(1234567)
pres_abs <- sample_pseudoabs(thin_dist, 
                             n = 10 * nrow(thin_dist), 
                             raster = na_bound_rast, 
                             method = c("dist_disc", km2m(50), km2m(75))
)
nrow(pres_abs) # 9449 

# plot presences and absences
ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = pres_abs, aes(col = class))



### Variable Selection ###



# Extract variables from predictors_multirast for all presences and pseudoabsences
summary(predictors_multi) # 40 000 + NAs per column
nrow(pres_abs) # 9449
nrow(predictors_multi) # 3143

pres_abs_pred <- pres_abs %>% 
  bind_cols(terra::extract(predictors_multi, pres_abs, ID = FALSE, na.rm = TRUE))
nrow(pres_abs_pred) # 9449

# after this step, no NA values in predictors_multi equivalent from tutorial
# but I have NA values from predictors_multi
summary(pres_abs_pred) # still some NAs, bioclim script magically has none at this point

# remove rows with NA values
pres_abs_pred <- na.omit(pres_abs_pred)
nrow(pres_abs_pred) # 8904, 545 rows removed

# skipped non-overlapping distribution step in tutorial

# inspect the variables for collinearity
pairs(predictors_multi)

# may need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
set.seed(1234567)
predictors_sample <- terra::spatSample(predictors_multi, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)


# subset to variables below 0.8 Pearson's correlation coefficient
# predictors_multi = SpatRaster with predictor data (all numeric, no NAs)

# below code was taking forever to run, but no delays in the bioclim code
# sub predictors_multi with predictors_sample if code below won't run
predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
pres_abs_pred <- pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "class")))
pres_abs_pred

# now subset the uncorrelated predictors within the multiraster
predictors_multi_input <- predictors_multi[[predictors_uncorr]]
predictors_multi_input


#### Fit the model by cross-validation ####



# use a recipe to define how to handle our dataset
# need to define the formula (class is the outcome, all other variables are predictors)
# for sf objects, geometry is auto-replaced by X and Y and assigned as coords, therefore not used as predictors
model_recipe <- recipe(pres_abs_pred, formula = class ~ .)
model_recipe

# tidymodels assumes the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted
pres_abs_pred %>% check_sdm_presence(class)



# build a workflow_set of different models, defining which hyperparameters we want to tune
# for most commonly used models, tidysdm auto chooses the most important parameters
skwenkwinem_rf <-
  workflow_set(
    preproc = list(default = model_recipe), 
    models = list(rf = sdm_spec_rf()), 
    cross = TRUE)
  

# set up spatial block cross-validation to tune and assess models:
# 80:20 split with 5 folds (v = 5) (supported by literature review)
set.seed(100)
cross_val <- spatial_block_cv(pres_abs_pred, v = 5)
autoplot(cross_val)

# use block CV folds to tune and assess models
# tutorial uses 3 combos of hyperparameters and says this is far too few for real life
# 10 combos of hyperparameters = ~ 3 mins computation time, less crowded plots
# 20 combos of hyperparameters = ~ 3 mins computation time, crowded plots
set.seed(1234567)
skwenkwinem_rf <- 
  skwenkwinem_rf %>% 
  workflow_map("tune_grid", 
               resamples = cross_val, grid = 10, # attempting 10 combos of hyperparameters
               metrics = sdm_metric_set(), verbose = TRUE
  ) 

# want workflow_set to correctly detect no tuning parameters for GLM
# inspect performance of models:
autoplot(skwenkwinem_rf)
rf_model_metrics <- collect_metrics(skwenkwinem_rf)

# write to file
write.csv(rf_model_metrics, file = "outputs/skwenkwinem_informed_rf_metrics.csv")

rf_ensemble <- simple_ensemble() %>% 
  add_member(skwenkwinem_rf, metric = "roc_auc") # or boyce_cont or tss_max
rf_ensemble
autoplot(rf_ensemble)



#### Visualizing the Contribution of Individual Variables ####



# for a written explanation of variable importance:
# using DALEX library, integrated with tidysdm
# create an explainer object
library(DALEX)
library(vip)
explainer_skwenkwinem_rf <- explain_tidysdm(skwenkwinem_rf)
vip_rf <- model_parts(explainer = explainer_skwenkwinem_rf, 
                            type = "variable_importance")
plot(vip_rf)


# marginal response curves can show the effect of a variable while keeping
# all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
# the marginal prediction

# investigate the contribution of anth_biome:
anth_biome_prof <- model_recipe %>%  # recipe from above
  step_profile(-anth_biome, profile = vars(anth_biome)) %>% 
  prep(training = pres_abs_pred)

anth_biome_data <- bake(anth_biome_prof, new_data = NULL)

anth_biome_data <- anth_biome_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, anth_biome_data)$mean
  )
anth_biome_data

ggplot(anth_biome_data, aes(x = anth_biome, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Anthropogenic biomes") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/anth_biome_response.png")

# get most suitable anthropogenic biomes
anth_biome_most_suitable <- anth_biome_data %>%
  arrange(desc(anth_biome_data$pred))
anth_biome_most_suitable


# investigate the contribution of Climate (climate_zones):
climate_zones_prof <- model_recipe %>%  # recipe from above
  step_profile(-climate_zones, profile = vars(climate_zones)) %>% 
  prep(training = pres_abs_pred)

climate_zones_data <- bake(climate_zones_prof, new_data = NULL)

climate_zones_data <- climate_zones_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, climate_zones_data)$mean
  )

ggplot(climate_zones_data, aes(x = climate_zones, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Climate zones") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/climate_zones_response.png")

# get most suitable climate zones
climate_zones_most_suitable <- climate_zones_data %>%
  arrange(desc(climate_zones_data$pred))
climate_zones_most_suitable


# investigate the contribution of ecoregions:
ecoregions_prof <- model_recipe %>%  # recipe from above
  step_profile(-ecoregions, profile = vars(ecoregions)) %>% 
  prep(training = pres_abs_pred)

ecoregions_data <- bake(ecoregions_prof, new_data = NULL)

ecoregions_data <- ecoregions_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, ecoregions_data)$mean
  )

ggplot(ecoregions_data, aes(x = ecoregions, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Ecoregions (level III)") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/ecoregions_response.png")

# get most suitable ecoregions
ecoregions_most_suitable <- ecoregions_data %>%
  arrange(desc(ecoregions_data$pred))
ecoregions_most_suitable

# investigate the contribution of elevation:
elevation_prof <- model_recipe %>%  # recipe from above
  step_profile(-elevation, profile = vars(elevation)) %>% 
  prep(training = pres_abs_pred)

elevation_data <- bake(elevation_prof, new_data = NULL)

elevation_data <- elevation_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, elevation_data)$mean
  )

ggplot(elevation_data, aes(x = elevation, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Elevation (m)") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/elevation_response.png")

# get most suitable elevation
elevation_most_suitable <- elevation_data %>%
  arrange(desc(elevation_data$pred))
elevation_most_suitable


# investigate the contribution of lndcvr_na:
landcover_prof <- model_recipe %>%  # recipe from above
  step_profile(-landcover, profile = vars(landcover)) %>% 
  prep(training = pres_abs_pred)

landcover_data <- bake(landcover_prof, new_data = NULL)

landcover_data <- landcover_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, landcover_data)$mean
  )

ggplot(landcover_data, aes(x = landcover, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Landcover") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/landcover_response.png")

# get most suitable landcover types
landcover_most_suitable <- landcover_data %>%
  arrange(desc(landcover_data$pred))
landcover_most_suitable

# investigate the contribution of soil_temp_5_15:
soil_temp_5_15_prof <- model_recipe %>%  # recipe from above
  step_profile(-soil_temp_5_15, profile = vars(soil_temp_5_15)) %>% 
  prep(training = pres_abs_pred)

soil_temp_5_15_data <- bake(soil_temp_5_15_prof, new_data = NULL)

soil_temp_5_15_data <- soil_temp_5_15_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, soil_temp_5_15_data)$mean
  )

# convert soil_temp_5_15 to °C:
soil_temp_5_15_data$soil_temp_5_15 <- soil_temp_5_15_data$soil_temp_5_15/10

ggplot(soil_temp_5_15_data, aes(x = soil_temp_5_15, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Soil Temperature (°C)") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/soil_temp_5_15_response.png")

# get most suitable soil temperature
soil_temp_5_15_most_suitable <- soil_temp_5_15_data %>%
  arrange(desc(soil_temp_5_15_data$pred))
soil_temp_5_15_most_suitable


# investigate the contribution of watersheds:
watersheds_prof <- model_recipe %>%  # recipe from above
  step_profile(-watersheds, profile = vars(watersheds)) %>% 
  prep(training = pres_abs_pred)

watersheds_data <- bake(watersheds_prof, new_data = NULL)

watersheds_data <- watersheds_data %>% 
  mutate(
    pred = predict(skwenkwinem_rf, watersheds_data)$mean
  )

ggplot(watersheds_data, aes(x = watersheds, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Watersheds") +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

#ggsave("outputs/watersheds_response.png")

# get most suitable watersheds
watersheds_most_suitable <- watersheds_data %>%
  arrange(desc(watersheds_data$pred))
watersheds_most_suitable

