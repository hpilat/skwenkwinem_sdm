# This is script 06/07
# Following tidysdm tutorial, we input Skwenkwinem (Claytonia lanceolata) occurrence records 
# and WorldClim predictors at 30 arcsec resolution into the tidysdm pipeline
# Please first run scripts in the following order: 
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing.R

# dir.create("outputs/")

library(tidysdm)
library(tidyterra)
library(sf)
library(terra)
library(ggplot2)
library(overlapping)

# North American extent (west coast to continental divide)
# new geographic extent created in 02_continental_divide.Rmd
# extent cropped to smaller extent in 03_extent_cropped.R
# read in extent objects:
# raster to use as a basemap
na_bound_rast <- rast("data/extents/na_bound_rast.tif")
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")

# read in skwenkwinem occurrences:
# cropped to proper study extent in 04_data_processing.R
skwenkwinem_vect <- vect("data/processed/skwenkwinem_masked.shp")
# mask to study area (all occurrences outside bounds set to NA)
skwenkwinem_vect <- mask(skwenkwinem_vect, na_bound_vect)
# cast to sf object
skwenkwinem_sf <- st_as_sf(skwenkwinem_vect)

# read in processed WorldClim rasters
climate_present <- rast("data/processed/worldclim_present_masked.tif")
climate_future <- rast("data/processed/worldclim_future_masked.tif")

# use tidyterra package for plotting so ggplot can be used with terra rasters
# aes(fill = layer) refers to column name in na_bound_rast
ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = skwenkwinem_sf) # sf object with coordinates



### Thinning Occurrences ###



# thin the occurrences to have one per cell in the na_bound_rast raster

set.seed(1234567)
thin_cell <- thin_by_cell(skwenkwinem_sf, raster = na_bound_rast)
nrow(thin_cell) # 2462

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = thin_cell) # thinned occurrences

# thin further to remove points closer than 15km
set.seed(1234567)
thin_dist <- thin_by_dist(skwenkwinem_sf, dist_min = km2m(15))
nrow(thin_dist) # 859

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = thin_dist)



### Pseudoabsences ###



# sample pseudoabsences/background points
# constrain pseudoabsences to be between 50 and 75km from any presences
# choice of disc size is arbitrary but was found to optimize model performance
# select 10 times as many pseudoabsences as presences 
# pres_abs_pred will then have presences and pseudoabsences
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
summary(climate_present) # 40 000 + NAs per column
nrow(pres_abs) # 9449
nrow(climate_present) # 3143

pres_abs_pred <- pres_abs %>% 
  bind_cols(terra::extract(climate_present, pres_abs, ID = FALSE, na.rm = TRUE))
nrow(pres_abs_pred) # 9449
summary(pres_abs_pred) # still some NAs

# remove rows with NA values
pres_abs_pred <- na.omit(pres_abs_pred)
nrow(pres_abs_pred) # 9208, 241 rows removed
summary(pres_abs_pred) # No NA values

# skipped non-overlapping distribution step in tutorial

# need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
set.seed(1234567)
predictors_sample <- terra::spatSample(climate_present, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)

pairs(predictors_sample)

# subset to variables below 0.8 Pearson's correlation coefficient
# climate_present = SpatRaster with predictor data (all numeric, no NAs)

predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
pres_abs_pred <- pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "class")))
pres_abs_pred

# now subset the uncorrelated predictors from climate_present
climate_present_uncorr <- climate_present[[predictors_uncorr]]
climate_present_uncorr



#### Fit the model by cross-validation ####



# use a recipe to define how to handle our dataset
# need to define the formula (class is the outcome, all other variables are predictors)
# for sf objects, geometry is auto-replaced by X and Y and assigned as coords, therefore not used as predictors
skwenkwinem_recipe <- recipe(pres_abs_pred, formula = class ~ .)
skwenkwinem_recipe

# tidymodels assumes the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted
pres_abs_pred %>% check_sdm_presence(class)

# build a workflow_set of different models, defining which hyperparameters we want to tune
# for most commonly used models, tidysdm auto chooses the most important parameters
skwenkwinem_models <- 
  workflow_set(
    preproc = list(default = skwenkwinem_recipe), 
    models = list(
      glm = sdm_spec_glm(), # standard GLM specs
      rf = sdm_spec_rf(), # rf specs with tuning
      gbm = sdm_spec_boost_tree(), # boosted tree specs with tuning
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

# write to file
write.csv(model_metrics, file = "outputs/skwenkwinem_bioclim30s_model-metrics.csv")



## Ensemble ##



# use AUC as metric to choose best random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full
# training dataset and therefore ready to make predictions
skwenkwinem_ensemble <- simple_ensemble() %>% 
  add_member(skwenkwinem_models, metric = "roc_auc")
# can also use roc_auc and tss_max as metrics
skwenkwinem_ensemble
autoplot(skwenkwinem_ensemble)
# need to have tidysdm version 0.9.3 or greater for this to work
skwenkwinem_ensemble_metrics <- collect_metrics(skwenkwinem_ensemble)

# write to file:
write.csv(skwenkwinem_ensemble_metrics, file = "outputs/skwenkwinem_bioclim30s_ensemble_metrics.csv")



## Projecting to the Present ##



# make predictions with the ensemble
# prediction_present <- predict_raster(skwenkwinem_ensemble, climate_present_uncorr)
# ggplot() +
#  geom_spatraster(data = prediction_present, aes(fill = mean)) +
#  scale_fill_terrain_c() + # "c" for continuous variables


# subset the model to only use the best models, based on AUC
# set threshold of 0.8 for AUC
# take the median of the available model predictions (mean is the default)
prediction_present_best <- predict_raster(skwenkwinem_ensemble, 
                                          climate_present_uncorr, 
                                          metric_thresh = c("roc_auc", 0.8), 
                                          fun= "mean")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Skwenkwinem Present Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")
# geom_sf(data = pres_abs_pred %>% filter(class == "presence"))
# model gives us probability of occurrence

# write to file
writeRaster(prediction_present_best, filename = "outputs/skwenkwinem_bioclim30s_predict_present_cont.tif")


# can convert to binary predictions (present vs absence)

skwenkwinem_ensemble_binary <- calib_class_thresh(skwenkwinem_ensemble, 
                                                  class_thresh = "tss_max"
)

prediction_present_binary <- predict_raster(skwenkwinem_ensemble_binary, 
                                            climate_present_uncorr, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))
prediction_present_binary

# plot the binary map
ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  labs(title = "Skwenkwinem Present Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude") # +
# geom_sf(data = pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_present_binary, filename = "outputs/skwenkwinem_bioclim30s_predict_present_binary.tif", overwrite = TRUE)



#### Projecting to the Future ####



# all 19 bioclimatic variables
# subset uncorrelated predictors from climate_future
climate_future_uncorr <- climate_future[[predictors_uncorr]]
climate_future_uncorr

# predict using the ensemble:
# prediction_future <- predict_raster(skwenkwinem_ensemble, climate_future_uncorr)

# ggplot() +
# geom_spatraster(data = prediction_future, aes(fill = mean)) +
# scale_fill_terrain_c()

prediction_future_best <- predict_raster(skwenkwinem_ensemble, 
                                         climate_future_uncorr, 
                                         metric_thresh = c("roc_auc", 0.8), 
                                         fun= "mean")

ggplot() +
  geom_spatraster(data = prediction_future_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Skwenkwinem Future Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")

# write to file
writeRaster(prediction_future_best, filename = "outputs/skwenkwinem_bioclim30s_predict_future_cont.tif", overwrite = TRUE)



# convert predictions to binary (presence/absence)
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence
# can convert to binary predictions (present vs absence)


skwenkwinem_ensemble_binary <- calib_class_thresh(skwenkwinem_ensemble, 
                                                  class_thresh = "tss_max"
                                                  )

prediction_future_binary <- predict_raster(skwenkwinem_ensemble_binary, 
                                           climate_future_uncorr, 
                                           type = "class", 
                                           class_thresh = c("tss_max"))
prediction_future_binary

ggplot() +
  geom_spatraster(data = prediction_future_binary, aes(fill = binary_mean)) +
  labs(title = "Skwenkwinem Future Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")#+
# geom_sf(data = pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_future_binary, filename = "outputs/skwenkwinem_bioclim30s_predict_future_binary.tif", overwrite = TRUE)



#### Visualizing the Contribution of Individual Variables ####



# for a written explanation of variable importance:
# using DALEX library, integrated with tidysdm
# create an explainer object
library(DALEX)
explainer_skwenkwinem_ensemble <- explain_tidysdm(skwenkwinem_ensemble)
vip_ensemble <- model_parts(explainer = explainer_skwenkwinem_ensemble, 
                            type = "variable_importance")
vip_ensemble
plot(vip_ensemble)


# Marginal Response Curves


# marginal response curves can show the effect of a variable while keeping
# all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
# the marginal prediction
# uncorrelated predictors:
predictors_uncorr


# investigate the contribution of bio02:
bio02_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio02, profile = vars(bio02)) %>% 
  prep(training = pres_abs_pred)

bio02_data <- bake(bio02_prof, new_data = NULL)

bio02_data <- bio02_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio02_data)$mean
  )

ggplot(bio02_data, aes(x = bio02, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio07:
bio03_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio03, profile = vars(bio03)) %>% 
  prep(training = pres_abs_pred)

bio03_data <- bake(bio03_prof, new_data = NULL)

bio03_data <- bio03_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio03_data)$mean
  )

ggplot(bio03_data, aes(x = bio03, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio05:
bio07_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio07, profile = vars(bio07)) %>% 
  prep(training = pres_abs_pred)

bio07_data <- bake(bio07_prof, new_data = NULL)

bio07_data <- bio07_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio07_data)$mean
  )

ggplot(bio07_data, aes(x = bio07, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio08:
bio08_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio08, profile = vars(bio08)) %>% 
  prep(training = pres_abs_pred)

bio08_data <- bake(bio08_prof, new_data = NULL)

bio08_data <- bio08_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio08_data)$mean
  )

ggplot(bio08_data, aes(x = bio08, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio09:
bio09_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio09, profile = vars(bio09)) %>% 
  prep(training = pres_abs_pred)

bio09_data <- bake(bio09_prof, new_data = NULL)

bio09_data <- bio09_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio09_data)$mean
  )

ggplot(bio09_data, aes(x = bio09, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio14:
bio13_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio13, profile = vars(bio13)) %>% 
  prep(training = pres_abs_pred)

bio13_data <- bake(bio13_prof, new_data = NULL)

bio13_data <- bio13_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio13_data)$mean
  )

ggplot(bio13_data, aes(x = bio13, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio15:
bio15_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio15, profile = vars(bio15)) %>% 
  prep(training = pres_abs_pred)

bio15_data <- bake(bio15_prof, new_data = NULL)

bio15_data <- bio15_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio15_data)$mean
  )

ggplot(bio15_data, aes(x = bio15, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio18:
bio18_prof <- skwenkwinem_recipe %>%  # recipe from above
  step_profile(-bio18, profile = vars(bio18)) %>% 
  prep(training = pres_abs_pred)

bio18_data <- bake(bio18_prof, new_data = NULL)

bio18_data <- bio18_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, bio18_data)$mean
  )

ggplot(bio18_data, aes(x = bio18, y = pred)) +
  geom_point(alpha = .5, cex = 1)



## Repeated Ensembles ##



# explore the effect of thinning and sampling pseudoabsences on model performance
# create a list of simple_ensembles by looping through the SDM pipeline
# create an empty object to store the simple ensembles we will create:
ensemble_list <- list()
set.seed(123) # make sure seed is set outside of the loop

for (i_repeat in 1:3) {
  # thin the data
  thin_rep <- thin_by_cell(pres_abs_pred, raster = climate_present)
  thin_rep <- thin_by_dist(thin_rep, dist_min = 5000)
  # sample pseudo-absences
  thin_rep <- sample_pseudoabs(thin_rep,
                               n = 3 * nrow(thin_rep),
                               raster = climate_present,
                               method = c("dist_min", 50000)
  )
  # get climate
  thin_rep <- thin_rep %>%
    bind_cols(terra::extract(climate_present, thin_rep, ID = FALSE))
  # create folds
  thin_rep_cv <- spatial_block_cv(thin_rep, v = 5) # 5 folds
  # create a recipe
  thin_rep_rec <- recipe(thin_rep, formula = class ~ .)
  # create a workflow_set
  thin_rep_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = thin_rep_rec),
      models = list(
        # the standard glm specs
        glm = sdm_spec_glm(),
        # maxent specs with tuning
        maxent = sdm_spec_maxent()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())
  
  # train the model
  thin_rep_models <-
    thin_rep_models %>%
    workflow_map("tune_grid",
                 resamples = thin_rep_cv, grid = 10,
                 metrics = sdm_metric_set(), verbose = TRUE
    )
  # make an simple ensemble and add it to the list
  ensemble_list[[i_repeat]] <- simple_ensemble() %>%
    add_member(thin_rep_models, metric = "roc_auc")
}

# now create repeat_ensemble from the list:
thin_rep_ens <- repeat_ensemble() %>% add_repeat(ensemble_list)
thin_rep_ens

# predict by taking the mean and median of all models
thin_rep_ens <- predict_raster(thin_rep_ens, 
                               climate_present, 
                               fun = c("mean", "median"))

ggplot() +
  geom_spatraster(data = thin_rep_ens, aes(fill = median)) +
  scale_fill_terrain_c() +
  labs(title = "Skwenkwinem Prediction Sensitivity", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")
# convert to binary and calculate area?