# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 05/10
# Following tidysdm tutorial, we input skwenkwinem (Claytonia lanceolata) 
  # occurrence records and informed predictors into the tidysdm pipeline
# note: Skeetchestn-specific code has been annotated out, indicated with 
  # a # with no space following
# Please first run scripts in the following order: 
# 01_data_download.R
# 02_continental_divide.Rmd
# 03a_cropped_extent.R
# [03b is not necessary to run]
# 04_data_processing.R

library(tidysdm) # version >= 0.9.3
library(tidyterra)
library(terra)
library(sf)
library(ggplot2)
library(overlapping)
library(xgboost)
library(ranger)
library(here)

# North American extent (west coast to continental divide)
# new geographic extent created in 02_continental_divide.Rmd
# extent cropped to smaller extent in 04_data_processing.R
# read in extent objects:
# raster to use as a basemap
na_bound_rast <- rast(here::here("data", "extents","na_bound_rast.tif"))
# vector object to use for masking and area calculations
na_bound_vect <- vect(here::here("data", "extents","na_bound_vect.shp"))
#skeetch_vect <- vect(here::here("data", "extents","SkeetchestnTT_2020/SkeetchestnTT_2020.shp"))
# reproject to WGS84
#skeetch_vect <- terra::project(skeetch_vect, "EPSG:4326")


# read in skwenkwinem occurrences:
# cropped to proper study extent in 04_data_processing.R
skwenkwinem_vect <- vect(here::here("data","processed", "skwenkwinem_masked.shp"))
# mask to study area (all occurrences outside bounds set to NA)
skwenkwinem_vect <- mask(skwenkwinem_vect, na_bound_vect)
# cast to sf object
skwenkwinem_sf <- st_as_sf(skwenkwinem_vect)

# read in multilayer raster with predictor data, created in
# 04_data_processing.R
predictors_multi <- rast(here::here("data", "processed", "predictors_multi.tif"))

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
nrow(thin_cell) # 2565

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

## GOOD!

### Pseudoabsences ###

# sample pseudoabsences/background points
# constrain pseudoabsences to be between 50 and 75km from any presences
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

# Extract values from predictors_multirast layers for all presences and pseudoabsences
summary(predictors_multi) # 40 000 + NAs per column
nrow(pres_abs) # 9449
nrow(predictors_multi) # 3143

pres_abs_pred <- pres_abs %>% 
  bind_cols(terra::extract(predictors_multi, pres_abs, ID = FALSE, na.rm = TRUE))
nrow(pres_abs_pred) # 9449

# after this step, no NA values in predictors_multi equivalent from tutorial
# but I have NA values from predictors_multi
summary(pres_abs_pred) # still some NAs 

# remove rows with NA values
pres_abs_pred <- na.omit(pres_abs_pred)
nrow(pres_abs_pred) # 8987, 462 rows removed

# skipped non-overlapping distribution step in tutorial

# inspect the variables for collinearity
# pairs(predictors_multi) 
  # error said plot margins too large

# may need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
set.seed(1234567)
predictors_sample <- terra::spatSample(predictors_multi, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)


# subset to variables below 0.8 Pearson's correlation coefficient
# predictors_multi = SpatRaster with predictor data (all numeric, no NAs)

# sub predictors_multi with predictors_sample if code below won't run
predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# suggests removing soil pH, due to correlation with soil temp; let's double check:

cor.test(values(predictors_sample$soil_phh2o_0_5), values(predictors_sample$soil_temp_0_5), na.rm =TRUE)

# cor = 0.876

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
pres_abs_pred <- pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "class")))
pres_abs_pred

# now subset the uncorrelated predictors within the multiraster
predictors_multi_input <- predictors_multi[[predictors_uncorr]]


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

# write to file
write.csv(model_metrics, file = here::here("outputs", "skwenkwinem_informed_model_metrics.csv"))



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
write.csv(skwenkwinem_ensemble_metrics, file = here::here("outputs", "skwenkwinem_informed_ensemble_metrics.csv"))



#### Projecting to the Present ####



# predictions using the ensemble
# default is taking the mean of the predictions from each model
# line below uses over 10GB of RAM
# need to input uncorrelated predictor data
# prediction_present_multirast <- predict_raster(skwenkwinem_ensemble, predictors_multi_input)

# ggplot() +
# geom_spatraster(data = prediction_present_multirast, aes (fill = mean)) +
# scale_fill_terrain_c()# + # c for continuous
# geom_sf(data = pres_abs_pred %>% filter(class == "presence"))

# subset the ensemble to only use the best models (AUC > 0.8)
# note: had to subset to 0.7 to include any models when pseudoabsence discs 
# are closer than 50km from presence points
# switched to 0.8 threshold for 50-75km pseudoabsence discs
# take the mean of the available model predictions (default is the mean)
prediction_present_best <- predict_raster(skwenkwinem_ensemble, predictors_multi_input, 
                                          metric_thresh = c("roc_auc", 0.8), 
                                          fun = "mean")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Skwenkwinem Present Prediction", subtitle = "Informed Model", xlab = "Longitude", ylab = "Latitude")# +
#  geom_sf(data = pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_present_best, filename = here::here("outputs", "skwenkwinem_informed_predict_present_cont.tif"), overwrite = TRUE)



# Binary Predictions
# clear unused R memory before running the below code - apparently need as much RAM as possible



# desirable to have binary predictions (presence/absence) rather than probability of occurrence
# calibrate threshold used to convert probabilities into classes
skwenkwinem_ensemble <- calib_class_thresh(skwenkwinem_ensemble,
                                   class_thresh = "tss_max")

prediction_present_binary <- predict_raster(skwenkwinem_ensemble, 
                                            predictors_multi_input, 
                                            type = "class", 
                                            class_thresh = c("tss_max") 
)

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  # geom_sf(data= pres_abs_pred %>% filter(class == "presence")) +
  labs(title = "Skwenkwinem Present Prediction", subtitle = "Informed Model", xlab = "Longitude", ylab = "Latitude")

# write to file
writeRaster(prediction_present_binary, filename = here::here("outputs", "skwenkwinem_informed_predict_present_binary.tif"), overwrite = TRUE)



#### Visualizing the Contribution of Individual Variables ####



# for a written explanation of variable importance:
# using DALEX library, integrated with tidysdm
# create an explainer object
library(DALEX)
explainer_skwenkwinem_ensemble <- explain_tidysdm(skwenkwinem_ensemble)
vip_ensemble <- model_parts(explainer = explainer_skwenkwinem_ensemble, 
                            type = "variable_importance")
plot(vip_ensemble)


# get variable importance metrics:
informed_var_imp <- vip_ensemble %>% 
  dplyr::filter(variable != "_baseline_" & variable != "_full_model_")

# get mean dropout loss for each of the variables in a dataframe
informed_var_imp_df <- as.data.frame(vip_ensemble) %>% 
  group_by(variable) %>% 
  summarize(across(dropout_loss, list(mean = mean))) %>% 
  arrange(desc(dropout_loss_mean)) %>% 
  dplyr::filter(variable != "_baseline_" & variable != "_full_model_")

write.csv(informed_var_imp, file = here::here("outputs", "informed_variable_importance.csv"))

# plot variable importance
informed_var_imp_boxplot <- ggplot(informed_var_imp, 
                                   aes(x = reorder(variable, -dropout_loss),
                                       y = dropout_loss,
                                       fill = variable)) +
  geom_boxplot(fill = "lightgrey", colour = "grey40", size = 0.65) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0.05, 0.325),
                     breaks = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) +
  scale_x_discrete(labels = c("soil temperature", "elevation", 
                              "anthropogenic biomes","landcover")) +
  scale_fill_viridis_d() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Variable", y = "Variable importance") +
  theme(axis.title.y = element_blank())

informed_var_imp_boxplot

ggsave(here::here("outputs", "informed_var_imp.png"), informed_var_imp_boxplot)



# Extract Skeetchestn values for the two most important predictors: 
  # elevation and soil temperature
elevation_skeetch <- terra::extract(predictors_multi$elevation, skeetch_vect)
summary(elevation_skeetch)

#soil_temp_skeetch <- terra::extract(predictors_multi$soil_temp_0_5, skeetch_vect)
#summary(soil_temp_skeetch)
# remove NAs:
#soil_temp_skeetch <- drop_na(soil_temp_skeetch)

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
    pred = predict(skwenkwinem_ensemble, anth_biome_data)$mean
  )
anth_biome_data

anth_biome_plot <- ggplot(anth_biome_data) +
  geom_col(aes(x = anth_biome, y = pred)) +
  scale_x_discrete(name = "Anthropogenic biomes", 
                   labels = c("urban", "dense settlements", "irrigated villages", 
                              "cropped and pastoral villages", "rainfed villages",
                              "rainfed mosaic villages", "residential irrigated cropland",
                              "residential rainfed mosaic", "populated irrigated cropland",
                              "populated rainfed cropland", "remote cropland",
                              "residential rangelands", "populated rangelands",
                              "remote rangelands", "populated forests", "remote forests",
                              "wild forests", "sparse trees", "barren")) +
  theme(axis.title.x = element_text(angle = 45)) +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

ggsave(here::here("outputs", "anth_biome_response.png"), anth_biome_plot)


# investigate the contribution of elevation:
elevation_prof <- model_recipe %>%  # recipe from above
  step_profile(-elevation, profile = vars(elevation)) %>% 
  prep(training = pres_abs_pred)

elevation_data <- bake(elevation_prof, new_data = NULL)

elevation_data <- elevation_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, elevation_data)$mean
  )

# plot marginal response with average elevation from Skeetch and 
  # average study site elevation
elev_plot <- ggplot(elevation_data, aes(x = elevation, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Elevation (m)") +
  scale_y_continuous(name = "Relative habitat suitability", 
                     limits = c(0, 1), 
                     expand = c(0, 0)) +
  #annotate("rect", xmin = min(elevation_skeetch$elevation), 
           #xmax = max(elevation_skeetch$elevation), ymin = 0, ymax = 1, 
           #alpha = .25, fill = "grey") +
  theme_classic()

ggsave(here::here("outputs", "elevation_response.png"), elev_plot)


# investigate the contribution of lndcvr_na:
landcover_prof <- model_recipe %>%  # recipe from above
  step_profile(-landcover, profile = vars(landcover)) %>% 
  prep(training = pres_abs_pred)

landcover_data <- bake(landcover_prof, new_data = NULL)

landcover_data <- landcover_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, landcover_data)$mean
  )

land_plot <- ggplot(landcover_data, aes(x = landcover, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_discrete(name = "Landcover") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(name = "Relative habitat suitability") +
  theme_classic()

ggsave(here::here("outputs", "landcover_response.png"), land_plot)


# investigate the contribution of soil_temp_0_5:
soil_temp_0_5_prof <- model_recipe %>%  # recipe from above
  step_profile(-soil_temp_0_5, profile = vars(soil_temp_0_5)) %>% 
  prep(training = pres_abs_pred)

soil_temp_0_5_data <- bake(soil_temp_0_5_prof, new_data = NULL)

soil_temp_0_5_data <- soil_temp_0_5_data %>% 
  mutate(
    pred = predict(skwenkwinem_ensemble, soil_temp_0_5_data)$mean
  )

soiltemp_plot <- ggplot(soil_temp_0_5_data, aes(x = soil_temp_0_5, y = pred)) +
  geom_point(alpha = 0.25, cex = 4) +
  scale_x_continuous(name = "Soil temperature seasonality (°C)") +
  scale_y_continuous(name = "Relative habitat suitability", 
                     limits = c(0, 1), 
                     expand = c(0, 0)) +
  #annotate("rect", xmin = min(soil_temp_skeetch$soil_temp_0_5), 
           #xmax = max(soil_temp_skeetch$soil_temp_0_5), ymin = 0, ymax = 1, 
           #alpha = .25, fill = "grey") +
  theme_classic()

ggsave(here::here("outputs", "soil_temp_0_5_response.png"), soiltemp_plot)
