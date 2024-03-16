# This is script 07/08
# This script plots our ensemble metrics from both models together
  # and our continuous habitat suitability predictions
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(devtools)
# devtools::install_github("h-a-graham/rayvista", dependencies = TRUE)

# Predictions:

# Continuous:
informed_present_continuous <- rast("outputs/skwenkwinem_informed_predict_present_cont.tif")
bioclim30s_present_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_present_cont.tif")
bioclim30s_future_continuous <- rast("outputs/skwenkwinem_bioclim30s_predict_future_cont.tif")

# Read in elevation raster (for hillshading)
elevation <- rast("data/processed/elevation.tif")

# Extent objects:

# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")
# Skeetchestn territory boundary vector for masking:
skeetch_vect <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# transform to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect, "EPSG:4326")

# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extent <- ext(-121.6, -120.1, 50.3, 51.6)




# Isolate AUC metrics from model metrics:



# read in model metrics csv files:
informed_model_metrics <- read.csv("outputs/skwenkwinem_informed_model_metrics.csv", header = TRUE)
bioclim30s_model_metrics <- read.csv("outputs/skwenkwinem_bioclim30s_model_metrics.csv", header = TRUE)

# select only relevant columns and filter out all rows except roc_auc
informed_model_metrics_AUC <- informed_model_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

bioclim30s_model_metrics_AUC <- bioclim30s_model_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc")

# write to new csv to import into word
write.csv(informed_model_metrics_AUC, file = "outputs/skwenkwinem_informed_model_metrics_AUC.csv")
write.csv(bioclim30s_model_metrics_AUC, file = "outputs/skwenkwinem_bioclim30s_model_metrics_AUC.csv")


# repeat above steps for ensemble metrics:
informed_ensemble_metrics <- read.csv("outputs/skwenkwinem_informed_ensemble_metrics.csv", header = TRUE)
bioclim30s_ensemble_metrics <- read.csv("outputs/skwenkwinem_bioclim30s_ensemble_metrics.csv", header = TRUE)

# select only relevant columns and filter out all rows except roc_auc
informed_ensemble_metrics_AUC <- informed_ensemble_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc") %>% 
  # drop .metric column
  dplyr::select("wflow_id", "mean", "std_err") %>% 
  # rename columns to be more informative
  dplyr::rename(algorithm = wflow_id) %>% 
  # add model column and input "informed" in the rows
  add_column(model = "informed", .before = "algorithm")

bioclim30s_ensemble_metrics_AUC <- bioclim30s_ensemble_metrics %>% 
  dplyr::select("wflow_id", ".metric", "mean", "std_err") %>% 
  dplyr::filter(.metric == "roc_auc") %>% 
  # drop .metric column
  dplyr::select("wflow_id", "mean", "std_err") %>% 
  # rename columns to be more informative
  dplyr::rename(algorithm = wflow_id) %>% 
  # add model column and input "informed" in the rows
  add_column(model = "bioclim30s", .before = "algorithm")

# now bind the rows together into 1 object:
ensemble_AUC <- rbind(informed_ensemble_metrics_AUC, bioclim30s_ensemble_metrics_AUC)

# write to new csv to import into word
write.csv(ensemble_AUC, file = "outputs/skwenkwinem_ensemble_metrics.csv")

# plot ensemble metrics together
ensemble_metrics <- ggplot(ensemble_AUC, aes(x = algorithm, y = mean, colour = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  ggtitle("Ensemble Model Performance") +
  labs(x = "Algorithm", y = "Mean AUC")

# save to file
ggsave("outputs/ensemble_metrics.png", plot = ensemble_metrics)



# Plotting for entire study area:

# Informed Model:
informed_full_extent_cont <- ggplot() +
  geom_spatraster(data = informed_present_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Probability of Presence", na.value = "transparent") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Present Habitat Suitability", 
       subtitle = "Informed Model")

informed_full_extent_cont
ggsave("outputs/informed_full_extent_cont.png", plot = informed_full_extent_cont)


# Bioclim30s Present Model:
bioclim_pres_full_extent_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_present_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Probability of Presence", na.value = "transparent") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Present Habitat Suitability", 
       subtitle = "Bioclim Model")

bioclim_pres_full_extent_cont

ggsave("outputs/bioclim_pres_full_extent_cont.png", plot = bioclim_pres_full_extent_cont)


# Bioclim30s Future Model:
bioclim_fut_full_extent_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_future_continuous, aes(fill = mean)) +
  scale_fill_viridis_c(name = "Probability of Presence", na.value = "transparent") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Future Habitat Suitability", 
       subtitle = "Bioclim Model")

bioclim_fut_full_extent_cont

ggsave("outputs/bioclim_fut_full_extent_cont.png", plot = bioclim_fut_full_extent_cont)



# Plot continuous rasters together:
# create multilayer ggplot object?
full_extent_plots <- c(informed_full_extent_cont, 
                       bioclim_pres_full_extent_cont, 
                       bioclim_fut_full_extent_cont)
library(patchwork)
informed_full_extent_cont | bioclim_pres_full_extent_cont | bioclim_fut_full_extent_cont



# Calculate hillshade
slopes <- terra::terrain(elevation, "slope", unit = "radians")
aspect <- terra::terrain(elevation, "aspect", unit = "radians")
hillshade <- terra::shade(slopes, aspect)

# Plot hillshading as a basemap:
# Use Skeetchestn Territory as x and y limits:
terra::plot(hillshade, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
            xlim = st_bbox(skeetch_vectWGS84)[c(1,3)], ylim = st_bbox(skeetch_vectWGS84)[c(2,4)])

# create a grayscale colour palette
gray_palette <- hcl.colors(100, "Grays")
# overlay with elevation

terra::plot(elevation, col = gray_palette, alpha = 0.4, legend = FALSE,
            axes = FALSE, add = TRUE)
# add contour lines
terra::contour(elevation, col = "grey40", add = TRUE, nlevels = 15)


# Crop predictions to Skeetchestn Territory:
informed_present_skeetch <- crop(informed_present_continuous, skeetch_extent)
bioclim30s_present_skeetch <- crop(bioclim30s_present_continuous, skeetch_extent)
bioclim30s_future_skeetch <- crop(bioclim30s_future_continuous, skeetch_extent)

# turn Skeetchestn boundary vector from polygon into lines
skeetch_lines <- as.lines(skeetch_vectWGS84)

# Plot continuous prediction from informed model for Skeetchestn Territory:

skeetch_informed_cont <- ggplot() +
  geom_spatraster(data = informed_present_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Probability of Presence") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Present Habitat Suitability", 
       subtitle = "Informed Model")

skeetch_informed_cont

ggsave("outputs/skeetch_informed_cont.png", plot = skeetch_informed_cont)



# plot continuous prediction from bioclim30s present model for Skeetchestn:

skeetch_bioclim_present_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_present_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Probability of Presence") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Present Habitat Suitability", 
       subtitle = "Bioclim30s Model")

skeetch_bioclim_present_cont

ggsave("outputs/skeetch_bioclim_present_cont.png", plot = skeetch_bioclim_present_cont)


# plot continuous prediction from bioclim30s future model for Skeetchestn:

skeetch_bioclim_future_cont <- ggplot() +
  geom_spatraster(data = bioclim30s_future_skeetch, aes(fill = mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_viridis_c(name = "Probability of Presence") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Future Habitat Suitability", 
       subtitle = "Bioclim30s Model")

skeetch_bioclim_future_cont

ggsave("outputs/skeetch_bioclim_future_cont.png", plot = skeetch_bioclim_future_cont)




# Plot continuous rasters together:


# Full Study Extent:


# need to change layer names
names(informed_present_continuous) <- "informed_present"
names(bioclim30s_present_continuous) <- "bioclim_present"
names(bioclim30s_future_continuous) <- "bioclim_future"
continuous_predictions <- c(informed_present_continuous, 
                            bioclim30s_present_continuous,
                            bioclim30s_future_continuous)
continuous_predictions


predictions_continuous_plot <- ggplot() +
  geom_spatraster(data = binary_predictions) +
  facet_wrap(~lyr, nrow = 1, ncol = 3) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = NA, fill = NA)) +
  scale_fill_viridis_c(name = "Probability of \n Presence", na.value = "white") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(105, 110, 115, 120, 125, 130, 135),
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     # limits = c(-103.04, -137.07),
                     expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     #limits = c(50.3, 51.6),
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  theme_classic()



# Skeetchestn Territory:



# change names of our raster layers:
names(informed_present_skeetch) <- "informed_present"
names(bioclim30s_present_skeetch) <- "bioclim_present"
names(bioclim30s_future_skeetch) <- "bioclim_future"

# create a multilayer raster:
predictions_cont_skeetch <- c(informed_present_skeetch, 
                              bioclim30s_present_skeetch, 
                              bioclim30s_future_skeetch)

predictions_cont_skeetch_plot <- ggplot() +
  geom_spatraster(data = predictions_cont_skeetch) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  facet_wrap(~lyr, nrow = 1, ncol = 3) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = NA, fill = NA)) +
  scale_fill_viridis_c(name = "Probability of \n Presence", na.value = "white") +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  theme_classic()