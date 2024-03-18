# This is script 08/08
# This script plots area of agreement between our predictions for our total
# study area and Skeetchestn Territory
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

# Read in Binary Predictions:
informed_present_binary <- rast("outputs/skwenkwinem_informed_predict_present_binary.tif")
bioclim30s_present_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_present_binary.tif")
bioclim30s_future_binary <- rast("outputs/skwenkwinem_bioclim30s_predict_future_binary.tif")


# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp") # WGS84

# Skeetchestn territory boundary vector for masking:
skeetch_vect_albers <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject to WGS84:
skeetch_vectWGS84 <- project(skeetch_vect_albers, "EPSG:4326")


# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extentWGS84 <- ext(-121.6, -120.1, 50.3, 51.6) # xmin, xmax, ymin, ymax



# Full study extent:
# Informed
# Use binary prediction raster
plot(informed_present_binary)
summary(informed_present_binary)

# need to reclassify cells from presence to 1 and pseudoabsence to 0
# terra::classify requires us to provide a matrix of values from -> values to
# create a list so 1 (presence) stays as 1 and 2 (pseudoabsence) becomes 0
matrix_informed_cols <- c(1, 2, 1, 0)
matrix_informed <- matrix(matrix_informed_cols, ncol = 2)
matrix_informed

# reclassify values to 1 (presence) and 0 (pseudoabsence)
informed_classified <- terra::classify(informed_present_binary, matrix_informed)
plot(informed_classified)

writeRaster(informed_classified, filename = "outputs/informed_classified.tif", overwrite = TRUE)


# Area predicted to be suitable by the Bioclim (present) model:
# use binary prediction raster:
plot(bioclim30s_present_binary)

# reclassify cells, starting with creating matrix
# want values to go from 1 (presence) to 2 (distinguished from informed raster)
# and 2 (pseudoabsences) to 0 (same as in informed raster)
matrix_bioclim_cols <- c(1, 2, 2, 0)
matrix_bioclim <- matrix(matrix_bioclim_cols, ncol=2)
matrix_bioclim

# classify binary raster according to matrix
bioclim_classified <- classify(bioclim30s_present_binary, matrix_bioclim)
plot(bioclim_classified)


writeRaster(bioclim_classified, filename = "outputs/bioclim_present_classified.tif", overwrite = TRUE)


# Model Agreement:

# add informed_classified and bioclim_classified together to get model_agreement:
model_agreement <- (informed_classified + bioclim_classified)
plot(model_agreement)
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

# save to file
writeRaster(model_agreement, filename = "outputs/model_agreement.tif", overwrite = TRUE)


# Plotting:


# crop to study area:
model_agreement <- crop(model_agreement, na_bound_vect)
model_agreement <- mask(model_agreement, na_bound_vect)
# need to convert binary_mean from numeric to factor
summary(model_agreement)
model_agreement <- as.factor(model_agreement)
summary(model_agreement)
plot(model_agreement)
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence

agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement, aes(fill = binary_mean)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "informed", "bioclim", "both"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725", "#95D054")) +
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
  labs(title = "Area of Agreement", 
       subtitle = "Informed and Bioclim30s Models (Present)")

agreement_present

ggsave("outputs/agreement_present.png", plot = agreement_present)



# Plot area of agreement in Skeetchestn Territory:


# crop model_agreement to Skeetchestn extent:
model_agreement_skeetch <- crop(model_agreement, skeetch_extentWGS84)
plot(model_agreement_skeetch)

# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:
# turn Skeetchestn boundary polygon into lines geometry:
skeetch_lines <- as.lines(skeetch_vectWGS84)

# convert raster from numeric to factor
model_agreement_skeetch <- as.factor(model_agreement_skeetch)

model_agreement_skeetch
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence


skeetch_agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "informed", "bioclim present", "overlap"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725", "#95D054")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Informed and Bioclim30s Models (Present)")

skeetch_agreement_present

ggsave("outputs/skeetch_agreement_present.png", plot = skeetch_agreement_present)



# Bioclim30s Future:


# use binary prediction raster:
plot(bioclim30s_future_binary)

# reclassify cells, starting with creating matrix
# want values to go from 1 (presence) to 2 (distinguished from informed raster)
# and 2 (pseudoabsences) to 0 (same as in informed raster)
matrix_bioclim_fut_cols <- c(1, 2, 4, 0)
matrix_bioclim_fut <- matrix(matrix_bioclim_fut_cols, ncol=2)
matrix_bioclim_fut

# classify binary raster according to matrix
bioclim_classified_fut <- classify(bioclim30s_future_binary, matrix_bioclim_fut)
plot(bioclim_classified_fut)

writeRaster(bioclim_classified_fut, filename = "outputs/bioclim_classified_fut.tif", overwrite = TRUE)


# add bioclim_classified_fut and bioclim_classified together to get model_agreement:
model_agreement_fut <- (bioclim_classified + bioclim_classified_fut)
plot(model_agreement_fut)
# 0 = pseudoabsence
# 2 = bioclim30s present prediction of presence
# 4 = bioclim30s future prediction of presence
# 6 = agreement between both bioclim30s present and future predicted presence

# save to file
writeRaster(model_agreement_fut, filename = "outputs/model_agreement_future.tif", overwrite = TRUE)



# Plotting:



# need to convert binary_mean from numeric to factor
summary(model_agreement_fut)
model_agreement_fut <- as.factor(model_agreement_fut)
summary(model_agreement_fut)
plot(model_agreement_fut)
# now: 
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions

agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut, aes(fill = binary_mean)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "bioclim present", "bioclim future", "overlap"), 
                    values = c("grey", "#FDE725", "#F8870E", "#C73E4C")) +
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
  labs(title = "Area of Agreement", 
       subtitle = "Bioclim30s Present and Future Models")

agreement_future
ggsave("outputs/agreement_future.png", plot = agreement_future)




# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:


model_agreement_fut_skeetch <- crop(model_agreement_fut, skeetch_extentWGS84)
plot(model_agreement_fut_skeetch)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions
summary(model_agreement_fut_skeetch)

# need to convert binary_mean from numeric to factor
model_agreement_fut_skeetch <- as.factor(model_agreement_fut_skeetch)
summary(model_agreement_fut_skeetch)

plot(model_agreement_fut_skeetch)
# 0 = pseudoabsence
# 2 = bioclim present prediction
# 4 = bioclim future prediction
# 6 = agreement between both bioclim present and future predictions


skeetch_agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_fut_skeetch, aes(fill = binary_mean)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("pseudoabsence", "bioclim present", "bioclim future", "overlap"), 
                    values = c("grey", "#FDE725", "#F8870E", "#C73E4C")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     # limits = c(120.2, 121.6),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Bioclim30s Present and Future Models")

skeetch_agreement_future
ggsave("outputs/skeetch_agreement_future.png", plot = skeetch_agreement_future)



# Plot the area of agreement plots together:

# Full study extent:

model_agreement
model_agreement_fut

# create temp rasters so we don't overwrite the originals:
model_agreement_temp <- model_agreement
model_agreement_fut_temp <- model_agreement_fut

# change the names of our rasters:
names(model_agreement_temp) <- "Informed and Bioclim Present"
names(model_agreement_fut_temp) <- "Bioclim Present and Future"

# create a multilayer raster object:
agreement_full_extent <- c(model_agreement_temp, model_agreement_fut_temp)

# convert raster from numeric to factor
agreement_full_extent <- as.factor(agreement_full_extent)

agreement_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_full_extent) +
  facet_wrap(~lyr, nrow = 1, ncol = 2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(name = NULL, na.translate = FALSE, 
                    labels = c("pseudoabsence", "informed", "bioclim present", "informed & bioclim", "bioclim future", "bioclim present & future"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725", "#95D054", "#F8870E", "#C73E4C")) +
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
                     expand = c(0, 0))

agreement_facet_plot
ggsave("outputs/agreement_full_extent_faceted.png", agreement_facet_plot, 
       width = 12, height = 4, units = "in")



# Skeetchestn Territory:

# Full study extent:

model_agreement_skeetch
model_agreement_fut_skeetch

# create temp rasters so we don't overwrite the originals:
model_agreement_skeetch_temp <- model_agreement_skeetch
model_agreement_fut_skeetch_temp <- model_agreement_fut_skeetch

# change the names of our rasters:
names(model_agreement_skeetch_temp) <- "Informed and Bioclim Present"
names(model_agreement_fut_skeetch_temp) <- "Bioclim Present and Future"

# create a multilayer raster object:
agreement_skeetch <- c(model_agreement_skeetch_temp, model_agreement_fut_skeetch_temp)

# convert raster from numeric to factor
agreement_skeetch <- as.factor(agreement_skeetch)

agreement_skeetch_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_skeetch) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white") +
  facet_wrap(~lyr, nrow = 1, ncol = 2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(name = NULL, na.translate = FALSE, 
                    labels = c("pseudoabsence", "informed", "bioclim present", "informed & bioclim", "bioclim future", "bioclim present & future"), 
                    values = c("grey", "#2A7B8EFF", "#FDE725", "#95D054", "#F8870E", "#C73E4C")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     # limits = c(120.2, 121.6),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     expand = c(0, 0))

agreement_skeetch_facet_plot

ggsave("outputs/agreement_skeetch_faceted.png", agreement_skeetch_facet_plot, 
       width = 12, height = 4, units = "in")
