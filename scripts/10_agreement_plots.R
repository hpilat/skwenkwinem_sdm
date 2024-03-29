# This is script 10/10
# This script plots area of agreement between our predictions for our total
# study area and Skeetchestn Territory
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R
# 08_area_calculations_full_extent.R
# 09_area_calculations_skeetch.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)


# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp") # WGS84

# Skeetchestn territory boundary vector for masking:
skeetch_vect_albers <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject to WGS84:
skeetch_vectWGS84 <- terra::project(skeetch_vect_albers, "EPSG:4326")


# create an extent object slightly larger than skeetch_vectWGS84
skeetch_vectWGS84 # round up extent values:
skeetch_extentWGS84 <- ext(-121.6, -120.1, 50.3, 51.6) # xmin, xmax, ymin, ymax


# Read in agreement maps for total study extent:
model_agreement_pres <- rast("outputs/agreement_informed_bioclim.tif")
model_agreement_fut <- rast("outputs/agreement_bioclim_fut_pres.tif")

# convert to factor so we can alter the scale manually in ggplot:
model_agreement_pres <- as.factor(model_agreement_pres)
model_agreement_fut <- as.factor(model_agreement_fut)

# reproject to WGS84:
model_agreement_pres <- terra::project(model_agreement_pres, "EPSG:4326", method = "near")
model_agreement_fut <- terra::project(model_agreement_fut, "EPSG:4326", method = "near")

# crop to study extent:
model_agreement_pres <- crop(model_agreement_pres, na_bound_vect)
plot(model_agreement_pres)
model_agreement_fut <- crop(model_agreement_fut, na_bound_vect)

model_agreement_pres
model_agreement_fut



# Plotting:


# Informed and Bioclim Present:
summary(model_agreement_pres)
# want to set 0 from lyr.1 to NA so they can be ignored in ggplot:
model_agreement_presNA <- terra::subst(model_agreement_pres, from = 0, to = NA)
summary(model_agreement_presNA)
plot(model_agreement_presNA)
# 0 = NA
# 1 = bioclim prediction of presence
# 2 = both bioclim and informed prediction of presence
# 3 = informed prediction of presence
# 4 = pseudoabsence

agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement_presNA, aes(fill = lyr.1)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("bioclim present", "overlap", "informed present", "pseudoabsence"), 
                    values = c("#FDE725", "#95D054", "#2A7B8EFF", "grey")) +
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
       subtitle = "Informed and Bioclim30s Models (Present)") +
  theme_classic()

agreement_present

ggsave("outputs/agreement_present.png", plot = agreement_present)



# Plot area of agreement in Skeetchestn Territory:


# crop model_agreement to Skeetchestn extent:
agreement_pres_skeetch <- crop(model_agreement_presNA, skeetch_extentWGS84)
summary(agreement_pres_skeetch)
plot(agreement_pres_skeetch)

# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:
# turn Skeetchestn boundary polygon into lines geometry:
skeetch_lines <- as.lines(skeetch_vectWGS84)

agreement_pres_skeetch
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = bioclim30s prediction of presence
# 3 = agreement between both informed and bioclim30s predicted presence


skeetch_agreement_present <- ggplot() +
  geom_spatraster(data = agreement_pres_skeetch, aes(fill = lyr.1)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("bioclim present", "overlap", "informed present", "pseudoabsence"), 
                    values = c("#FDE725", "#95D054", "#2A7B8EFF", "grey")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
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

# Plotting:



summary(model_agreement_fut)
# want to set 0 from lyr.1 to NA so they can be ignored in ggplot:
model_agreement_futNA <- terra::subst(model_agreement_fut, from = 0, to = NA)
summary(model_agreement_futNA)
plot(model_agreement_futNA)

# 1 = bioclim present prediction
# 2 = pseudoabsence
# 3 = agreement between both bioclim present and future predictions
# 4 = bioclim future prediction


agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_futNA, aes(fill = lyr.1)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("bioclim present", "overlap", "bioclim future", "pseudoabsence"), 
                    values = c("#FDE725", "#F8870E", "#C73E4C", "grey")) +
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
       subtitle = "Bioclim30s Present and Future Models") + 
  theme_classic()

agreement_future
ggsave("outputs/agreement_future.png", plot = agreement_future)



# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:



agreement_fut_skeetch <- crop(model_agreement_futNA, skeetch_extentWGS84)
summary(agreement_fut_skeetch)
plot(agreement_fut_skeetch)

# 0 = pseudoabsence
# 2 = bioclim30s present prediction of presence
# 4 = bioclim30s future prediction of presence
# 6 = agreement between both bioclim30s present and future predicted presence

skeetch_agreement_future <- ggplot() +
  geom_spatraster(data = agreement_fut_skeetch, aes(fill = lyr.1)) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("bioclim present", "overlap", "bioclim future", "pseudoabsence"), 
                    values = c("#FDE725", "#F8870E", "#C73E4C", "grey")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
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

model_agreement_presNA
levels(model_agreement_presNA)
model_agreement_futNA
levels(model_agreement_futNA)

# create temp rasters so we don't overwrite the originals:
model_agreement_pres_temp <- model_agreement_presNA
model_agreement_fut_temp <- model_agreement_futNA

# change the names of our rasters:
names(model_agreement_pres_temp) <- "Informed and Bioclim Present"
names(model_agreement_fut_temp) <- "Bioclim Present and Future"

# create a multilayer raster object:
agreement_full_extent <- c(model_agreement_pres_temp, model_agreement_fut_temp)
levels(agreement_full_extent)


agreement_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_full_extent) +
  facet_wrap(~lyr, nrow = 1, ncol = 2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("bioclim present", "informed & bioclim present", "informed present", "pseudoabsence", "bioclim present & future", "bioclim future"), 
                    values = c("#FDE725", "#95D054","#2A7B8EFF", "grey", "#F8870E", "#C73E4C")) +
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

agreement_pres_skeetch
agreement_fut_skeetch

# create temp rasters so we don't overwrite the originals:
agreement_skeetch_temp <- agreement_pres_skeetch
agreement_fut_skeetch_temp <- agreement_fut_skeetch

# change the names of our rasters:
names(agreement_skeetch_temp) <- "Informed and Bioclim Present"
names(agreement_fut_skeetch_temp) <- "Bioclim Present and Future"

# create a multilayer raster object:
agreement_skeetch <- c(agreement_skeetch_temp, agreement_fut_skeetch_temp)

agreement_skeetch_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_skeetch) +
  geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  facet_wrap(~lyr, nrow = 1, ncol = 2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(name = NULL, na.translate = FALSE, 
                    labels = c("bioclim present", "informed & bioclim present", "informed", "pseudoabsence", "bioclim present & future", "bioclim future"), 
                    values = c("#FDE725", "#95D054","#2A7B8EFF", "grey", "#F8870E", "#C73E4C")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
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
