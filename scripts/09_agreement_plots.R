# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 10/11
# This script plots area of agreement between our predictions for our total
# study area and Skeetchestn Territory
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_tidysdm_ranunculus_informed.R
# 06_tidysdm_ranunculus_bioclim30s.R
# 08a_area_calculations_full_extent.R
# 08b_area_calculations_skeetch.R


library(tidyverse)
library(tidyterra)
library(terra)
library(sf)



# total study area boundary:
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp") # WGS84

# reproject to North America Albers equal-area conic
# https://spatialreference.org/ref/esri/102008/
# define CRS
new_crs <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
na_bound_vect <- terra::project(na_bound_vect, new_crs)

# Skeetchestn territory boundary vector for masking:
#skeetch_vect <- vect("data/extents/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject:
#skeetch_vect <- terra::project(skeetch_vect, new_crs)
# turn Skeetchestn boundary polygon into lines geometry:
#skeetch_lines <- as.lines(skeetch_vect)

# create an extent object slightly larger than skeetch_vect
#skeetch_vect # round up extent values:
#skeetch_extent <- ext(-121.6, -120.1, 50.3, 51.6)

# create a dataframe containing a coordinate for the Skeetchestn band office
# 50.83951982786047, -120.95445365748702
skeetch_coord <- data.frame(lat = 50.83951982786047, lon = -120.95445365748702)
skeetch_coord_vect <- terra::vect(skeetch_coord, crs = "EPSG:4326")

# Read in agreement maps for total study extent:
model_agreement_pres <- rast("outputs/agreement_informed_bioclim.tif")
model_agreement_fut <- rast("outputs/agreement_bioclim_fut_pres_585.tif")

# convert to factor so we can alter the scale manually in ggplot:
model_agreement_pres <- as.factor(model_agreement_pres)
model_agreement_fut <- as.factor(model_agreement_fut)

# reproject to new CRS:
model_agreement_pres <- terra::project(model_agreement_pres, new_crs, method = "near")
model_agreement_fut <- terra::project(model_agreement_fut, new_crs, method = "near")

# crop to study extent:
model_agreement_pres <- crop(model_agreement_pres, na_bound_vect)
plot(model_agreement_pres)
model_agreement_fut <- crop(model_agreement_fut, na_bound_vect)

model_agreement_pres
model_agreement_fut



# Plotting:


# Informed and WorldClim Present:
summary(model_agreement_pres)
# want to set 0 from lyr.1 to NA so they can be ignored in ggplot:
model_agreement_presNA <- terra::subst(model_agreement_pres, from = 0, to = NA)
summary(model_agreement_presNA)
plot(model_agreement_presNA)
# 0 = NA
# 1 = WorldClim prediction of presence
# 2 = both WorldClim and informed prediction of presence
# 3 = informed prediction of presence
# 4 = pseudoabsence

agreement_present <- ggplot() +
  geom_spatraster(data = model_agreement_presNA, aes(fill = lyr.1)) +
  geom_spatvector(data = skeetch_coord_vect, color = "white", size = 1.75, shape = 17) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("WorldClim present", "Overlap", "Informed present", "Pseudoabsence"), 
                    values = c("#FDE725", "#95D054", "#2A7B8EFF", "grey")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     labels = c("135", "130", "125", "120", "115", "110", "105"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     labels = c("30", "35", "40", "45", "50", "55", "60"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "Informed and WorldClim Models (Present)") +
  theme_classic()

agreement_present

ggsave("outputs/agreement_present.png", plot = agreement_present)



# Plot area of agreement in Skeetchestn Territory:


# use UTM projection:
#agreement_present_UTM <- terra::project(model_agreement_presNA, "EPSG:32610")
#skeetch_extent_UTM <- terra::project(skeetch_extent, from = "EPSG:4326", to = "EPSG:32610")
#skeetch_lines_UTM <- terra::project(skeetch_lines, "EPSG:32610")

# Crop predictions to Skeetchestn Territory:
#agreement_present_skeetch <- crop(agreement_present_UTM, skeetch_extent_UTM)

#summary(agreement_present_skeetch)
#plot(agreement_present_skeetch)


#agreement_present_skeetch
# 0 = pseudoabsence
# 1 = informed prediction of presence
# 2 = WorldClim30s prediction of presence
# 3 = agreement between both informed and WorldClim30s predicted presence


#skeetch_agreement_present <- ggplot() +
  #geom_spatraster(data = agreement_present_skeetch, aes(fill = lyr.1)) +
  #geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  #theme_classic() +
  #scale_fill_manual(name = NULL, na.translate = FALSE,
                    #labels = c("WorldClim present", "overlap", "informed present", "pseudoabsence"), 
                    #values = c("#FDE725", "#95D054", "#2A7B8EFF", "grey")) +
  #scale_x_continuous(name = "Longitude (°W)",
                     #labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                     #expand = c(0,0)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #scale_y_continuous(name = "Latitude (°N)",
                     #labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                     #expand = c(0, 0)) +
  #labs(title = "Area of Agreement", 
       #subtitle = "Informed and WorldClim Models (Present)")

#skeetch_agreement_present

#ggsave("outputs/skeetch_agreement_present.png", plot = skeetch_agreement_present)



# WorldClim Future:

# Plotting:



summary(model_agreement_fut)
# want to set 0 from lyr.1 to NA so they can be ignored in ggplot:
model_agreement_futNA <- terra::subst(model_agreement_fut, from = 0, to = NA)
summary(model_agreement_futNA)
plot(model_agreement_futNA)

# 1 = WorldClim present prediction
# 2 = pseudoabsence
# 3 = agreement between both WorldClim present and future predictions
# 4 = WorldClim future prediction


agreement_future <- ggplot() +
  geom_spatraster(data = model_agreement_futNA, aes(fill = lyr.1)) +
  geom_spatvector(data = skeetch_coord_vect, color = "white", size = 1.75, shape = 17) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("WorldClim present", "Overlap", "WorldClim future", "Pseudoabsence"), 
                    values = c("#FDE725", "#F8870E", "#C73E4C", "grey")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     labels = c("135", "130", "125", "120", "115", "110", "105"), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  labs(title = "Area of Agreement", 
       subtitle = "WorldClim Present and Future Models") + 
  theme_classic()

agreement_future

ggsave("outputs/agreement_future_585.png", plot = agreement_future)



# plot area of agreement not masked to Skeetch with Skeetch polygon overlaid:



# use UTM projection:
#agreement_future_UTM <- terra::project(model_agreement_futNA, "EPSG:32610")

# Crop predictions to Skeetchestn Territory:
#agreement_future_skeetch <- crop(agreement_future_UTM, skeetch_extent_UTM)

#summary(agreement_future_skeetch)
#plot(agreement_future_skeetch)

# 0 = pseudoabsence
# 2 = WorldClim30s present prediction of presence
# 4 = WorldClim30s future prediction of presence
# 6 = agreement between both bioclim30s present and future predicted presence

#skeetch_agreement_future <- ggplot() +
  #geom_spatraster(data = agreement_future_skeetch, aes(fill = lyr.1)) +
  #geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  #theme_classic() +
  #scale_fill_manual(name = NULL, na.translate = FALSE,
                    #labels = c("WorldClim present", "overlap", "WorldClim future", "pseudoabsence"), 
                    #values = c("#FDE725", "#F8870E", "#C73E4C", "grey")) +
  #scale_x_continuous(name = "Longitude (°W)",
                    #labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"),
                    #expand = c(0,0)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #scale_y_continuous(name = "Latitude (°N)",
                    #labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4", "51.6"), 
                    #expand = c(0, 0)) +
  #labs(title = "Area of Agreement", 
       #subtitle = "WorldClim30s Present and Future Models")
  

#skeetch_agreement_future

#ggsave("outputs/skeetch_agreement_future_585.png", plot = skeetch_agreement_future)



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
names(model_agreement_pres_temp) <- "Informed and WorldClim present"
names(model_agreement_fut_temp) <- "WorldClim present and future"

# create a multilayer raster object:
agreement_full_extent <- c(model_agreement_pres_temp, model_agreement_fut_temp)
levels(agreement_full_extent)

agreement_facet_plot <- ggplot() +
  geom_spatraster(data = agreement_full_extent) +
  geom_spatvector(data = skeetch_coord_vect, color = "white", size = 1.75, shape = 17) +
  facet_wrap(~lyr, nrow = 1, ncol = 2, labeller = label_wrap_gen(width = 18)) +
  theme(axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        panel.border = element_blank(), 
        strip.text = element_text(size = 10)) +
  scale_fill_manual(name = NULL, na.translate = FALSE,
                    labels = c("Bioclim present", "Informed & Bioclim present", "Informed present", "Bioclim present & future", "Bioclim future", "Unsuitable"), 
                    values = c("#FDE725", "#95D054","#2A7B8EFF", "#F8870E", "#C73E4C", "grey")) +
  scale_x_continuous(name = "Longitude (°W)", 
                     labels = c("135", "130", "125", "120", "115", "110", "105"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "Latitude (°N)",
                     breaks = c(35, 40, 45, 50, 55),
                     labels = c("35", "40", "45", "50", "55"), 
                     expand = c(0, 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(panel.spacing = unit(25, "pt"))

agreement_facet_plot

ggsave("outputs/agreement_full_extent_faceted_585.png", agreement_facet_plot, 
       width = 8, height = 4, units = "in")



# Skeetchestn Territory:

#agreement_present_skeetch
#agreement_future_skeetch

# create temp rasters so we don't overwrite the originals:
#agreement_skeetch_temp <- agreement_present_skeetch
#agreement_fut_skeetch_temp <- agreement_future_skeetch

# change the names of our rasters:
#names(agreement_skeetch_temp) <- "Informed and WorldClim present"
#names(agreement_fut_skeetch_temp) <- "WorldClim present and future"

# create a multilayer raster object:
#agreement_skeetch <- c(agreement_skeetch_temp, agreement_fut_skeetch_temp)

#agreement_skeetch_facet_plot <- ggplot() +
  #geom_spatraster(data = agreement_skeetch) +
  #geom_spatvector(data = skeetch_lines, aes(fill = NULL), colour = "white", show.legend = FALSE) +
  #facet_wrap(~lyr, nrow = 1, ncol = 2) +
  #theme_classic() +
  #theme(axis.line = element_line(colour = "black"),
        #strip.background = element_blank(),
        #panel.border = element_blank()) +
  #scale_fill_manual(name = NULL, na.translate = FALSE,
                    #labels = c("WorldClim present", "Informed & WorldClim present", "Informed present", "Unsuitable", "WorldClim present & future", "WorldClim future"), 
                    #values = c("#FDE725", "#95D054","#2A7B8EFF", "grey", "#F8870E", "#C73E4C")) +
  #scale_x_continuous(name = "Longitude (°W)", 
                     # breaks = c(120.2, 120.4, 120.6, 120.8, 121.0, 121.2, 121.4),
                     #labels = c("121.6", "121.4", "121.2", "121.0", "120.8", "120.6", "120.4", "120.2"), 
                     # limits = c(120.2, 121.6),
                     #expand = c(0,0)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #scale_y_continuous(name = "Latitude (°N)",
                     #breaks = c(50.4, 50.6, 50.8, 51.0, 51.2, 51.4),
                     #labels = c("50.4", "50.6", "50.8", "51.0", "51.2", "51.4"), 
                     #expand = c(0, 0))

#agreement_skeetch_facet_plot

#ggsave("outputs/agreement_skeetch_faceted_585.png", agreement_skeetch_facet_plot, 
       #width = 12, height = 4, units = "in")
