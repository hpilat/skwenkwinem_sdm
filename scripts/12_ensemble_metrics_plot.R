# Author: Hannah Pilat, Jason Pither, David Ensing
# Date: April 12th, 2024

# This is script 12/12

# Here, we plot the model results for both our Informed and Bioclim models
# Please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.R
# 03_cropped_extent.R
# 05_data_processing.R
# 06_informed_model.R
# 07_bioclim30s_model.R


library(tidyverse)
library(tidyterra)



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
  add_column(model = "bioclim", .before = "algorithm")

# now bind the rows together into 1 object:
ensemble_AUC <- rbind(informed_ensemble_metrics_AUC, bioclim30s_ensemble_metrics_AUC)

# write to new csv to import into word
write.csv(ensemble_AUC, file = "outputs/skwenkwinem_ensemble_metrics.csv")

ensemble_AUC <- read.csv("outputs/skwenkwinem_ensemble_metrics.csv", header = TRUE)
ensemble_AUC

# plot ensemble metrics together
# plot ensemble metrics together
ensemble_metrics <- ggplot(ensemble_AUC, aes(x = algorithm, y = mean, colour = model)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), width= 0.15, linewidth = 0.75) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(y = "Mean AUC", x = "Algorithm") +
  theme_classic() +
  scale_x_discrete(labels = c("MaxEnt", "rf")) +
  scale_y_continuous(limits = c(0,1), 
                     expand = c(0,0)) +
  theme(axis.title.x = element_text(vjust = -1.0), 
        axis.title.y = element_text(hjust = -1.0), 
        legend.title = element_blank()) +
  theme(legend.position = "top")

ensemble_metrics

# save to file
ggsave("outputs/ensemble_metrics_new.png", plot = ensemble_metrics, height = 6, width = 4)
