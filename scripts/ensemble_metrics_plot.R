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
