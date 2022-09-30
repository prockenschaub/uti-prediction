
# Load all common project packages
source("00_init.R")

# Load the additional modelling packages
library(tidymodels)
library(parallel)
library(doParallel)

library(glmnet)
library(randomForest)
library(xgboost)
library(mfp)

# Set up the paths
.dir_root <- "analysis"
.dir_data <- "data"
.dir_rsmpl <- file.path(.dir_data, "resamples")

.dir_custom <- "src"

.dir_results <- "results"
.dir_univ <- file.path(.dir_results, "univariate")
.dir_mods <- file.path(.dir_results, "models")
.dir_best <- file.path(.dir_mods, "best")
.dir_perf <- file.path(.dir_results, "performance")
.dir_plot <- file.path(.dir_results, "plots")

# Indicate that everything is set up
.initialised <- TRUE

# Load all custom functions in this folder
source(file.path(.dir_custom, "load_data.R"))
source(file.path(.dir_custom, "pipelines.R"))
source(file.path(.dir_custom, "models.R"))
source(file.path(.dir_custom, "tune.R"))
source(file.path(.dir_custom, "multi_impute.R"))

