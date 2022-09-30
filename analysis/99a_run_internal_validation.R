
# Initialise the working environment
.dir_root <- "analysis"
source(file.path(.dir_root, "00_init.R"))

seed <- 2312

# Which dataset to use
rs_size <- "full"
main_groups <- "full"

# Which models to fit
all_models <- c("lr", "fp", "net", "xgb", "rf")
all_pre <- c("none", "log", "yj")

n_par_comb <- 30L
restart_from <- 1
batch_size <- 7

# Which metrics to consider and criteria to choose best
metrics <- metric_set(roc_auc, pr_auc, sens, spec)
criteria <- "roc_auc"
target_sensitivity <- 0.95



# Define the settings to run ----------------------------------------------

runs <- tribble(
  ~rs_type, ~imp_meth, ~preproc, ~pred_set           , ~models   ,
  "cv"    , "mean"   , all_pre , c("full", "reduced"), all_models,
  "cv"    , "knn"    , "log"   , c("full", "reduced"), all_models,
  "cv"    , "kmeans" , "log"   , c("full", "reduced"), all_models[all_models != "fp"],
  "bs"    , "mean"   , "log"   , c("full", "reduced"), all_models
) %>% 
  unnest(preproc) %>% 
  unnest(pred_set)



# Fit all specified settings ----------------------------------------------

for(i in 1:nrow(runs)){
  
  cat("\n\n")
  cat("****************************************************************\n")
  cat("Start run number", i, "\n")
  print(runs[i, ])
  
  rs_type <- runs[i, ]$rs_type
  pred_set <- runs[i, ]$pred_set
  preproc <- runs[i, ]$preproc
  imp_meth <- runs[i, ]$imp_meth
  models <- runs[i, ]$models[[1]]
  
  # Other parameters for the tuning process
  ctrl_experiment <- list(
    # Random seeds
    seed = 2312,
    
    # Variable scope
    scope = partial(set_scope, predictors = get_predictors(pred_set)),
    
    # Metrics calculated during tuning
    metrics = metrics,
    
    # Controls passed on to ``tune``
    tune = control_grid(
      verbose = TRUE, 
      extract = extract_workflow,
      save_pred = FALSE,
      allow_par = TRUE,
      pkgs = "recipes"
    )
  )
  
  # Create the folder structure
  folder <- .dir_mods
  
  for(d in c(rs_size, rs_type, imp_meth, preproc)){
    folder <- file.path(folder, d)
    if(!dir.exists(folder)){
      dir.create(folder)
    }
  }
  
  for(d in c("perf", "best")){
    if(!dir.exists(file.path(folder, d))){
      dir.create(file.path(folder, d))
    }
  }
  
  if(!dir.exists(file.path(.dir_perf, "01_discrimination", rs_size))){
    dir.create(file.path(.dir_perf, "01_discrimination", rs_size))
  }
  
  if(!dir.exists(file.path(.dir_perf, "02_calibration", rs_size))){
    dir.create(file.path(.dir_perf, "02_calibration", rs_size))
  }
  
  if(!dir.exists(file.path(.dir_plot, rs_size))){
    dir.create(file.path(.dir_plot, rs_size))
  }
  
  remove(folder)
  
  # Run the calculations
  cat("\n")
  cat("****************************************************************\n")
  cat("Perform the model fitting\n")
  
  source(file.path(.dir_root, "03a_run_single_impute.R"), print.eval = TRUE)
  
  
  cat("\n")
  cat("****************************************************************\n")
  cat("Evaluate models and choose best\n")
  
  source(file.path(.dir_root, "04a_define_performance.R"))
  
  if(rs_type == "bs"){
    # Add apparent performance for optimism-adjusted bootstrap
    source(file.path(.dir_root, "04b_calculate_bs_performance.R"))
  }
  
  source(file.path(.dir_root, "04c_select_best_models.R"), print.eval = TRUE)
  
  if(rs_type == "cv" && 
     imp_meth == "mean" && 
     preproc == "yj" && 
     pred_set == "reduced"){
    # Run these extra steps only for the published model to save time
    source(file.path(.dir_root, "04d_discrimination.R"), print.eval = TRUE)
    source(file.path(.dir_root, "04e_calibration.R"), print.eval = TRUE)
  }
}


