
# Initialise the working environment
.dir_root <- "99_manuscript"
source(file.path(.dir_root, "00_init.R"))

seed <- 2312

# What to run
restart_from <- 1
batch_size <- 7

# Which dataset to use
rs_size <- "full"
rs_type <- "cv"

# Which models to fit
models <- c("lr")

# Which metrics to consider and criteria to choose best
metrics <- metric_set(roc_auc, pr_auc, sens, spec)



# Define the settings to run ----------------------------------------------

runs <- tribble(
    ~preproc        , ~pred_set            ,
    c("none", "yj"), c("full", "reduced")     
  ) %>% 
  unnest(preproc) %>% 
  unnest(pred_set)




# Fit all specified settings ----------------------------------------------

for(i in 1:nrow(runs)){
  
  cat("\n\n")
  cat("****************************************************************\n")
  cat("Start run number", i, "\n")
  print(runs[i, ])
  
  pred_set <- runs[i, ]$pred_set
  preproc <- runs[i, ]$preproc
  
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
  
  for(d in c(rs_size, rs_type, "mi", preproc)){
    folder <- file.path(folder, d)
    if(!dir.exists(folder)){
      dir.create(folder)
    }
  }
  
  for(d in c("perf")){
    if(!dir.exists(file.path(folder, d))){
      dir.create(file.path(folder, d))
    }
  }
  
  remove(folder)
  
  # Run the calculations
  cat("\n")
  cat("****************************************************************\n")
  cat("Perform the model fitting\n")
  
  source(file.path(.dir_root, "03b_run_multi_impute.R"), print.eval = TRUE)
  
}



# Evaluate on external data -----------------------------------------------

path <- file.path(.dir_rsmpl, "train_test_full_noind_mi_yj.rds")
ext <- read_rds(path)


# Add the missing indicators back in
data_ind <- load_ed() %>% 
  add_miss_ind() %>% 
  select(pat_id, idx_ed, idx_urine, miss_ua:miss_crp) %>% 
  as_tibble()

for (i in seq_along(ext$splits)) {
  ext$splits[[i]]$data <- inner_join(
    ext$splits[[i]]$data, 
    data_ind, 
    by = c("pat_id", "idx_ed", "idx_urine")
  )
}

if(ctrl_experiment$tune$allow_par == TRUE){
  setup_parallel()
}

# NOTE: for computational reasons, this is only run for logistic regression
eng <- engine_log_reg
rec <- set_scope(ext$splits[[1]]$data[0, ], get_predictors("full")) %>% 
  pipe_noop()

res <- ext %>% 
  mutate(
    prepped = map(splits, ~ prep(rec, analysis(.))), 
    train = map(prepped, juice),
    test = map2(splits, prepped, ~ bake(.y, assessment(.x))), 
    fitted = map(train, ~ fit_xy(eng, select(., -growth), .$growth)), 
    preds = map2(
        fitted, 
        test, 
        ~ predict(.x, new_data = .y %>% select(-growth), type = "prob")
      ) %>% 
      map2(test, ~ bind_cols(.x, .y %>% select(growth)))
  ) %>% 
  select(id, preds) %>% 
  unnest(preds) %>% 
  group_by(id) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(c("row", "growth"), names_from = "id", values_from = ".pred_yes")


set.seed(55)
boot <- bootstraps(res, times = 1000)

estimate <- function(imp_preds){
  imp_preds %>% 
    summarise_at(
      str_c("Imputation", 1:5),
      ~ roc_auc_vec(growth, .)
    )
}

boot <- boot %>% 
  mutate(results = map(splits, ~ estimate(analysis(.))))

boot_sum <- boot %>% 
  mutate(
    row_mean = map_dbl(results, rowMeans),
    row_var = map_dbl(results, apply, MARGIN = 1, FUN = var)
  ) %>% 
  summarise(
    mean = mean(row_mean),
    var_b = var(row_mean),
    var_w = mean(row_var)
  )

with(boot_sum, mean + qnorm(c(0.025, 0.975)) * sqrt(var_b * (1 + 1/5) + var_w))
