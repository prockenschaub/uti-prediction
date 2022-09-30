
if(!exists(".initialised")){
  # Initialise the working environment
  .dir_root <- "analysis"
  source(file.path(.dir_root, "00_init.R"))
  
  # Which dataset to use
  rs_type <- "cv"
  rs_size <- "full"
  
  # Which predictor set to use (full/reduced)
  pred_set <- "reduced"
  preproc <- "yj"
  
  # What to run
  restart_from <- 1
  batch_size <- 7
  
  ctrl_experiment <- list(
    # Random seeds
    seed = 4012,
    
    # Variable scope
    scope = partial(set_scope, predictors = get_predictors(pred_set)),
    
    # Metrics calculated during tuning
    metrics = metric_set(roc_auc, pr_auc, sens, spec),
    
    # Controls passed on to ``tune``
    tune = control_grid(
      verbose = TRUE, 
      extract = extract_workflow,
      save_pred = FALSE,
      allow_par = TRUE,
      pkgs = "recipes"
    )
  )

  models <- "lr"
}


# Load resamples ----------------------------------------------------------

path <- file.path(.dir_rsmpl, str_c(rs_type, rs_size, str_c(pred_set, "_noind"), 
                                    "mi", preproc, sep = "_"))
rs <- read_rds(str_c(path, ".rds"))


# Add the missing indicators back in
data_ind <- load_ed() %>% 
  add_miss_ind() %>% 
  select(pat_id, idx_ed, idx_urine, miss_ua:miss_crp) %>% 
  as_tibble()

for (i in seq_along(rs$splits)) {
  rs$splits[[i]]$data <- inner_join(rs$splits[[i]]$data, data_ind, by = c("pat_id", "idx_ed", "idx_urine"))
}



# Set up parallel environment if specified --------------------------------

if(ctrl_experiment$tune$allow_par == TRUE){
  setup_parallel()
}



# Run the experiment ------------------------------------------------------

# NOTE: for computational reasons, this is only run for logistic regression
model <- list(
  engine = engine_log_reg,
  params = engine_log_reg %>% parameters(),
  prepro = pipe_noop # Already preprocessed before imputation
)

# Run analysis ----------------------------------------------------------

start <- Sys.time()
cat("\nStart fitting (", as.character(start) , ")")

res <- tune_model(rs, model, ctrl_experiment)

end <- Sys.time()
diff <- end - start
cat("\nFinish fitting ( in", diff, attr(diff, "units"), ")")

# Simply store the entire model -----------------------------------------
folder <- file.path(.dir_mods, rs_size, rs_type, 'mi', preproc)
file <- str_c("lr", pred_set, sep = "_")
write_rds(res, file.path(folder, str_c(file, ".rds")))


