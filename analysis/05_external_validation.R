
if(!exists(".initialised")){
  # Initialise the working environment
  .dir_root <- "99_manuscript"
  source(file.path(.dir_root, "00_init.R"))
  source(file.path(.dir_root, "04a_define_performance.R"))
  
  # Which dataset to use
  rs_type <- "cv"
  rs_size <- "full"
  
  # Which models to process
  models <- c("lr", "fp", "net", "xgb", "rf")
  preproc <- "yj"
  
  # Which imputation method
  imp_meth <- "mean"
  
  # Which metrics to compare and what criteria to use for the best model
  metrics <- metric_set(roc_auc, pr_auc, sens, spec)
}



# Load the models ---------------------------------------------------------

folder <- file.path(.dir_mods, rs_size, rs_type, imp_meth, preproc)
pattern <- str_c("(", str_c(models, collapse = "|"), ")_(full|reduced).rds")

model_files <- file.path(folder, "best") %>% 
  list.files(pattern)

model_nms <- model_files %>% str_replace_all(".rds", "")

model_objs <- model_files %>% 
  set_names(model_nms) %>% 
  map(~file.path(folder, "perf", .)) %>% 
  map(read_rds)



# Get the best hyperparameter combination ---------------------------------

best_params <- model_objs %>% 
  map(show_best, "roc_auc", 1) %>% 
  map(~ select(., -(.metric:std_err))) %>% 
  map(distinct)


# Load the training/test split --------------------------------------------

train <- analysis(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
cal <- assessment(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
test <- assessment(read_rds(file.path(.dir_rsmpl, "train_test.rds")))


# Train the final models --------------------------------------------------

engines <- list(
  lr = engine_log_reg,
  fp = engine_fp,
  net = engine_glmnet,
  xgb = engine_xgb,
  rf = engine_rf
)

pipe_trnsfm <- switch(preproc,
                       log = pipe_log, 
                       bc = pipe_boxcox,
                       yj = pipe_yeojohnson,
                       pipe_noop)

formu <- list(pipe_rm_unused, pipe_winsor, pipe_trnsfm, pipe_meanimpute)
one_hot <- c(formu, list(pipe_one_hot))

preprocs <- list(
  lr = formu, 
  fp = formu,
  net = one_hot,
  xgb = one_hot, 
  rf = one_hot
)

set_best_args <- function(clss, nm){
  if(ncol(best_params[[nm]]) == 0){
    engines[[clss]]
  } else {
    set_args(engines[[clss]], !!!as.list(best_params[[nm]]))
  }
}

mclass <- str_extract(model_nms, str_c(models, collapse = "|"))
pset <- str_extract(model_nms, "full|reduced")

set.seed(33)
eng <- map2(mclass, model_nms, set_best_args)
rec <- pset %>% 
  map(get_predictors) %>% 
  map(set_scope, data = train) %>% 
  map2(mclass, ~ pipe_combine(.x, !!!preprocs[[.y]])) %>% 
  map(step_rm, recipes::has_role("id var"))
prepped <- map(rec, prep, train)
juiced <- map(prepped, juice)
fits <- map2(eng, juiced, ~ fit_xy(.x, .y %>% select(-growth), .y$growth))
preds <- fits %>% 
  map2(
    prepped, 
    ~ predict(
        .x, 
        new_data = bake(.y, test) %>% 
          select(-growth), 
        type = "prob"
  )) %>% 
  map(bind_cols, test[, .(growth)])


names(fits) <- model_nms
names(preds) <- model_nms

write_rds(fits, file.path(.dir_mods, "external", preproc, "all_models.rds"))
write_rds(preds, file.path(.dir_mods, "external", preproc, "all_preds.rds"))



# Evaluate the final discrimination metrics -------------------------------

bootstrap_performance <- function(preds, n_boot = 10, seed = 42){
  # Bootstrap confidence intervals for external performance
  #
  # Note: this is different from bootstrap for internal validation
  
  estimate <- function(pred){
    thrsh <- get_threshold(pred, 0.95)
    
    bin <- expr(factor(.pred_yes > thrsh, c(TRUE, FALSE), c("yes", "no")))
    pred %<>% mutate(class = eval(bin))
    
    bind_rows(
      roc_auc(pred, growth, .pred_yes),
      pr_auc(pred, growth, .pred_yes),
      spec(pred, growth, class),
      npv(pred, growth, class)
    ) %>% select(-.estimator)
  }
  
  set.seed(seed)
  boot <- bootstraps(preds, times = n_boot)
  
  boot %>% 
    mutate(results = map(splits, ~ estimate(analysis(.))))
}

extract_peformance <- function(bs) {
  bs %>% 
    select(results) %>% 
    unnest(results) %>% 
    group_by(.metric) %>% 
    summarise(
      conf.low = quantile(.estimate, 0.025),
      conf.high = quantile(.estimate, 0.975),
      .estimate = mean(.estimate)
    ) %>% 
    mutate_at(
      vars(one_of(".estimate", "conf.low", "conf.high")),
      ~ str_pad(round(., digits = 3), 5, "right", "0")
    ) %>% 
    mutate(ci = str_c(.estimate, " (", conf.low, "--", conf.high, ")")) %>% 
    select(-.estimate, -conf.low, -conf.high) %>% 
    pivot_wider(names_from = ".metric", values_from = "ci") %>% 
    select(roc_auc, pr_auc, spec, npv)
}


discr_bs <- map(preds, bootstrap_performance, n_boot = 1000, seed = 55)

discr_perf <- discr_bs %>% 
  map_dfr(extract_peformance, .id = "model")

write_rds(
  discr_perf, 
  file.path(.dir_perf, "01_discrimination", "external", preproc, "all_perf.rds")
)

# Test for differences across models
roc_bs <- discr_bs %>% 
  map(unnest, results) %>% 
  map(select, id, .metric, .estimate) %>% 
  map_dfr(filter, .metric == "roc_auc", .id = "model") %>% 
  pivot_wider(id_cols = "id", names_from = "model", values_from = ".estimate")

p_mat <- diag(1, length(model_nms))
rownames(p_mat) <- colnames(p_mat) <- model_nms

for (i in model_nms) {
  for (j in model_nms) {
    if (i != j) {
      p_mat[i, j] <- with(roc_bs, mean(get(i) < get(j)))
    }
  }
}


# Only the first
firsts <- test[, .(idx_ed, n = 1:.N), by = .(pat_id)]

discr_perf_firsts <-preds %>% 
  map(~ .[(firsts[test, on = .(pat_id, idx_ed)])$n == 1, ]) %>% 
  map(bootstrap_performance, n_boot = 1000, seed = 55) %>% 
  map_dfr(extract_peformance, .id = "model")



# Evaluate the calibration ------------------------------------------------

cal_train <- fits %>% 
  map2(
    prepped, 
    ~ predict(.x, new_data = bake(.y, cal) %>% select(-growth), type = "prob")
  ) %>% 
  map(bind_cols, cal[, .(growth)])


cal_mods <- cal_train %>%
  map(~ glm(growth == "yes" ~ .pred_yes, data = ., family = binomial))

cal_preds <- map2(
  preds,
  cal_mods,
  ~ mutate(
    .x, 
    .pred_yes = predict(.y, .x, type = "response"), .pred_no = 1 - .pred_yes
  )
)


write_rds(
  cal_preds, 
  file.path(.dir_perf, "02_calibration", "external", preproc, "all_cal_preds.rds")
)

# Calculate the calibration intercept and slope
source(file.path(.dir_custom, "yardstick", "yardstick_cal_intercept.R"))
source(file.path(.dir_custom, "yardstick", "yardstick_cal_slope.R"))

map_df(cal_preds, cal_intercept, growth, .pred_yes, .id = "model")
map_df(cal_preds, cal_slope, growth, .pred_yes, .id = "model")

map_df(preds, cal_intercept, growth, .pred_yes, .id = "model")
map_df(preds, cal_slope, growth, .pred_yes, .id = "model")


