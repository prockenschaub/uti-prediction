
if(!exists(".initialised")){
  # Initialise the working environment
  .dir_root <- "99_manuscript"
  source(file.path(.dir_root, "00_init.R"))
  
  # Which dataset to use
  rs_type <- "cv"
  rs_size <- "full"
  
  # Which predictor set to use (full/reduced)
  pred_set <- "full"
  
  # Which imputation to use
  imp_meth <- "mean"
  
  ctrl_experiment <- list(
    # Random seeds
    seed = 5588,
    
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
}


# Define the preprocessing ------------------------------------------------

formu <- list(pipe_rm_unused, pipe_winsor, pipe_yeojohnson, pipe_meanimpute)
one_hot <- c(formu, list(pipe_one_hot))

max_mtry <- switch(pred_set,
                   full = 89,
                   reduced = 20)


candidate_pool <- list(
  # Standard logistic regression #########################
  lr = list(
    engine = engine_log_reg,
    params = engine_log_reg %>% parameters(),
    prepro = formu
  ), 

  # Gradient boosting tree ###############################
  xgb = list(
    engine = engine_xgb, 
    params = engine_xgb %>% 
      parameters() %>% 
      update(
        tree_depth = tree_depth(range = c(1L, 20L)), 
        mtry = mtry(range(1L, max_mtry)),
        sample_size = sample_prop(range = c(0.2, 0.8)),
        learn_rate = learn_rate(range = c(-3, -0.7)),
        loss_reduction = loss_reduction(range = c(-3, -1))) %>% 
      grid_random_seed(size = 30, seed = 42) %>% 
      select(-trees) %>% 
      crossing(tibble(trees = c(100L, 500L, 1000L))),
    prepro = one_hot
  )
)



# Set up parallel environment if specified --------------------------------

cat("Setup parallel environment\n")

if(ctrl_experiment$tune$allow_par == TRUE){
  setup_parallel()
}




# Run the same tuning procedure -------------------------------------------

perf <- as_tibble(
  expand.grid(
    year = 2013:2017,
    model = c("lr", "xgb")
    )
  ) %>% 
  mutate(
    int = NA,
    int_sd = NA,
    ext = NA
  )


for(i in 1:nrow(perf)){
  
  # Load the data -----------------------------------------------------------
  
  data <- analysis(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
  
  # Change the event to first level to conform with ``tune`` out-of-the-box
  # (see issue # on github)
  data[, growth := fct_relevel(growth, "yes", after = 0L)]
  data %<>% .[arrival_date >= ymd("2011-10-01")]
  
  # Remove patients from the test set who are also part of the training set
  time_cut <- make_datetime(perf$year[i])
  
  ids_pre <- data %>% 
    .[arrival_date < time_cut, .(pat_id)] %>% 
    unique()
  
  dups <- data %>% 
    .[arrival_date >= time_cut, .(pat_id, idx_ed)] %>% 
    .[ids_pre, on = "pat_id", nomatch = 0]
  
  data %<>% .[!dups, on = .(pat_id, idx_ed)]
  data %<>% .[arrival_date < time_cut %m+% years(1)]
  
  # Sample into an initial split
  prop_train <- mean(data$arrival_date < time_cut)
  
  setorder(data, arrival_date)
  train_test <- initial_time_split(data, prop = prop_train)
  
  # Resample the training data
  set.seed(679)
  cv <- vfold_cv(analysis(train_test), 10, 3)
  

  # Run the tuning ----------------------------------------------------------
  can <- candidate_pool[[as.character(perf$model[i])]]
  data_strct <- analysis(train_test)[0, ]
  res <- tune_model(cv, can, ctrl_experiment)
  
  bst <- show_best(res, "roc_auc", 1)
  perf[i, "int"] <- bst$mean
  perf[i, "int_sd"] <- bst$std_err
  
  

  # Re-fit the best modal on all training data ------------------------------

  bst %<>% select(-(.metric:std_err)) %>% as.list()
  
  if(length(bst) > 0){
    eng <- do.call(set_args, args = c(list(object = can$engine), bst))
  } else {
    eng <- can$engine
  }
  
  rec <- do.call(
    pipe_combine, 
    args = c(list(recipe = ctrl_experiment$scope(data_strct)), can$prepro))
  prepped <- rec %>% prep(analysis(train_test))
  
  fit <- fit(eng, growth ~ .,  prepped %>% juice())
  pred <- predict(fit, prepped %>% bake(assessment(train_test)), type = "prob")
  
  perf[i, "ext"] <- roc_auc_vec(assessment(train_test)$growth, pred$.pred_yes)
}


write_rds(
    perf, 
    file.path(.dir_plot, rs_size, "iterative_perf.rds"),
)




# Load the results from full analysis -------------------------------------

folder <- file.path(.dir_perf, "01_discrimination", rs_size)
pattern <- str_c("(lr|xgb)_full_")

int <- file.path(folder, list.files(folder, pattern)) %>% 
  set_names(c("lr", "xgb")) %>% 
  map(read_rds) %>% 
  map_df(show_best, "roc_auc", 1, .id = "model") %>% 
  select(model, int = mean, int_sd = std_err)
  
ext <- file.path(.dir_mods, "external", "yj", "all_preds.rds") %>% 
  read_rds() %>% 
  .[c("lr_full", "xgb_full")] %>% 
  map_df(roc_auc, growth, .pred_yes, .id = "model") %>% 
  select(model, ext = .estimate) %>% 
  mutate(model = str_extract(model, "lr|xgb"), year = 2018)

final <- inner_join(int, ext, by = "model") %>% 
  mutate(model = factor(model, c("lr", "xgb")))



# Plot --------------------------------------------------------------------

cols <- c("#000000", "#E69F00")

long <- bind_rows(
  perf %>% mutate(),
  final
)%>% 
  gather(key = "k", value = "v", -year, -model, -int_sd)

val_labels <- c(
  "ext" = "External validation (test data from current year)",
  "int" = "Internal validation (resampled training data from years before)"
)

mod_labels <- c(
  "lr" = "LR",
  "xgb" = "XGB"
)

ggplot(
    NULL,
    aes(year, v, colour = k)
  )  +
  geom_pointrange(
    data = long %>% filter(k == "int", model == "xgb"),
    aes(
      ymin = v - 2 * int_sd, 
      ymax = v + 2 * int_sd
    ),
    shape = 20
  ) + 
  geom_point(
    data = long %>% filter(k == "ext", model == "xgb"),
    size = 3, 
    shape = 18
  ) + 
  scale_y_continuous(limits = c(0.7, 0.85)) + 
  scale_colour_manual(values = cols, labels = val_labels) + 
  #facet_wrap(model ~ ., labeller = labeller(model = mod_labels)) + 
  guides(colour = guide_legend(ncol = 1)) + 
  labs(
    x = "\nYear of external validation",
    y = "Estimated AUC\n", 
    colour = ""
  ) + 
  theme_bw() + 
  theme(
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )


ggsave(file.path("99_manuscript", "02_results", "03_plots", "iterative_perf.png"),
       width = 5, height = 5, dpi = 300)



