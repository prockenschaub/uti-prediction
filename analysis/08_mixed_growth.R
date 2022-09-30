
if(!exists(".initialised")){
  # Initialise the working environment
  .dir_root <- "99_manuscript"
  source(file.path(.dir_root, "00_init.R"))
  source(file.path(.dir_root, "04a_define_performance.R"))
  
  # Which dataset to use
  rs_type <- "cv"
  rs_size <- "full"
  
  # Which models to process
  models <- "xgb"
  preproc <- "yj"
  
  # Which imputation method
  imp_meth <- "mean"
  
  # Which metrics to compare and what criteria to use for the best model
  metrics <- metric_set(roc_auc, pr_auc, sens, spec)
}



# Load the models ---------------------------------------------------------

folder <- file.path(.dir_mods, rs_size, rs_type, imp_meth, preproc)
pattern <- str_c("(", str_c(models, collapse = "|"), ")_(full).rds")

model_files <- file.path(folder, "best") %>% 
  list.files(pattern)

model_nms <- model_files %>% str_replace_all(".rds", "")

model_objs <- model_files %>% 
  set_names(model_nms) %>% 
  map(~file.path(folder, "perf", .)) %>% 
  map(read_rds)

# Get main model predictions
preds <- read_rds( file.path(.dir_mods, "external", "yj", "all_preds.rds"))$xgb_full

# Get data
train <- analysis(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
test <- assessment(read_rds(file.path(.dir_rsmpl, "train_test.rds")))


# Get the best hyperparameter combination ---------------------------------

best_params <- model_objs %>% 
  map(show_best, "roc_auc", 1) %>% 
  map(~ select(., -(.metric:std_err))) %>% 
  map(distinct)


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
    pivot_wider(names_from = ".metric", values_from = "ci")
}


# Train the final models --------------------------------------------------

eng <- set_args(engine_xgb, !!!as.list(best_params$xgb_full))
preproc <- list(
  pipe_rm_unused, pipe_winsor, pipe_log, 
  pipe_meanimpute, pipe_one_hot
)

set_best_args <- function(clss, nm){
  if(ncol(best_params[[nm]]) == 0){
    engines[[clss]]
  } else {
    set_args(engines[[clss]], !!!as.list(best_params[[nm]]))
  }
}

rec <- get_predictors("full") %>% 
  set_scope(data = train) %>% 
  pipe_combine(!!!preproc) %>% 
  step_rm(recipes::has_role("id var"))


# Treat mixed growth as negative (default)
set.seed(44)
prepd_neg <- prep(rec, train)
juiced_neg <- juice(prepd_neg)
fits_neg <- fit_xy(eng, juiced_neg %>% select(-growth), juiced_neg$growth)
preds_neg <- fits_neg %>% 
  predict(
    new_data = bake(prepd_neg, test) %>% 
      select(-growth), 
    type = "prob"
  ) %>% 
  bind_cols(test %>% select(growth))

set.seed(55)
neg_bs <- bootstraps(preds_neg, times = 1000) %>% 
  mutate(
    results = map(splits, ~ roc_auc(data = analysis(.), growth, .pred_yes))
  )
neg_perf <- neg_bs %>% extract_peformance()



# Treat mixed growth as positive
train_pos <- train %>% 
  mutate(growth = if_else(hmg == "yes", factor("yes", levels(growth)), growth))
test_pos <- test %>% 
  mutate(growth = if_else(hmg == "yes", factor("yes", levels(growth)), growth))

set.seed(44)
prepd_pos <- prep(rec, train_pos)
juiced_pos <- juice(prepd_pos)
fits_pos <- fit_xy(eng, juiced_pos %>% select(-growth), juiced_pos$growth)
preds_pos <- fits_pos %>% 
  predict(
    new_data = bake(prepd_pos, test_pos) %>% 
      select(-growth), 
    type = "prob"
  ) %>% 
  bind_cols(test_pos %>% select(growth))

set.seed(55)
pos_bs <- bootstraps(preds_pos, times = 1000) %>% 
  mutate(
    results = map(splits, ~ roc_auc(data = analysis(.), growth, .pred_yes))
  )
pos_perf <- pos_bs %>% extract_peformance()


# Remove mixed growth altogether
train_rem <- train %>% 
  filter(hmg == "no")
test_rem <- test %>% 
  filter(hmg == "no")

set.seed(44)
prepd_rem <- prep(rec, train_rem)
juiced_rem <- juice(prepd_rem)
fits_rem <- fit_xy(eng, juiced_rem %>% select(-growth), juiced_rem$growth)
preds_rem <- fits_rem %>% 
  predict(
    new_data = bake(prepd_rem, test_rem) %>% 
      select(-growth), 
    type = "prob"
  ) %>% 
  bind_cols(test_rem %>% select(growth))

set.seed(55)
rem_bs <- bootstraps(preds_rem, times = 1000) %>% 
  mutate(
    results = map(splits, ~ roc_auc(data = analysis(.), growth, .pred_yes))
  )
rem_perf <- rem_bs %>% extract_peformance()


# Run predictions again on all test sets to compare bootstraps between defs
preds_rem2 <- fits_rem %>% 
  predict(
    new_data = bake(prepd_rem, test) %>% 
      select(-growth), 
    type = "prob"
  ) %>% 
  bind_cols(test %>% select(growth, hmg))


set.seed(55)
rem_bs2 <- bootstraps(preds_rem2, times = 1000) %>% 
  mutate(
    splits = map(splits, subset_split, expr(hmg == "no")), 
    results = map(splits, ~ roc_auc(data = analysis(.), growth, .pred_yes))
  )
rem_perf2 <- rem_bs2 %>% extract_peformance()



# Compare performance -----------------------------------------------------

bind_cols(
    neg_bs %>% unnest(results) %>% select(neg = .estimate),
    pos_bs %>% unnest(results) %>% select(pos = .estimate),
    rem_bs2 %>% unnest(results) %>% select(rem = .estimate)
  ) %>% 
  mutate(
    comp_pos = neg < pos,
    comp_rem = neg < rem
  ) %>% 
  summarise_all(mean)



# Predictions -------------------------------------------------------------

pred_change <- bind_rows(
    preds %>% 
      bind_cols(test %>% select(hmg)) %>% 
      mutate(treat_as = "neg"),
    preds_pos %>% 
      bind_cols(test %>% select(hmg)) %>% 
      mutate(treat_as = "pos"),
    preds_rem2 %>% 
      bind_cols(test %>% select(hmg)) %>% 
      mutate(treat_as = "rem")
  ) %>% 
  mutate(
    treat_as = factor(treat_as, c("neg", "pos", "rem")),
    res = case_when(
        hmg == "yes" ~ "mixed",
        growth == "yes" ~ "positive",
        TRUE ~ "negative"
      ) %>% 
      factor(
        c("positive", "mixed", "negative"),
        c("dominant growth", "mixed growth", "no  growth")
      )
  )

summ <- . %>% 
  group_by(res) %>% 
  summarise(
    mean = mean(.pred_yes),
    sd = sd(.pred_yes),
    median = median(.pred_yes)
  )

pred_change %>% 
  filter(treat_as == "neg") %>% 
  summ()

pred_change %>% 
  filter(treat_as == "pos") %>% 
  summ()

pred_change %>% 
  filter(treat_as == "rem") %>% 
  summ()


mx_lbls <- c(
  neg = "Treat mixed growth as negative",
  pos = "Treat mixed growth as positive",
  rem = "Remove mixed growth during training"
)

ggplot(pred_change, aes(.pred_yes, colour = res, linetype = res)) + 
  geom_freqpoly(bins = 50) + 
  scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9")) + 
  scale_linetype_manual(values = c(2, 4, 1)) + 
  facet_wrap(~ treat_as, ncol = 1, labeller = labeller(treat_as = mx_lbls)) + 
  coord_cartesian(xlim = c(0, 1), expand = FALSE) + 
  labs(
    x = "\nPredicted probability",
    y = "Number of patients\n",
    colour = "Culture result:",
    linetype = "Culture result:"
  ) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave(file.path(.dir_plot, "external", "yj", "mixed.png"),
       width = 7, height = 5, dpi = 300)

