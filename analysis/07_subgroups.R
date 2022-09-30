
.dir_root <- "99_manuscript"
source(file.path(.dir_root, "00_init.R"))
source(file.path(.dir_root, "04a_define_performance.R"))


# Load the relevant data items
all_preds <- read_rds( file.path(.dir_mods, "external", "yj", "all_preds.rds"))
recal <- read_rds(file.path(.dir_perf, "02_calibration", "external", "yj", "all_cal_preds.rds"))
test <- assessment(read_rds(file.path(.dir_rsmpl, "train_test.rds")))

xgb <- test %>% 
  select(pat_id, idx_ed, susp, age, sex, ethnicity) %>% 
  bind_cols(all_preds$xgb_full)

xgb_re <- test %>% 
  select(pat_id, idx_ed, susp, age, sex, ethnicity) %>% 
  bind_cols(recal$xgb_full)


# Discriminative performance ----------------------------------------------

bootstrap_sub_performance <- function(preds, by, n_boot = 1000){
  # Bootstrap confidence intervals for external performance by subgroups
  #
  # Note: this is different from bootstrap for internal validation
  
  estimate <- function(pred, by){
    thrsh <- get_threshold(pred, 0.95)
    
    bin <- expr(factor(.pred_yes > thrsh, c(TRUE, FALSE), c("yes", "no")))
    pred %<>% mutate(class = eval(bin))
    
    calc <- function(growth, .pred_yes, class) {
      x <- tibble(growth, .pred_yes, class)
      bind_rows(
        roc_auc(x, growth, .pred_yes),
        pr_auc(x, growth, .pred_yes),
        spec(x, growth, class),
        npv(x, growth, class)
      ) %>% select(-.estimator)
    }
    
    pred %>% 
      group_by(sub = rlang::eval_tidy(by)) %>% 
      summarise(perf =list(calc(growth, .pred_yes, class))) %>% 
      unnest(perf)
  }
  
  boot <- bootstraps(preds, times = n_boot)
  
  main_est <- estimate(preds, by)
  boot_perf <- boot$splits %>% 
    map_df(~ estimate(analysis(.), by), .id = "nmr")
  
  boot_ci <- boot_perf %>% 
    group_by(sub, .metric) %>% 
    summarise(
      conf.low = quantile(.estimate, 0.025),
      conf.high = quantile(.estimate, 0.975)
    ) %>% 
    inner_join(main_est, by = c("sub", ".metric")) %>% 
    select(.metric, .estimate, conf.low, conf.high) %>% 
    mutate_at(
      vars(one_of(".estimate", "conf.low", "conf.high")),
      ~ str_pad(round(., digits = 3), 5, "right", "0")
    )
  
  boot_comp <- boot_perf %>% 
    filter(.metric == "roc_auc") %>% 
    pivot_wider(id = "nmr", names_from = "sub",  values_from = ".estimate") %>% 
    mutate(FALSE_lower = `FALSE` < `TRUE`) %>% 
    summarise(P_FALSE_lower = mean(FALSE_lower))
  
  list(boot_ci, boot_comp)
}


# ED diagnoses
set.seed(9999)
bootstrap_sub_performance(xgb, expr(susp %in% c("UTI", "Pyelo", "Urosepsis")))

set.seed(9999) # Use the same seed to get the same bootstrap
bootstrap_sub_performance(
  xgb, 
  expr(susp %in% c(
    "UTI", "Pyelo", "Urosepsis", 
    "UTI symptoms", "Abdominal pain", "Altered mental status")
  )
)

# Age
set.seed(9999)
bootstrap_sub_performance(xgb, expr(as.numeric(age) < 6))

# Sex
set.seed(9999)
bootstrap_sub_performance(xgb, expr(sex == "female"))

# Ethnicity
set.seed(9999)
bootstrap_sub_performance(
  xgb %>% filter(ethnicity != "unknown"), 
  expr(ethnicity == "white")
)



# Calibration -------------------------------------------------------------

xgb %<>% mutate(age_bin = fct_yesno(as.numeric(age) >= 6))
xgb_re %<>% mutate(age_bin = fct_yesno(as.numeric(age) >= 6))


# Plot calibration
age_sex_lblr <- labeller(
  sex = c(male = "Men", female = "Women"), 
  age_bin = c(no = "<65 years", yes = "\u226565 years")
)

# Colour-blind friendly palette
cols <- c("#000000", "#E69F00")

ggplot(
  xgb_re, 
  aes(
    x = .pred_yes, 
    y = as.integer(growth == "yes")
  )
) + 
  geom_rug(data = xgb_re %>% filter(growth == "yes"), sides = "tl", alpha = 0.1) + 
  geom_rug(data = xgb_re %>% filter(growth == "no"), sides = "bl", alpha = 0.1) + 
  geom_abline(colour = "darkgrey", show.legend = FALSE) + 
  geom_smooth(
    data = xgb, 
    aes(colour = "Raw", linetype = "2"), 
    method = "loess", se = FALSE
  ) + 
  geom_smooth(
    aes(colour = "Re-calibrated", linetype = "1"),
    method = "loess",
    fill = "#AAAAAA"
  ) + 
  scale_x_continuous(breaks = 0:5 / 5) + 
  scale_y_continuous(breaks = 0:5 / 5) + 
  scale_color_manual(values = rev(cols)) +
  scale_alpha_continuous(range = c(0, 0.5)) + 
  facet_grid(age_bin ~ sex, labeller = age_sex_lblr) + 
  labs(
    x = "\nPredicted probability",
    y = "Observed proportion\n", 
    colour = "Probabilites:"
  ) + 
  guides(
    colour = guide_legend(override.aes = list(fill = NA, linetype = c(2, 1))),
    linetype = FALSE
  ) + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + 
  theme_bw() + 
  theme(
    panel.spacing = unit(2, "lines"),
    panel.grid.minor = element_blank(), 
    legend.position = "top"
  )


ggsave(file.path(.dir_plot, "external", "yj", "cal-subgroups.png"),
       width = 7, height = 7, dpi = 300)

