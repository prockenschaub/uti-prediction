
if(!exists(".initialised")){
  source(file.path("99_manuscript", "00_init.R"))
}


load_ed <- function(){
  # Dataset
  data <- readRDS(file.path(.dir_data, "all_pat_ed.rds"))
  
  # Combine ethnicity
  data[, ethnicity := fct_collapse(ethnicity, other = c("mixed", "other"))]
  
  # Limit only to patients whose samples were cultured
  data %<>% .[results == "yes"]
  
  data
}


add_miss_ind <- function(data){
  
  data <- copy(data)
  
  data[, c("miss_ua", "miss_ua_some", "miss_ua_cast",
           "miss_wcc", "miss_creat", "miss_albi", "miss_crp") 
       := fct_yesno(FALSE)]

  data[is.na(ua_bacteria) & is.na(ua_casts_total) & is.na(ua_casts_prcnt) &
         is.na(ua_conductivity) & is.na(ua_epithelial) & is.na(ua_other) & 
         is.na(ua_rbc_total) & is.na(ua_rbc_prcnt) & is.na(ua_sml_rnd_cells) &
         is.na(ua_sperm) & is.na(ua_wbc) & is.na(ua_crystals) & is.na(ua_yeast),
       miss_ua := fct_yesno(TRUE)]
  data[miss_ua == "no" & 
         is.na(ua_casts_prcnt) & is.na(ua_conductivity) & 
         is.na(ua_other) & is.na(ua_crystals),
       miss_ua_some := fct_yesno(TRUE)]
  data[miss_ua == "no" & miss_ua_some == "no" & is.na(ua_casts_prcnt),
       miss_ua_cast := fct_yesno(TRUE)]
  
  data[is.na(wcc), miss_wcc := fct_yesno(TRUE)]
  data[is.na(creat), miss_creat := fct_yesno(TRUE)]
  data[is.na(alp) & is.na(bili), miss_albi := fct_yesno(TRUE)]
  data[is.na(crp), miss_crp := fct_yesno(TRUE)]
  
  data
}


train_test_split <- function(data){
  train <- data[as.numeric(as.character(year)) < 2018]
  test <- data[!train, on = .(pat_id)]
  
  list(train = train, test = test)
}


# Define variables --------------------------------------------------------

# ID variables
.ids <- c("pat_id", "idx_ed", "idx_urine")

# Outcome of interest (i.e. prediction target or label)
.outcome <- "growth"

# Predictor variables
.variables <- list(
  demo  = c("age", "sex", "ethnicity"),
  ed    = c("susp"),
  ua    = c("ua_bacteria", "ua_casts_total", "ua_epithelial", "ua_rbc_total", 
            "ua_sml_rnd_cells", "ua_wbc", "ua_crystals"),
  lab   = c("wcc", "plats", "crp", "creat", "alp", "bili"),
  com   = c("cci", "cancer", "renal", "uro", "renal_surg"),
  past  = c("hosp_uti_n_24m", "ed_uti_n_24m", "urine_12m", "pos_12m", 
            "hosp_n_12m", "hosp_7d", "ed_n_12m"),
  time  = c("month", "day_of_year", "day_of_week", "time_of_day"),
  abx   = c("abx_12m"),
  miss  = c("miss_ua", "miss_ua_some", "miss_ua_cast",
            "miss_wcc", "miss_creat", "miss_albi", "miss_crp") 
)


get_predictors <- function(set){
  switch(set, 
         full = predictors_full(),
         full_noind = predictors_full_noind(),
         reduced = predictors_reduced(),
         reduced_noind = predictors_red_noind())
}

predictors_full <- function(){
  unlist(.variables)
}

predictors_reduced <- function(){
  c(predictors_red_noind(), "miss_ua", "miss_ua_some", "miss_ua_cast")
}

predictors_full_noind <- function(){
  unlist(.variables[names(.variables) != "miss"])
}

predictors_red_noind <- function(){
  c("age", "sex", "pos_12m", .variables$ua)
}
predictors_skew <- function(){
  c(.variables$ua, .variables$lab)
}

# Variables with missingness
missing_vars <- function(){
  c(.variables$ed, .variables$ua, .variables$lab)
}
