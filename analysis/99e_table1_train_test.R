
source(file.path(.dir_root, "00_init.R"))

library(knitr)
library(kableExtra)
library(tableone)
library(rsample)

train <- analysis(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
cal <- assessment(read_rds(file.path(.dir_rsmpl, "train_rs_cal.rds")))
test <- assessment(read_rds(file.path(.dir_rsmpl, "train_test.rds")))

data <- bind_rows(
  train %>% mutate(set = "training"),
  test %>% mutate(set = "testing")
) %>% as.data.table()

age_map <- c(rep("18-64", 5), rep("65+", 4))
names(age_map) <- levels(full_data$age)
data[, age_bin := fct_relabel(age, function(x) age_map[as.character(x)])]

data[, ethnicity := fct_recode(ethnicity, NULL = "unknown")]

data[, cci_tri := cut(cci, c(-Inf, 0, 2, Inf), c("0", "1-2", "$geq$3"))]

data[, hosp_12m := fct_yesno(hosp_n_12m > 0)]

data[, 
  susp := fct_collapse(
    susp, 
    uti = c("UTI", "Pyelo", "Urosepsis"),
    sym = c("Altered mental status", "Abdominal pain", "UTI symptoms"),
    inf = c("Sepsis", "Fever", "LRTI", "Other infection"),
    oth = c("Genitourinary problem", "Other")
)]

create_table1 <- function(total, strat, shell, nonnormal = NULL, 
                          catDigits = 1, contDigits = 2, pDigits = 3){
  # Make a latex version of tables 1a and 1b
  
  shell %<>% .[!is.na(label)]
  
  # Extract the table from the `tableone` object
  tbl_1 <- 
    list(total, strat) %>% 
    map(print, dropEqual = TRUE, printToggle = FALSE, nonnormal = nonnormal,
        catDigits = catDigits, contDigits = contDigits, pDigits = pDigits) %>% 
    reduce(cbind) %>%
    as.data.table(keep.rownames = "var") %>% 
    .[, !("test")]
  
  tbl_1 %<>% .[, map(.SD, str_trim)]
  
  # Remove the headers
  tbl_1[ , p := if_else(p == "" & lag(p) != "", lag(p), p, p)] 
  tbl_1 %<>% .[Overall != ""]
  
  # Merge to the table shell
  tbl_1 %<>% .[shell, on = "var", nomatch = 0]
  tbl_1[, label := str_replace(label, "\\%", "\\\\%")]
  tbl_1[, label := str_replace(label, "\\&", "\\\\&")]
  
  # Render the table in latex
  tbl_1_rend <- 
    tbl_1[, c("label", "Overall", "training", "testing", "p")] %>% 
    kable(
      "markdown", 
      booktabs = TRUE,
      linesep = "",
      col.names = c("", "Overall", "Trainig", "Test", "p-value"),
      escape = FALSE,
      caption = "Table 1" 
    )
  
  # Add row headers
  rhead_1 <- tbl_1 %>% 
    .[, .(header, row = 1:.N)] %>% 
    .[!is.na(header), .(start = min(row), end = max(row)), by = header]
  
  for(i in 1:nrow(rhead_1)){
    tbl_1_rend %<>% 
      pack_rows(rhead_1[i]$header, rhead_1[i]$start, rhead_1[i]$end)
  }
  
  # Print the table
  tbl_1_rend
}



# Table 1a: patient characteristics and medical history
covar <- c("age_bin", "sex", "ethnicity", 
           "cci_tri", "cancer", "immuno", "renal", "uro", "renal_surg", 
           "hosp_12m", "hosp_uti_12m", "urine_12m", "pos_12m", "abx_12m")


tbl_1a_total <- CreateTableOne(covar, data = data)
tbl_1a_strat <- CreateTableOne(covar, strata = "set", data = data, )

shell_1a <- read_excel(file.path("03_cleaning", "04a_table_shells.xlsx"), 
                       sheet = "table_1a")
setDT(shell_1a)

create_table1(tbl_1a_total, to_row_percent(tbl_1a_strat), shell_1a)


# Table 1b: ED diagnosis
covar <- c("susp")

tbl_1b_total <- CreateTableOne(covar, data = data)
tbl_1b_strat <- CreateTableOne(covar, strata = "set", data = data)

shell_1b <- tribble(
  ~var, ~label, ~header,
  "uti", "UTI", NA,
  "sym", "UTI symptom", NA,
  "inf", "Other infection", NA,
  "oth", "Other diagnoses", NA
)
setDT(shell_1b)

create_table1(tbl_1b_total, to_row_percent(tbl_1b_strat), shell_1b)



# Table 1c: clinical presentation

covar <- c("ua_bacteria", "ua_epithelial", "ua_rbc_total", "ua_wbc",
           "hr", "rr", "temp", "bp", "o2",
           "crp", "wcc", "plats", "creat", "bili", "alp")

# Scale the variables for easier display
scl_data <- copy(data)
scl_data[, ua_bacteria := ua_bacteria / 1000]


tbl_1c_total <- CreateTableOne(covar, data = scl_data)
tbl_1c_strat <- CreateTableOne(covar, strata = "set", data = scl_data)


shell_1c <- read_excel(file.path("03_cleaning", "04a_table_shells.xlsx"), 
                       sheet = "table_1c")
setDT(shell_1c)        

create_table1(tbl_1c_total, tbl_1c_strat, shell_1c, covar, contDigits = 1)
