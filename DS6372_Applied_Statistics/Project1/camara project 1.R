#Setup 
set.seed(1234)
pkgs <- c("tidyverse","janitor","skimr","broom","caret")
to_install <- pkgs[!suppressWarnings(sapply(pkgs, requireNamespace, quietly = TRUE))]
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

glimpse(AutoDF)
skimr::skim(AutoDF)

AutoDF %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  arrange(desc(n_missing)) -> na_overview
print(na_overview)

#Train/Test split 80/20
idx <- caret::createDataPartition(AutoDF$MSRP, p = 0.80, list = FALSE)
train_auto <- AutoDF[idx, ]
test_auto  <- AutoDF[-idx, ]
nrow(train_auto); nrow(test_auto)

#Numeric columns in TRAIN 
num_cols <- train_auto %>% dplyr::select(where(is.numeric)) %>% names()

#NA audit for TRAIN numerics 
train_auto %>%
  summarise(across(all_of(num_cols), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "num_col", values_to = "n_missing") %>%
  arrange(desc(n_missing)) -> na_train_numeric
print(na_train_numeric)

#Compute TRAIN medians (for all numerics)
train_medians <- train_auto %>%
  summarise(across(all_of(num_cols), ~median(., na.rm = TRUE))) %>%
  as.list()
#Peek the 3 imputed vars, Most Important
train_medians[c("Engine.HP","Engine.Cylinders","Number.of.Doors")]

#Median imputation 
impute_with_medians <- function(df, med_list, cols) {
  df %>% mutate(across(all_of(cols), ~ ifelse(is.na(.), med_list[[cur_column()]], .)))
}

#Apply to TRAIN and TEST using TRAIN medians
train_imp <- impute_with_medians(train_auto, train_medians, num_cols)
test_imp  <- impute_with_medians(test_auto,  train_medians, num_cols)

#Verify numeric NAs are gone ----
check_na_numeric <- function(df, cols) {
  df %>%
    summarise(across(all_of(cols), ~sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything(), names_to = "num_col", values_to = "n_missing") %>%
    arrange(desc(n_missing))
}
check_na_numeric(train_imp, num_cols)
check_na_numeric(test_imp,  num_cols)

#Pre/Post summaries for imputed vars
summary(dplyr::select(train_auto, Engine.HP, Engine.Cylinders, Number.of.Doors))
summary(dplyr::select(train_imp,  Engine.HP, Engine.Cylinders, Number.of.Doors))
