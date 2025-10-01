
rm(list = ls(all = TRUE))
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(caret)
library(ranger)
library(stringr)
library(ROSE)
library(DMwR)
library(DALEX)

setwd("C:/Users/ptwoj/Dropbox/__biezace/2024-02-07_BBS_zombie_po_recenzjach/")

### data and formulas

all_weekly_snapshots <- fread("_cryptos_weekly_snapshots/all_symbols.csv")

# simple analyses

#--------------------------------------------------
# transformations based on statistics

all_weekly_snapshots2 <- all_weekly_snapshots

statistics_all_variables_PW <- 
  readxl::read_xlsx("statistics_all_variables_df_PW.xlsx", sheet = 1)

variables_to_log <- statistics_all_variables_PW %>% 
  filter(log == 1) %>% 
  dplyr::select(statystyka) %>% 
  pull()

for (variable_ in variables_to_log) {
  message(variable_)
  all_weekly_snapshots2[[variable_]] <- log(all_weekly_snapshots2[[variable_]] + 1)
}

#-------------------------------
# additional variables based on coin info

# zapisane w "02_initial_analyses_2023-12.R"
coins_info_all <- readxl::read_xlsx("coins_info_all.xlsx")

tags_frequencies_PW <- readxl::read_xlsx("tags_frequencies_PW.xlsx") %>% 
  filter(take_less2 == 1) %>% 
  group_split(tag_take)

# adding new columns

coins_info_all2 <- coins_info_all

for (i_tag in 1:length(tags_frequencies_PW)) {
  
  message(i_tag)
  
  tags_ <- tags_frequencies_PW[[i_tag]] %>% 
    dplyr::select(tag) %>% 
    pull()
  
  # add a new dummy column (1 if tag appears, 0 otherwise)
  coins_info_all2[, paste0("tag_", tags_frequencies_PW[[i_tag]]$tag_take[1])] <-
    ifelse(rowSums(sapply(tags_, function(u) str_detect(coins_info_all$tags, u)), 
                   na.rm = TRUE) > 0, 1, 0)
  
  rm(tags_)
}

coins_info_all2$tags <- NULL

rm(tags_frequencies_PW)

# twitter i subredit

coins_info_all2$has_twitter_account <- 
  if_else(!is.na(coins_info_all2$twitter_username), 1, 0)

coins_info_all2$twitter_username <- NULL

coins_info_all2$has_subreddit_account <- 
  if_else(!is.na(coins_info_all2$subreddit), 1, 0)

coins_info_all2$subreddit <- NULL

# remove unnecessary columns

coins_info_all2$urls <- NULL
coins_info_all2$description <- NULL
coins_info_all2$platform <- NULL
coins_info_all2$active <- NULL
coins_info_all2$status_for_20230425 <- NULL

# conversion of date
coins_info_all2$date_added <- as_date(str_sub(coins_info_all2$date_added, 1, 10))

# first dates in the data

coins_info_all3 <- 
  all_weekly_snapshots2 %>% 
  group_by(id, symbol, slug) %>% 
  mutate(i = 1:n()) %>% 
  filter(i == 1) %>% 
  ungroup() %>% 
  dplyr::select(id, symbol, slug, date) %>% 
  left_join(coins_info_all2 %>% 
              dplyr::select(-name), 
            by = join_by(id, symbol, slug)) %>% 
  mutate(date_dif = as.numeric(as_date(date) - date_added))

coins_info_all3 %>% 
  dplyr::select(1:4, date_added, date_dif) %>% 
  filter(date_dif < 0) %>% 
  arrange(date_dif)

# I update info about the first quotation
# (if a symbol observed earlier than the date_added info)

coins_info_all3 <- coins_info_all3 %>% 
  mutate(date_added2 = if_else(date_dif < 0, as_date(date), date_added))


# final combining of the data

all_weekly_snapshots3 <- all_weekly_snapshots2 %>% 
  left_join(coins_info_all3 %>% 
              dplyr::select(-date_added, -date), 
            by = join_by(id, symbol, slug))

# create a column "age"

all_weekly_snapshots3$age_in_days <-
  as.numeric(as_date(all_weekly_snapshots3$date) - all_weekly_snapshots3$date_added2)

all_weekly_snapshots3$date_added2 <- NULL
all_weekly_snapshots3$date_dif <- NULL

all_weekly_snapshots3$category <- as.factor(all_weekly_snapshots3$category)

# load summary function

source("F_summary_binary_with_AUROC.R")

predictors_all <- names(all_weekly_snapshots3)[c(25:100)]
predictors_all2 <- names(all_weekly_snapshots3)[c(25:104, 107:109)]

predictors_all_1a <- names(all_weekly_snapshots3)[c(25:100, 101, 107:109)]


if( class(all_weekly_snapshots3$delisted_in_next7days) != "factor") {
  
  all_weekly_snapshots3$delisted_in_next7days <- 
    factor(all_weekly_snapshots3$delisted_in_next7days,
           levels = c(1, 0),
           labels = c("Yes", "No"))
}


if( class(all_weekly_snapshots3$delisted_in_next28days) != "factor") {
  
  all_weekly_snapshots3$delisted_in_next28days <- 
    factor(all_weekly_snapshots3$delisted_in_next28days,
           levels = c(1, 0),
           labels = c("Yes", "No"))
}

# formulas

formula1_delisted_28days <- as.formula(paste0("delisted_in_next28days ~ ",
                                             paste0(predictors_all, collapse = " + ")))

formula2_delisted_28days <- as.formula(paste0("delisted_in_next28days ~ ",
                                             paste0(predictors_all2, collapse = " + ")))

formula1a_delisted_28days <- as.formula(paste0("delisted_in_next28days ~ ",
                                              paste0(predictors_all_1a, collapse = " + ")))

formula1_delisted_7days <- as.formula(paste0("delisted_in_next7days ~ ",
                                            paste0(predictors_all, collapse = " + ")))

formula2_delisted_7days <- as.formula(paste0("delisted_in_next7days ~ ",
                                             paste0(predictors_all2, collapse = " + ")))

formula1a_delisted_7days <- as.formula(paste0("delisted_in_next7days ~ ",
                                              paste0(predictors_all_1a, collapse = " + ")))


#-----
# import models summary

summary_all_algos <- read_xlsx("summary_all_algos_and_models_with_xgb.xlsx")

summary_best_algos <- 
  summary_all_algos %>% 
  filter(rank_validation == 1)

#--------------------------
# prepare data for reestimation of best models on last subsample

train_end <- "2022-06-30"

train_start <- floor_date(as_date(train_end), "month") - months(35)
test_start <- as_date(train_end) + days(1)
test_end <- ceiling_date(test_start + months(5), "month") - 1

data_train <- all_weekly_snapshots3 %>% 
  filter(between(as_date(date), train_start, as_date(train_end)))

data_test <- all_weekly_snapshots3 %>% 
  filter(between(as_date(date), test_start, test_end))

data_train <- na.omit(data_train)
data_test <- na.omit(data_test)

# downsampled training data

# 28 days

set.seed(987654321)

data_train_down_1_28days <- 
  downSample(data_train %>% 
               dplyr::select(delisted_in_next28days,
                             all_of(predictors_all)), 
             data_train$delisted_in_next28days)

set.seed(987654321)

data_train_down_1a_28days <-
  downSample(data_train %>% 
               dplyr::select(delisted_in_next28days,
                             all_of(predictors_all_1a)), 
             data_train$delisted_in_next28days)

set.seed(987654321)

data_train_down_2_28days <-
  downSample(data_train %>% 
               dplyr::select(delisted_in_next28days,
                             all_of(predictors_all2)), 
             data_train$delisted_in_next28days)

set.seed(987654321)

data_train_ROSE_1_28days <- 
  ROSE(delisted_in_next28days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next28days,
                       all_of(predictors_all)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_ROSE_1a_28days <- 
  ROSE(delisted_in_next28days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next28days,
                       all_of(predictors_all_1a)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_ROSE_2_28days <-
  ROSE(delisted_in_next28days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next28days,
                       all_of(predictors_all2)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_SMOTE_1_28days <- 
  SMOTE(delisted_in_next28days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next28days,
                        all_of(predictors_all)))

set.seed(987654321)

data_train_SMOTE_1a_28days <- 
  SMOTE(delisted_in_next28days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next28days,
                        all_of(predictors_all_1a)))

set.seed(987654321)

data_train_SMOTE_2_28days <- 
  SMOTE(delisted_in_next28days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next28days,
                        all_of(predictors_all2)))

# 7 days

set.seed(987654321)

data_train_down_1_7days <- 
  downSample(data_train %>% 
               dplyr::select(delisted_in_next7days,
                             all_of(predictors_all)), 
             data_train$delisted_in_next7days)

set.seed(987654321)

data_train_down_1a_7days <-
  downSample(data_train %>% 
               dplyr::select(delisted_in_next7days,
                             all_of(predictors_all_1a)), 
             data_train$delisted_in_next7days)

set.seed(987654321)

data_train_down_2_7days <-
  downSample(data_train %>% 
               dplyr::select(delisted_in_next7days,
                             all_of(predictors_all2)), 
             data_train$delisted_in_next7days)

set.seed(987654321)

data_train_ROSE_1_7days <- 
  ROSE(delisted_in_next7days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next7days,
                       all_of(predictors_all)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_ROSE_1a_7days <- 
  ROSE(delisted_in_next7days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next7days,
                       all_of(predictors_all_1a)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_ROSE_2_7days <-
  ROSE(delisted_in_next7days ~ .,
       data = data_train %>% 
         dplyr::select(delisted_in_next7days,
                       all_of(predictors_all2)),
       #  desired sample size of the resulting data set
       N = 10000)$data

set.seed(987654321)

data_train_SMOTE_1_7days <- 
  SMOTE(delisted_in_next7days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next7days,
                        all_of(predictors_all)))

set.seed(987654321)

data_train_SMOTE_1a_7days <- 
  SMOTE(delisted_in_next7days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next7days,
                        all_of(predictors_all_1a)))

set.seed(987654321)

data_train_SMOTE_2_7days <- 
  SMOTE(delisted_in_next7days ~ .,
        data = data_train %>% 
          dplyr::select(delisted_in_next7days,
                        all_of(predictors_all2)))


##-----------------
# hyperparameters

my_tr_control <- trainControl(method = "none",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

parameters_lasso <- expand.grid(alpha = 1,
                                lambda = exp(log(10)*seq(-4, 6, length.out = 50))) 

parameters_lasso <- parameters_lasso %>% 
  unite("hyperparameters", alpha, lambda, 
        sep = ", ", remove = FALSE)

parameters_rf_down <- expand.grid(mtry = seq(5, 70, 5),
                                  splitrule = "gini",
                                  min.node.size = c(50, 100, 200, 400))  %>% 
  unite("hyperparameters", 
        mtry, splitrule, min.node.size,
        sep = ", ", remove = FALSE)

params_xgboost <- expand.grid(nrounds = c(200, 400),
                              max_depth = c(4, 5, 6),
                              eta = c(0.06, 0.125), 
                              gamma = c(1),
                              colsample_bytree = c(0.4, 0.6, 0.8),
                              min_child_weight = c(100, 200, 400),
                              subsample = 1) %>% 
  unite("hyperparameters", 
        nrounds, max_depth,
        eta, gamma, colsample_bytree,
        min_child_weight, subsample,
        sep = ", ", remove = FALSE)
  

# -----------------
# reestimation of glm

# !!!!!!!!!!!! tu wychodzi problem kompletnej separacji!

algos_ <- summary_best_algos %>% 
  filter(algorithm == "glm")

for (i_row in 1:nrow(algos_))  {
  
  message(i_row)
  
  wariant_ <- algos_[i_row,]
  
  data_train_name <- paste0("data_train", "_",
                           toupper(wariant_$resampling), "_",
                           wariant_$model, "_",
                           readr::parse_number(wariant_$dependent),
                           "days")
  
  formula_name <- paste0("formula",
                         wariant_$model, 
                         "_delisted_",
                         readr::parse_number(wariant_$dependent),
                         "days")
  
  model_ <- train(get(formula_name),
                  data = get(data_train_name),
                  method = "glm",
                  maxit = 200,
                  preProcess = c("center", "scale"),
                  trControl = my_tr_control) # tu już BEZ down !!!

    summary(model_)
  
    summary_binary(predicted_probs = predict(model_, 
                                             newdata = data_test,
                                             type = "prob")$Yes,
                   real = data_test$delisted_in_next7days,
                   level_positive = "Yes", 
                   cutoff = 0.5,
                   level_negative = "No") 
    
    # rozwiązanie problemu kompletnej separacji
    # https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
    # https://www.r-bloggers.com/2010/11/example-8-15-firth-logistic-regression/
    
    model_l <- logistf(get(formula_name),
                       data = get(data_train_name))
    
    summary(model_l)
    
    summary_binary(predicted_probs =  predict(model_l, 
                                              type = "response"),
                   real = get(formula_name)$delisted_in_next7days,
                   level_positive = "Yes", 
                   cutoff = 0.5,
                   level_negative = "No") 
    
   
    summary_binary(predicted_probs =  predict(model_l, 
                                              newdata = data_test,
                                              type = "response"),
                   real = data_test$delisted_in_next7days,
                   level_positive = "Yes", 
                   cutoff = 0.5,
                   level_negative = "No") 
}


# -----------------
# reestimation of LASSO

algos_ <- summary_best_algos %>% 
  filter(algorithm == "lasso")

# i_row = 1

for (i_row in 1:nrow(algos_))  {
  
  message(i_row)
  
  wariant_ <- algos_[i_row,]
  hyperparams_ <- parameters_lasso %>% 
    filter(hyperparameters == wariant_$hyperparams) %>% 
    dplyr::select(-hyperparameters)
  
  data_train_name <- paste0("data_train", "_",
                            ifelse(wariant_$resampling == "down",
                                   wariant_$resampling,
                                   toupper(wariant_$resampling)), "_",
                            wariant_$model, "_",
                            readr::parse_number(wariant_$dependent),
                            "days")
  
  formula_name <- paste0("formula",
                         wariant_$model, 
                         "_delisted_",
                         readr::parse_number(wariant_$dependent),
                         "days")
  
  model_ <- train(get(formula_name),
                  data = get(data_train_name),
                  method = "glmnet",
                  tuneGrid = hyperparams_,
                  preProcess = c("center", "scale"),
                  trControl = my_tr_control) # tu już BEZ down !!!
  
  #summary_binary(predicted_probs = predict(model_, 
  #                                         newdata = data_test,
  #                                         type = "prob")$Yes,
  #               real = data_test$delisted_in_next7days,
  #               level_positive = "Yes", 
  #               cutoff = 0.5,
  #               level_negative = "No") 
  
  elements_ <- strsplit(wariant_$wariant_id, "_")[[1]]
  model_name <- paste0("model_", 
                       paste0(elements_[c(1, 3, 5, 9)], collapse = "_"))
  
  assign(model_name, model_)
  
  rm(wariant_, hyperparams_,
     data_train_name, formula_name, model_,
     elements_, model_name)
}

# -----------------
# reestimation of random forest

algos_ <- summary_best_algos %>% 
  filter(algorithm == "rf")

# i_row = 1

for (i_row in 1:nrow(algos_))  {
  
  message(i_row)
  
  wariant_ <- algos_[i_row,]
  hyperparams_ <- parameters_rf_down %>% 
    filter(hyperparameters == gsub(", 1, ", ", gini, ",
                                   wariant_$hyperparams)) %>% 
    dplyr::select(-hyperparameters)
  
  data_train_name <- paste0("data_train", "_",
                            ifelse(wariant_$resampling == "down",
                                   wariant_$resampling,
                                   toupper(wariant_$resampling)), "_",
                            wariant_$model, "_",
                            readr::parse_number(wariant_$dependent),
                            "days")
  
  formula_name <- paste0("formula",
                         wariant_$model, 
                         "_delisted_",
                         readr::parse_number(wariant_$dependent),
                         "days")
  
  set.seed(987654321)
  
  model_ <- train(get(formula_name),
                  data = get(data_train_name),
                  method = "ranger",
                  num.trees = 200,
                  tuneGrid = hyperparams_,
                  preProcess = c("center", "scale"),
                  trControl = my_tr_control) # tu już BEZ down !!!
  
  #summary_binary(predicted_probs = predict(model_, 
  #                                         newdata = data_test,
  #                                         type = "prob")$Yes,
  #               real = data_test$delisted_in_next7days,
  #               level_positive = "Yes", 
  #               cutoff = 0.5,
  #               level_negative = "No") 
  
  elements_ <- strsplit(wariant_$wariant_id, "_")[[1]]
  model_name <- paste0("model_", 
                       paste0(elements_[c(1, 3, 5, 9)], collapse = "_"))
  
  assign(model_name, model_)
  
  rm(wariant_, hyperparams_,
     data_train_name, formula_name, model_,
     elements_, model_name)
}


# -----------------
# reestimation of xgboost

algos_ <- summary_best_algos %>% 
  filter(algorithm == "xgb")

# i_row = 1

for (i_row in 1:nrow(algos_))  {
  
  message(i_row)
  
  wariant_ <- algos_[i_row,]
  hyperparams_ <- params_xgboost %>% 
    filter(hyperparameters == wariant_$hyperparams) %>% 
    dplyr::select(-hyperparameters)
  
  data_train_name <- paste0("data_train", "_",
                            ifelse(wariant_$resampling == "down",
                                   wariant_$resampling,
                                   toupper(wariant_$resampling)), "_",
                            wariant_$model, "_",
                            readr::parse_number(wariant_$dependent),
                            "days")
  
  formula_name <- paste0("formula",
                         wariant_$model, 
                         "_delisted_",
                         readr::parse_number(wariant_$dependent),
                         "days")
  
  set.seed(987654321)
  
  model_ <- train(get(formula_name),
                  data = get(data_train_name),
                  method = "xgbTree",
                  tuneGrid = hyperparams_,
                  preProcess = c("center", "scale"),
                  trControl = my_tr_control) # tu już BEZ down !!!
  
  #summary_binary(predicted_probs = predict(model_, 
  #                                         newdata = data_test,
  #                                         type = "prob")$Yes,
  #               real = data_test$delisted_in_next7days,
  #               level_positive = "Yes", 
  #               cutoff = 0.5,
  #               level_negative = "No") 
  
  elements_ <- strsplit(wariant_$wariant_id, "_")[[1]]
  model_name <- paste0("model_", 
                       paste0(elements_[c(1, 3, 5, 9)], collapse = "_"))
  
  assign(model_name, model_)
  
  rm(wariant_, hyperparams_,
     data_train_name, formula_name, model_,
     elements_, model_name)
}


# explainers

modele_ <- ls(pattern = "model_")

# explainers + wykresy ważności zmiennych

# na pełnych danych 

mod_ = modele_[1]

for (mod_ in modele_) {
  
  message(mod_)
  
  elements_ <- strsplit(mod_, "_")[[1]]
  
  dependent_ <- paste0("delisted_in_next",
                       readr::parse_number(elements_[2]),
                       "days")
  
  data_train_name <- paste0("data_train", "_",
                            ifelse(elements_[5] == "down",
                                   elements_[5],
                                   toupper(elements_[5])), "_",
                            elements_[3], "_",
                            readr::parse_number(elements_[2]),
                            "days")
    # data_test
  
  formula_name <- paste0("formula",
                         elements_[3], 
                         "_delisted_",
                         readr::parse_number(elements_[2]),
                         "days")
  
  predictors_ <- strsplit(as.character(get(formula_name)), " ~ ")[[1]][2] %>% 
    strsplit(" + ", fixed = TRUE) %>% 
    .[[1]]
  
  # dependent as 0/1
  data_train_use <- get(data_train_name) %>% data.frame()
  data_train_use[[dependent_]] <- 2 -as.numeric(data_train_use[[dependent_]])
  
  data_test_use <- data.frame(data_test)
  data_test_use[[dependent_]] <- 2 -as.numeric(data_test_use[[dependent_]])
  
  explainer_train <- 
    DALEX::explain(get(mod_),
                   data = data_train_use[,predictors_], # tylko zmienne objaśniające
                   y = data_train_use[[dependent_]], # zmienna objaśniana
                   label = paste(c("train", elements_[4:5]), collapse = "_"))
  
  explainer_test <- 
    DALEX::explain(get(mod_),
                   data = data_test_use[,predictors_], # tylko zmienne objaśniające
                   y = data_test_use[[dependent_]], # zmienna objaśniana
                   label = paste(c("test", elements_[4:5]), collapse = "_"))
  
  assign(x = gsub("model", "explainer_train", mod_),
         value = explainer_train)  
  
  assign(x = gsub("model", "explainer_test", mod_),
         value = explainer_test)  
  
  rm(elements_, dependent_, predictors_, 
     data_train_name, formula_name,
     data_train_use, data_test_use,
     explainer_train, explainer_test)
  
  gc()
}

explainers_train <- ls(pattern = "explainer_train")
explainers_test <- ls(pattern = "explainer_test")

save(list = c(explainers_train, explainers_test),
     file = "explainers_all.RData")


# variable importance

for (explainer_ in c(explainers_train, explainers_test)) {
  
  message(explainer_)
  
  vip_  <- model_parts(get(explainer_), N = NULL, type = "raw")
  
  # N = number of observations that should be sampled 
  # for calculation of variable importance.
  # If NULL then variable importance will be calculated 
  # on whole dataset (no sampling).
  
    assign(x = gsub("explainer", "vip", explainer_),
         value = vip_)  
  
    rm(vip_)
    gc()
}

vips_train <- ls(pattern = "vip_train")
vips_test <- ls(pattern = "vip_test")

save(list = c(vips_train, vips_test),
     file = "vips_all.RData")


plot(vip_test_days28_1_rf_smote)
plot(vip_test_days28_1a_rf_smote)
plot(vip_test_days28_2_rf_smote)

plot(vip_test_days28_1_rf_down)
plot(vip_test_days28_1a_rf_down)
plot(vip_test_days28_2_rf_down)

# 10 most important predictors for the best model

dir.create("xai_plots")

for (vip_ in c(vips_train, vips_test)) {
  
  print(vip_)
  
  importance_full_model <- 
    get(vip_) %>% 
    data.frame() %>% 
    filter(permutation == 0,
           variable  ==  "_full_model_") %>% 
    dplyr::select(dropout_loss) %>% 
    pull()
  
  vip_PW <- 
    get(vip_) %>% 
    data.frame() %>% 
    filter(permutation > 0,
           !variable  %in% c("_full_model_", "_baseline_")) %>% 
    group_by(variable) %>% 
    summarise(importance = mean(dropout_loss, na.rm = TRUE)) %>% 
    arrange(-importance) %>% 
    mutate(importance = importance - importance_full_model) 
    
  
  # wykres
  
  vip_PW %>% 
    mutate(label = as.factor(variable),
           variable = forcats::fct_reorder(variable, importance)
    ) %>% 
    ggplot(aes(y = variable,
               x = importance)) +
    geom_col(fill = "darkblue") + 
    theme_bw() +
    #ggtitle(paste0("Variable importance")) +
    theme(legend.position = "none") -> var_importance

  ggsave(plot = var_importance,
         filename = paste0("xai_plots/", vip_, ".png"),
         width = 8,
         height = 5)
  
  rm(importance_full_model, vip_PW)
}


# Baseline is the change in model performance when ALL variables are permuted







# PDPs

load("explainers_all.RData")

rm(pdp_all)

for (explainer_ in c(explainers_train, explainers_test)) {
  
  message(explainer_)
  
  elements_ <- strsplit(explainer_, "_")[[1]]
  
  dependent_ <- paste0("delisted_in_next",
                       readr::parse_number(elements_[3]),
                       "days")
  
  formula_name <- paste0("formula",
                         elements_[4], 
                         "_delisted_",
                         readr::parse_number(elements_[3]),
                         "days")
  
  predictors_ <- strsplit(as.character(get(formula_name)), " ~ ")[[1]][2] %>% 
    strsplit(" + ", fixed = TRUE) %>% 
    .[[1]]
  
  for(independent_variable in predictors_) {
    
    pdp_  <- model_profile(get(explainer_),
                           N = NULL,
                           variables =  independent_variable)$agr_profiles %>% 
      data.frame() %>% 
      dplyr::select(-X_ids_)
    
    names(pdp_)[1:4] <- c("indep_var", "model", 
                          "X", "yhat")
    
    
    pdp_$specification <- parse_number(elements_[2])
    
    pdp_$algorithm <- gsub(parse_number(elements_[2]), "",
                           elements_[2])
    
    
    if(!exists("pdp_all")) pdp_all <- pdp_ else
      pdp_all <- rbind(pdp_all, pdp_)
    
    rm(explainer_, pdp_)
    
  } # end of loop for independent_variable
  rm(elements_, spec_, dependent_, predictors_)
  
} # end of loop for mod_

save(list = "pdp_all",
     file = "pdp_full_data_all_specs_50_50.RData")

load("pdp_full_data_all_specs_50_50.RData")


#--------------------------------------------
# dla poszczególnych zmiennych wykresy PDP


for (mod_ in modele) {
  
  print(mod_)
  
  elements_ <- strsplit(mod_, "_")[[1]]
  
  spec_ <- parse_number(elements_[2])
  
  dependent_ <- as.character(get(paste0("spec", 
                                        spec_)))[2]
  
  predictors_ <- strsplit(as.character(get(paste0("spec", spec_)))[3],
                          " + ", fixed = TRUE)[[1]]
  
  for(indep_var_ in predictors_) {
    
    message(indep_var_)
    
    pdp_ <- 
      pdp_all %>% 
      filter(specification == spec_,
             indep_var == indep_var_) %>% 
      mutate(X = as.numeric(X))
    
    if(nrow(pdp_) == 0) next
    
    pdp_ %>% 
      ggplot(aes(x = X,
                 y = yhat,
                 group = algorithm,
                 col = algorithm)) +
      geom_line() + 
      theme_bw() +
      labs(x = indep_var_,
           y = "average dependent") -> pdp_plot
    
    ggsave(plot = pdp_plot,
           filename = paste0("xai_plots/", parse_number(mod_),
                             "/pdp_indepvar=",
                             indep_var_,
                             "_full_sample_50_50.png"),
           width = 8,
           height = 5)
    
  } # end of loop for indep_var_ 
  
} # end of loop for modele






