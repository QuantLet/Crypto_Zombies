
rm(list = ls(all = TRUE))
library(xts)
library(lubridate)
library(data.table)
library(tidyr)
library(ggplot2)
library(fBasics)
library(caret)
library(ranger)
library(DMwR)
library(stringr)
library(dplyr)

setwd("C:/Users/ptwoj/Dropbox/__biezace/2024-02-07_BBS_zombie_po_recenzjach/")

all_weekly_snapshots <- fread("_cryptos_weekly_snapshots/all_symbols.csv")

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

# I update info about the first quotation
# (if a symbol observed earlier than the date_added info)

coins_info_all3 <- coins_info_all3 %>% 
  mutate(date_added2 = if_else(date_dif < 0, as_date(date), date_added))

# final combining of the data

all_weekly_snapshots3 <- all_weekly_snapshots2 %>% 
  left_join(coins_info_all3 %>% 
              dplyr::select(-date_added, - date), 
            by = join_by(id, symbol, slug))

# create a column "age"

all_weekly_snapshots3$age_in_days <-
  as.numeric(as_date(all_weekly_snapshots3$date) - all_weekly_snapshots3$date_added2)

all_weekly_snapshots3$date_added2 <- NULL
all_weekly_snapshots3$date_dif <- NULL

all_weekly_snapshots3$category <- as.factor(all_weekly_snapshots3$category)

# load summary function

source("F_summary_binary_with_AUROC.R")

predictors_all <- names(all_weekly_snapshots3)[25:100]

predictors_all2 <- names(all_weekly_snapshots3)[c(25:104, 107:109)]

if( class(all_weekly_snapshots3$delisted_in_next7days) != "factor") {
  
  all_weekly_snapshots3$delisted_in_next7days <- 
    factor(all_weekly_snapshots3$delisted_in_next7days,
           levels = c(1, 0),
           labels = c("Yes", "No"))
}

table(all_weekly_snapshots3$delisted_in_next7days)

formula_delisted_7days <- as.formula(paste0("delisted_in_next7days ~ ",
                                             paste0(predictors_all, collapse = " + ")))

# !!!!!!!!!!! dodać inne cechy jakościowe !!!!!

formula2_delisted_7days <- as.formula(paste0("delisted_in_next7days ~ ",
                                              paste0(predictors_all2, collapse = " + ")))

# training control without cross-validation (all results saved)

my_tr_control <- trainControl(method = "none",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

parameters_lasso <- expand.grid(alpha = 1,
                                lambda = exp(log(10)*seq(-4, 6, length.out = 50)))

parameters_rf_down <- expand.grid(mtry = seq(5, 70, 5),
                                  splitrule = "gini",
                                  min.node.size = c(50, 100, 200, 400))


train_end <- "2018-12-31"
# train_end <- as_date("2019-06-30") 
# train_end <- "2021-12-31"

rm(summary_all_algos)

for (train_end in c("2018-12-31",
                    "2019-06-30",
                    "2019-12-31",
                    "2020-06-30",
                    "2020-12-31",
                    "2021-06-30",
                    "2021-12-31",
                    "2022-06-30")) {
  
  message(train_end)
  
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
  
  set.seed(987654321)
  
  data_train_downsampled <- SMOTE(delisted_in_next7days ~ .,
                                  data = data_train %>% 
                                    dplyr::select(delisted_in_next7days,
                                                  all_of(predictors_all2)))   
  
 
  #-----------------------------------------------------
  print("glm")
  
  model_glm_train_down <- train(formula2_delisted_7days,
                                data = data_train_downsampled,
                                method = "glm",
                                preProcess = c("center", "scale"),
                                trControl = my_tr_control) # tu już BEZ down !!!
  
  summary_train_glm <- 
    summary_binary(predicted_probs = predict(model_glm_train_down, 
                                             newdata = data_train,
                                             type = "prob")$Yes,
                   real = data_train$delisted_in_next7days,
                   level_positive = "Yes", 
                   cutoff = 0.5,
                   level_negative = "No") %>% 
    mutate(sample = "train",
           resampling = "smote",
           end = train_end,
           algorithm = "glm",
           hyperparams = "")
  
  summary_test_glm_down <- 
    summary_binary(predicted_probs = predict(model_glm_train_down, 
                                             newdata = data_test,
                                             type = "prob")$Yes,
                   real = data_test$delisted_in_next7days,
                   level_positive = "Yes", 
                   cutoff = 0.5,
                   level_negative = "No") %>% 
    mutate(sample = "test",
           resampling = "smote",
           end = as.character(test_end),
           algorithm = "glm",
           hyperparams = "")
  
  if(!exists("summary_all_algos")) {
    summary_all_algos <- rbind(summary_train_glm,
                               summary_test_glm_down) 
  } else {
    summary_all_algos <- rbind(summary_all_algos,
                               summary_train_glm,
                               summary_test_glm_down) 
    }
  
  rm(summary_train_glm, summary_test_glm, summary_test_glm_down,
     model_glm_train, model_glm_train_down)
  
  #-----------------------------------------------------
  print("lasso")
  
  for(i_row in 1:nrow(parameters_lasso)) {
  
    set.seed(987654321)

    model_lasso_train_down <- train(formula2_delisted_7days,
                               data = data_train_downsampled,
                               method = "glmnet",
                               preProcess = c("center", "scale"),
                               tuneGrid = parameters_lasso[i_row,],
                               trControl = my_tr_control) # tu już BEZ down !!!
    
    summary_train_lasso <- 
      summary_binary(predicted_probs = predict(model_lasso_train_down, 
                                               newdata = data_train,
                                               type = "prob")$Yes,
                     real = data_train$delisted_in_next7days,
                     level_positive = "Yes", 
                     cutoff = 0.5,
                     level_negative = "No") %>% 
      mutate(sample = "train",
             resampling = "smote",
             end = train_end,
             algorithm = "lasso",
             hyperparams = paste0(model_lasso_train_down$bestTune, collapse = ", "))
    
    summary_test_lasso_down <- 
      summary_binary(predicted_probs = predict(model_lasso_train_down, 
                                               newdata = data_test,
                                               type = "prob")$Yes,
                     real = data_test$delisted_in_next7days,
                     level_positive = "Yes", 
                     cutoff = 0.5,
                     level_negative = "No") %>% 
      mutate(sample = "test",
             resampling = "smote",
             end = as.character(test_end),
             algorithm = "lasso",
             hyperparams = paste0(model_lasso_train_down$bestTune, collapse = ", "))
    
    summary_all_algos <- rbind(summary_all_algos,
                               summary_train_lasso,
                               summary_test_lasso_down)
    
    rm(model_lasso_train,
       model_lasso_train_down,
       summary_train_lasso,
       summary_test_lasso_down)          
    
    gc()
    
  } # end of loop for i_row in LASSO
  
  #-----------------------------------------------------
  print("random forest")
  
  for(i_row in 1:nrow(parameters_rf_down)) {
  
    set.seed(987654321)
    
    model_rf_train_down <- train(formula2_delisted_7days,
                                 data = data_train_downsampled,
                                 method = "ranger",
                                 num.trees = 200,
                                 preProcess = c("center", "scale"),
                                 tuneGrid = parameters_rf_down[i_row,],
                                 trControl = my_tr_control)
    
    summary_train_rf <- 
      summary_binary(predicted_probs = predict(model_rf_train_down, 
                                               newdata = data_train,
                                               type = "prob")$Yes,
                     real = data_train$delisted_in_next7days,
                     level_positive = "Yes", 
                     cutoff = 0.5,
                     level_negative = "No") %>% 
      mutate(sample = "train",
             resampling = "smote",
             end = train_end,
             algorithm = "rf",
             hyperparams = paste0(model_rf_train_down$bestTune, collapse = ", "))
    
    summary_test_rf_down <- 
      summary_binary(predicted_probs = predict(model_rf_train_down, 
                                               newdata = data_test,
                                               type = "prob")$Yes,
                     real = data_test$delisted_in_next7days,
                     level_positive = "Yes", 
                     cutoff = 0.5,
                     level_negative = "No") %>% 
      mutate(sample = "test",
             resampling = "smote",
             end = as.character(test_end),
             algorithm = "rf",
             hyperparams = paste0(model_rf_train_down$bestTune, collapse = ", "))
    
    
    summary_all_algos <- rbind(summary_all_algos,
                               summary_train_rf,
                               summary_test_rf_down)
    
    rm(model_rf_train,
          model_rf_train_down,
          summary_train_rf,
          summary_test_rf_down)    
    
    gc()
    
  } # end of loop for i_row in rf
  
  save(list = "summary_all_algos",
       file = "summary_all_algos_smote_formula2_7days.RData")
  
    rm(train_start, test_start, test_end,
     data_train, data_test)
  
} # end of loop for train_end

writexl::write_xlsx(summary_all_algos,
                    "summary_all_algos_smote_formula2_7days.xlsx")
