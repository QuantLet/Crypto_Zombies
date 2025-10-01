
rm(list = ls(all = TRUE))
library(xts)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(ggplot2)
library(fBasics)
library(caret)
library(ranger)
library(DMwR)

all_weekly_snapshots <- fread("_cryptos_weekly_snapshots/all_symbols.csv")


#--------------------------------------------------
# transformations based on statistics

all_weekly_snapshots2 <- all_weekly_snapshots

statistics_all_variables_PW <- 
  readxl::read_xlsx("statistics_all_variables_df_PW.xlsx", sheet = 1)

variables_to_log <- statistics_all_variables_PW %>% 
  filter(log == 1) %>% 
  select(statystyka) %>% 
  pull()

for (variable_ in variables_to_log) {
  message(variable_)
  all_weekly_snapshots2[[variable_]] <- log(all_weekly_snapshots2[[variable_]] + 1)
}

# load summary function

source("F_summary_binary_with_AUROC.R")

predictors_all <- names(all_weekly_snapshots2)[-1:-24]


all_weekly_snapshots2$delisted_in_next28days <- 
  factor(all_weekly_snapshots2$delisted_in_next28days,
         levels = c(1, 0),
         labels = c("Yes", "No"))

table(all_weekly_snapshots2$delisted_in_next28days)

formula_delisted_28days <- as.formula(paste0("delisted_in_next28days ~ ",
                                             paste0(predictors_all, collapse = " + ")))

# !!!!!!!!!!! dodać inne cechy jakościowe !!!!!

# source("mySummary.R")

# training control without cross-validation (all results saved)

my_tr_control <- trainControl(method = "none",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

parameters_lasso <- expand.grid(alpha = 1,
                                lambda = exp(log(10)*seq(-4, 6, length.out = 50)))

parameters_rf_down <- expand.grid(mtry = seq(5, 70, 5),
                                  splitrule = "gini",
                                  min.node.size = c(50, 100, 200))


#train_end <- as_date("2018-12-31")
#train_end <- as_date("2019-06-30") 
train_end <- "2021-12-31"

rm(summary_all_algos)

for (train_end in c(#"2018-12-31",
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
  
  data_train <- all_weekly_snapshots2 %>% 
    filter(between(as_date(date), train_start, as_date(train_end)))
  
  data_test <- all_weekly_snapshots2 %>% 
    filter(between(as_date(date), test_start, test_end))
  
  data_train <- na.omit(data_train)
  data_test <- na.omit(data_test)
  
  # downsampled training data
  
  set.seed(987654321)
  
  data_train_downsampled <- SMOTE(delisted_in_next28days ~ .,
                                  data = data_train %>% 
                                    dplyr::select(-delisted_in_next7days,
                                                  -delisted_in_next14days,
                                                  -delisted_in_next21days,
                                                  -c(1:12, 17:24)))   
  
 
  #-----------------------------------------------------
  print("glm")
  
  model_glm_train_down <- train(formula_delisted_28days,
                                data = data_train_downsampled,
                                method = "glm",
                                preProcess = c("center", "scale"),
                                trControl = my_tr_control) # tu już BEZ down !!!
  
  summary_train_glm <- 
    summary_binary(predicted_probs = predict(model_glm_train_down, 
                                             newdata = data_train,
                                             type = "prob")$Yes,
                   real = data_train$delisted_in_next28days,
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
                   real = data_test$delisted_in_next28days,
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

    model_lasso_train_down <- train(formula_delisted_28days,
                               data = data_train_downsampled,
                               method = "glmnet",
                               preProcess = c("center", "scale"),
                               tuneGrid = parameters_lasso[i_row,],
                               trControl = my_tr_control) # tu już BEZ down !!!
    
    summary_train_lasso <- 
      summary_binary(predicted_probs = predict(model_lasso_train_down, 
                                               newdata = data_train,
                                               type = "prob")$Yes,
                     real = data_train$delisted_in_next28days,
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
                     real = data_test$delisted_in_next28days,
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
    
    model_rf_train_down <- train(formula_delisted_28days,
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
                     real = data_train$delisted_in_next28days,
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
                     real = data_test$delisted_in_next28days,
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
       file = "summary_all_algos_smote.RData")
  
    rm(train_start, test_start, test_end,
     data_train, data_test)
  
} # end of loop for train_end

writexl::write_xlsx(summary_all_algos,
                    "summary_all_algos_smote.xlsx")
