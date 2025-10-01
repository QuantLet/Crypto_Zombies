rm(list = ls(all = TRUE))
library(xts)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(ggplot2)
library(fBasics)
library(stringr)

setwd("C:/Users/ptwoj/Dropbox/__biezace/2024-02-07_BBS_zombie_po_recenzjach/")

files_ <- list.files(pattern = "summary_all_algos_")
files_ <- files_[str_detect(files_, ".RData")]

files_ <- files_[-1]

for (file_ in files_) {
  
  load(file_)
  
  summary_all_algos$model <- ifelse(length(grep("formula2", file_)) == 1,
                                    2, ifelse(length(grep("formula1a", file_)) == 1,
                                              "1a", "1"))
  summary_all_algos$dependent <- ifelse(length(grep("7days", file_)) == 0,
                                        "days28", "days7")
  
  if(!exists("summary_all")) summary_all <- summary_all_algos else {
    summary_all <- rbind(summary_all, summary_all_algos)}
  
  rm(summary_all_algos)
  
}


files_2 <- list.files(pattern = "summary_xgb")
files_2 <- files_2[str_detect(files_2, ".RData")]


# file_ = files_2[1]

for (file_ in files_2) {
  
  load(file_)
  
  summary_xgb$dependent <- ifelse(length(grep("7days", file_)) == 0,
                                        "days28", "days7")
  
  summary_xgb <- summary_xgb %>% 
    dplyr::select(all_of(names(summary_all)))
  
  if(!exists("summary_all_xgb")) summary_all_xgb <- summary_xgb else {
    summary_all_xgb <- rbind(summary_all_xgb, summary_xgb)}
  
  rm(summary_xgb)
  
}


table(summary_all$end, 
      summary_all$sample)

table(summary_all_xgb$end, 
      summary_all_xgb$sample)

# !!!!! tu przez pomyłkę dla test wpisywałem też train_end 
# to na szczęście zawsze pół roku później :)

summary_all_xgb <-
  summary_all_xgb %>% 
  mutate(end = ifelse(sample == "test", 
                      as.character(ceiling_date(as_date(end) + days(1) + months(5), "month") - 1),
                      end))
  
# zbieram razem 

summary_all_test <-
  rbind(summary_all, summary_all_xgb) %>% 
  filter(sample == "test") %>% 
  mutate(sample = ifelse(end != "2022-12-31", "validation", "test"))

table(summary_all_test$end,
      summary_all_test$sample)

which(substr(summary_all_test$hyperparams, 1, 6) == "75, 1,")

summary_all_test <- summary_all_test[-which(summary_all_test$hyperparams == "75, 1, 50"),]

table(summary_all_test$end,
      summary_all_test$sample)

table(summary_all_test$algorithm,
      summary_all_test$sample)



# summary in the validation sample (average for 7 subperiods)

check_ <-
summary_all_test %>% 
  group_by(sample, resampling, algorithm, hyperparams, model, dependent) %>% 
  summarise(n = n(),
            Accuracy = mean(Accuracy, na.rm = TRUE),
            Sensitivity = mean(Sensitivity, na.rm = TRUE),
            Specificity = mean(Specificity, na.rm = TRUE),
            Precision = mean(Pos.Pred.Value, na.rm = TRUE),
            F1 = mean(F1, na.rm = TRUE),
            Balanced_Accuracy = mean(Balanced.Accuracy, na.rm = TRUE),
            AUROC = mean(AUROC, na.rm = TRUE),
            miss_Accuracy = sum(is.na(Accuracy)),
            miss_Sensitivity = sum(is.na(Sensitivity)),
            miss_Specificity = sum(is.na(Specificity)),
            miss_Precision = sum(is.na(Precision)),
            miss_F1 = sum(is.na(F1)),
            miss_Balanced_Accuracy = sum(is.na(Balanced_Accuracy)),
            miss_AUROC = sum(is.na(AUROC))
            ) %>% 
  ungroup() %>% 
  mutate(wariant_id = paste(dependent,
                            "model", model,
                            "algorithm", algorithm,
                            "hyper", hyperparams,
                            "resampling",resampling,
                            sep = "_")
         )

# best in validation by type

check2 <- check_ %>% 
  group_by(sample, model, dependent, resampling, algorithm) %>% 
  arrange(-Balanced_Accuracy) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  tidyr::pivot_wider(id_cols = c(wariant_id, dependent, model,
                                      algorithm, hyperparams, resampling),
                     names_from = sample,
                     values_from = c(Accuracy:AUROC, rank))
                    

# to wide by wariant_id

writexl::write_xlsx(check2,
                    "summary_all_algos_and_models_with_xgb.xlsx")




