
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
library(data.table)
library(lubridate)

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
  

######----------------------
# load explainers

load("explainers_all.RData")

load("vips_all.RData")

vips_train <- ls(pattern = "vip_train")
vips_test <- ls(pattern = "vip_test")

# only needed

vips_rf_ROSE <- c(vips_train, vips_test) %>% 
  grep("rf_rose", ., value = TRUE) %>% 
  grep("test", ., value = TRUE) 

# 10 most important predictors for the best model

dir.create("xai_plots")

for (vip_ in vips_rf_ROSE) {
  
  print(vip_)
  
  spec_ <- strsplit(vip_, "_")[[1]][4]
  if (spec_ == "2") spec_ <- "3"
  if (spec_ == "1a") spec_ <- "2"
  
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
    mutate(importance = importance - importance_full_model) %>% 
    head(10) %>% 
    mutate(variable = gsub("_vol_close", "",
                           gsub("_roll", "", variable)))
            
  # wykres
  
  vip_PW %>% 
    mutate(label = as.factor(variable),
           variable = forcats::fct_reorder(variable, importance)
    ) %>% 
    ggplot(aes(y = variable,
               x = importance)) +
    geom_col(fill = "darkblue") + 
    theme_bw() +
    labs(y = "") +
    ggtitle(paste0("specification ", spec_)) +
    theme(legend.position = "none") -> var_importance

  ggsave(plot = var_importance,
         filename = paste0("xai_plots/", vip_, "_top10.png"),
         width = 8,
         height = 5)
  
  assign(paste0("plot_", vip_), 
         var_importance)
  
  rm(importance_full_model, vip_PW)
}


# Baseline is the change in model performance when ALL variables are permuted

#-----------------------------------------------------------
# PDPs

load("explainers_all.RData")

explainers_test <- ls(pattern = "explainer_test")

explainers_test_rose <- explainers_test %>% 
  grep("_rose", ., value = TRUE) %>% 
  grep("28", ., value = TRUE) 

predictors_check <- c("volume_roll_min182",
                      "volume_roll_min28",
                      "volume_roll_max182",
                      "r_roll_median182",
                      "category")

rm(pdp_all)

for (explainer_ in explainers_test_rose) {
  
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
  
  for(independent_variable in predictors_check) {
    
    print(independent_variable)
    
    if(elements_[4] == "1" & independent_variable == "category") next
    
    pdp_  <- model_profile(get(explainer_),
                           N = 10000,
                           variables =  independent_variable)$agr_profiles %>% 
      data.frame() %>% 
      dplyr::select(-X_ids_)
    
    names(pdp_)[1:4] <- c("indep_var", "model", 
                          "X", "yhat")
    
    
    pdp_$specification <- elements_[4]
    
    pdp_$algorithm <- elements_[5]
    
    
    if(!exists("pdp_all")) pdp_all <- pdp_ else
      pdp_all <- rbind(pdp_all, pdp_)
    
    rm(pdp_)
    
  } # end of loop for independent_variable
  rm(elements_, dependent_, formula_name)
  
} # end of loop for mod_

save(list = "pdp_all",
     file = "pdp_full_test_28days_ROSE.RData")

load("pdp_full_test_28days_ROSE.RData")


#--------------------------------------------
# dla poszczególnych zmiennych wykresy PDP

for (explainer_ in explainers_test_rose) {
  
  message(explainer_)
  
  elements_ <- strsplit(explainer_, "_")[[1]]
  
  dependent_ <- paste0("delisted_in_next",
                       readr::parse_number(elements_[3]),
                       "days")
  
  spec_ <- elements_[4]
  
  for(indep_var_ in predictors_check) {
    
    print(indep_var_)
    
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
      scale_color_brewer(palette = "Set1") +
      labs(x = gsub("_roll", "", indep_var_) %>% 
             ifelse(str_sub(., 1, 3) == "vol",
                    paste0("log_", .),
                    .),
           y = "average dependent")  +
      theme(legend.position = "bottom") -> pdp_plot
    
    ggsave(plot = pdp_plot,
           filename = paste0("xai_plots/spec_", spec_,
                             "_pdp_indepvar=",
                             indep_var_,
                             "_test.png"),
           width = 4,
           height = 3)
    
  } # end of loop for indep_var_ 
  
} # end of loop for modele


#------------------------------------------------
# combine PDP 4 plots for specification 1
# (for the others shapes are very similar)

for(indep_var_ in predictors_check[1:4]) {
  
  print(indep_var_)
  
  pdp_ <- 
    pdp_all %>% 
    filter(specification == "1",
           indep_var == indep_var_) %>% 
    mutate(X = as.numeric(X))
  
  pdp_ %>% 
    ggplot(aes(x = X,
               y = yhat,
               group = algorithm,
               col = algorithm)) +
    geom_line() + 
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    labs(x = gsub("_roll", "", indep_var_) %>% 
           ifelse(str_sub(., 1, 3) == "vol",
                  paste0("log_", .),
                  .),
         y = "average dependent")  +
    theme(legend.position = "bottom") -> pdp_plot
  
  assign(paste0("pdp_plot_", indep_var_), pdp_plot)
  
} # end of loop for indep_var_ 

# https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/

get_only_legend <- function(plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(plot))  
  
  #  Mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

legenda <- get_only_legend(pdp_plot_r_roll_median182)  

combined_plot <- 
  gridExtra::grid.arrange(pdp_plot_volume_roll_min182 +
                            theme(legend.position = "none"),
                          pdp_plot_volume_roll_min28 + 
                            theme(legend.position = "none"),
                          pdp_plot_volume_roll_max182 + 
                            theme(legend.position = "none"),
                          pdp_plot_r_roll_median182 +
                            theme(legend.position = "none"))

combined_plot2 <- 
  gridExtra::grid.arrange(combined_plot, 
                        legenda, 
                        nrow = 2, 
                        heights = c(10, 1))
 
ggsave(plot = combined_plot2,
       filename = paste0("xai_plots/PDP_plots_1_28days_test.png"),
       width = 8,
       height = 6)


# for category

indep_var_ <- "category"
  
  pdp_ <- 
    pdp_all %>% 
    filter(specification == "1a",
           indep_var == indep_var_) 
  
  pdp_ %>% 
    ggplot(aes(x = X,
               y = yhat,
               group = algorithm,
               fill = algorithm)) +
    geom_bar(position = "dodge", stat = "identity") + 
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    labs(x = "category",
         y = "average dependent")  +
    theme(legend.position = "bottom") -> pdp_plot
  
    ggsave(plot = pdp_plot,
         filename = paste0("xai_plots/PDP_plot_1_28days_category.png"),
         width = 6,
         height = 4)
  
#-----------
# combine importance plots for 28 days - specs 1, 1a, 2
    
combined_plot_vip <- 
      gridExtra::grid.arrange(plot_vip_test_days28_1_rf_rose,
                              plot_vip_test_days28_1a_rf_rose,
                              plot_vip_test_days28_2_rf_rose)
    
ggsave(plot = combined_plot_vip,
       filename = paste0("xai_plots/VIP_plot_3specs_28days.png"),
       width = 8,
       height = 8)
    

    