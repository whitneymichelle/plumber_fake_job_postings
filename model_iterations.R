#set working directory
#setwd("D:/fake_job_postings_project")

#clean env
rm(list = ls())

#load libraries
library(renv)
library(tidyverse)
library(skimr)
library(janitor)
library(tidymodels)
library(textrecipes)
library(themis)
library(stopwords)
library(tidytext)
library(bench)
library(xgboost)
library(openxlsx)
library(vip)
library(randomForest)


#initialize a new project-local environment with a private R library
#renv::init()

#read in data files
df <- read_csv("fake_job_postings.csv")

#modifying data frame
df2 <- df %>% 
      separate(location, c("country", "state", "city")) %>%
      select(-job_id) %>% 
      mutate(department_present = case_when(is.na(department) ~ 0,
                                        TRUE ~ 1)) %>%
      mutate(salary_range_present = case_when( (is.na(salary_range) | str_detect(salary_range, "[:alpha:]"))~0,
                                          TRUE ~ 1)) %>%
      mutate(benefits_present = case_when(is.na(benefits)~0,
                                           TRUE ~ 1)) %>%
      mutate(industry_present = case_when(is.na(industry)~0,
                                      TRUE ~ 1)) %>%
      mutate(function_present = case_when(is.na(`function`)~0,
                                      TRUE ~ 1)) %>%
      mutate(country_present = case_when(str_detect(country, "[:alpha:]") ~ 1,
                                                    TRUE ~ 0))%>%
      mutate(state_present = case_when(str_detect(state, "[:alpha:]") ~ 1,
                                                TRUE ~ 0))%>%
      mutate(city_present = case_when(str_detect(city, "[:alpha:]") ~ 1,
                                              TRUE ~ 0))%>%
      mutate_all(na_if, "") %>%
      mutate_all(na_if, "NA")%>%
      mutate_if(is.character, list(~replace_na(., "unknown"))) %>%
      mutate(fraudulent = forcats::fct_rev(as.character(fraudulent))) %>% #factor conversion needed for step_smote in recipe section
      mutate(employment_type = as_factor(employment_type), required_experience = as_factor(required_experience),
             required_education = as_factor(required_education))%>%
      select(-department, -salary_range, -benefits, -industry, -`function`, -country, -state, -city)%>%
      mutate_if(is.character, str_trim)%>% #get rid of white space in some character columns
      clean_names()

#split data into test and training sets
set.seed(345)
split <- initial_split(df2, prop = .80)
train_df <- training(split)
test_df <- testing(split)

#clean data column names
test_df <- test_df
train_df <- train_df

#create recipe and roles--pre processing for the model
recipe <- recipe(fraudulent ~ ., data = train_df) %>%
  step_tokenize(title, company_profile, description, requirements) %>% # Tokenizes to words by default
  step_stopwords(title, company_profile, description, requirements) %>% 
  step_tokenfilter(title, company_profile, description, requirements, max_tokens = 400) %>%
  step_tf(title, company_profile, description, requirements)%>%
  step_dummy(all_predictors(), -all_numeric()) %>%
  step_normalize(all_predictors()) %>%
  step_smote(fraudulent) #generate new examples of the minority class using nearest neighbors 


#multiple model function
mod_iter <- function(spec, engine) {
  
  #set model parameters
  mod <-
    spec() %>%
    set_engine(engine) %>%
    set_mode("classification")
  
  #workflow
  set.seed(345)
  wf <- workflow() %>%
    add_model(mod) %>%
    add_recipe(recipe)
  
  #fit model
  fit <- wf %>%
    fit(data = train_df)
  
  #predict on test data
  fraud_predict_p <- predict(fit, test_df, type = "prob") %>% 
    bind_cols(test_df %>% select_all()) 
 
  fraud_predict_c <- predict(fit, test_df, type = "class") %>% 
    bind_cols(test_df %>% select_all()) 
   
  #roc curve
  roc_curve <- fraud_predict_p %>% 
    roc_curve(truth = fraudulent, .pred_1) %>% 
    autoplot()
  
  #roc auc
  roc_auc <- fraud_predict_p %>% 
    roc_auc(truth = fraudulent, .pred_1)
  
  #confusion matrix
  conf_matrix <- fraud_predict_c %>%
    conf_mat(fraudulent, .pred_class)
  
  
  # variable importance plot
  vip_img <- fit %>%
    pull_workflow_fit() %>%
    vip(num_features = 30)
  
  return(list(fit, fraud_predict, roc_curve, roc_auc, vip_img, conf_matrix))
  
}


spec<- c(boost_tree, rand_forest)
engine <- c('xgboost', 'randomForest')

results <-  bench::mark(check = FALSE,
                        models_info <- purrr::pmap(list(spec,engine), mod_iter))


saveRDS(results, "benchmark_results.RDS")

#compare AUC values, ranger higher
models_info[[1]][[4]]#.972
models_info[[2]][[4]]#.992

saveRDS(models_info[[2]][[1]], "rf_model.RDS")
saveRDS(models_info[[2]][[6]], "rf_conf_mat.RDS")
write_csv(models_info[[2]][[2]], "rf_test_df_predictions.csv" )
ggsave("rf_roc_curve_plot.png", models_info[[2]][[3]])
ggsave("rf_vip_plot.png", models_info[[2]][[5]])


saveRDS(models_info[[1]][[1]], "xgb_model.RDS")
saveRDS(models_info[[1]][[6]], "xgb_conf_mat.RDS")
write_csv(models_info[[1]][[2]], "xgb_test_df_predictions.csv" )
ggsave("xgb_roc_curve_plot.png", models_info[[1]][[3]])
ggsave("xgb_vip_plot.png", models_info[[1]][[5]])


#snapshot packages
renv::snapshot()

