#set working directory
#setwd("D:/fake_job_postings_project")

#clean env
#rm(list = ls())

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
library(ranger)
library(bench)
library(xgboost)
library(openxlsx)


#initialize a new project-local environment with a private R library
#renv::init()

#read in data files
df <- read_csv('fake_job_postings.csv')

#modifying data frame
df2 <- df %>% 
      select(job_id, title, description, requirements, fraudulent)%>%
      mutate(fraudulent = as.factor(fraudulent))%>%
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
  update_role(job_id, new_role = "id_variable") %>%
  step_tokenize(title, description, requirements) %>% # tokenizes to words by default
  step_stopwords(title, description, requirements) %>% 
  step_tokenfilter(title, description, requirements, max_tokens = 400) %>%
  step_tf(title, description, requirements)%>%
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
  fraud_predict <- predict(fit, test_df, type = "prob") %>% 
    bind_cols(test_df %>% select_all()) 
  
  #roc curve
  roc_curve <- fraud_predict %>% 
    roc_curve(truth = fraudulent, .pred_1) %>% 
    autoplot()
  
  #roc auc
  roc_auc <- fraud_predict %>% 
    roc_auc(truth = fraudulent, .pred_1)
  
  return(list(fit, fraud_predict, roc_curve, roc_auc))
  
}


spec<- c(boost_tree, logistic_reg)
engine <- c('xgboost', 'glm')

results <-  bench::mark(check = FALSE,
                        models_info <- purrr::pmap(list(spec,engine), mod_iter))

saveRDS(results, "benchmark_results.RDS")

#compare AUC values, ranger higher
models_info[[1]][[4]]#.972
models_info[[2]][[4]]#.990

saveRDS(models_info[[2]][[1]], "ranger_model.RDS")
write_csv(models_info[[2]][[2]], "test_df_predictions.csv" )
ggsave(paste0("roc_curve_plot.png"), models_info[[2]][[3]])

#snapshot packages
renv::snapshot()

