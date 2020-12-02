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


#initialize a new project-local environment with a private R library
renv::init()

#read in data files
df <- read_csv('fake_job_postings.csv')

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



#snapshot packages
renv::snapshot()

