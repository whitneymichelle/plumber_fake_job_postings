# plumber.R

# Read model data.
model <- readRDS(file = 'ranger_model.RDS')

prepped_recipe <- model$pre$actions$recipe$recipe %>% prep()


#* @param wt
#* @param qsec
#* @param am
#* @post /predict
CalculatePrediction <- function(df){
  
inputted_data_processed <- bake(prepped_recipe, new_data = df, all_predictors())
  
fraud_predict <- predict(fit, df, type = "prob") %>% 
  bind_cols(test_df %>% select_all()) 

test_results <- 
  test_results %>%
  rename(`random forest` = .pred) %>%
  bind_cols(
    predict(glmn_fit, new_data = test_normalized) %>%
      rename(glmnet = .pred)
  )
  
  return(y.pred)
}

