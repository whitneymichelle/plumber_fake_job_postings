# Fake Job Posting Prediction Model
#### Primary objectives: 
- show cleaning text data for predictive model
- show running multiple models and comparing performance
- show deploying model to an api using plumber package

The dataset used for the analysis is located here: https://www.kaggle.com/shivamb/real-or-fake-fake-jobposting-prediction

## Requirements

### Libraries

### From the Repository

- `model_iterations.R` file: contains script of cleaning dataframe and function to run multiple models with their default settings in tidymodels
- `model_iterations_output/` folder: contains the confusion matrices, roc_auc metrics/curves, important variable plots, and CSVs of test dataframe with prediction probabilities 
    for the models as well as a CSV of performance across different probabilities for the xgboost model
- `plumber_api' file: contains script to turn model into api

## R Session Information

-

