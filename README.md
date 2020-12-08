
# Fake Job Posting Prediction Model & Plumber API
#### Primary objectives: 
- show cleaning text data for predictive model
- show running multiple models and comparing performance
- show deploying model to an api using plumber package

The dataset used for the analysis is located here: https://www.kaggle.com/shivamb/real-or-fake-fake-jobposting-prediction

Because of the size of the data file, only a 10 percent sample of the data is shown in the repository here: `fake_job_postings_sample.csv`

## Requirements

### Libraries
- renv
- tidyverse
- skimr
- janitor
- tidymodels
- textrecipes
- themis
- stopwords
- tidytext
- bench
- xgboost
- openxlsx
- vip
- randomForest
- probably
- plumber
- yaml

### Model and Plumber Files

- `model_iterations.R` file contains script of cleaning dataframe and function to run multiple models with their default parameters in tidymodels

- `model_iterations_output/` folder contains the confusion matrices, roc_auc metrics/curves, important variable plots, and CSVs of test dataframe with prediction probabilities for the models as well as a CSV of performance across different probabilities for the xgboost model. Models are not saved as R objects in the repository; however, the script does save both models that are created and the final chosen model needs to be loaded to the `plumber_api_files/plumber_api.R` file to build the api.

- `plumber_api_files/` folder contains 
   - `plumber_api.R`: sets up structure of the api
   - `plumber_api_settings.yaml`: defines parameters of the api, and openapi can be further customized
   - `plumber_entrypoint.R` : reads in `plumber_api.R` file and `plumber_api_settings.yaml` to run api

## Instructions
1. Run `model_iterations.R` file, which saves two models. Choose the model that performs best based on your criteria
2. Load chosen model into `plumber_api_files/plumber_api.R`
3. Check/update/edit api settings`plumber_api_files/plumber_api_setting.R` to your specifications but will work as-is.
4. Run `plumber_entrypoint.R`, which runs the api in your localhost

## R Session Information

Session information in `renv.lock file`. Use `renv\activate.R` to activate repository environment.



