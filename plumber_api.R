## ---- api-setup
library(plumber)
library(xgboost)

# Load model
model <- readRDS("xgb_model.RDS")

#* @apiTitle faking job postings API
#* @apiDescription Endpoints for working with fake job postings dataset

## ---- post-data
#* Submit data and get a prediction in return
#* @param req the request object
#* @post /predict
#* @serializer html
function(req, res) {
  data <- dplyr::as_tibble(eq$postBody)
  
  if (is.null(data)) {
    res$status <- 400
    return(list(error = "No data submitted"))
  }
  
prediction_df <- predict(model, data)%>%
   bind_cols(data %>% select_all()) 

return(prediction_df)
}


