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
  data <- req$postBody
  if (is.null(data)) {
    res$status <- 400
    return(list(error = "No data submitted"))
  }
  if (!class(data) %in% c("json", "tibble") ) {
    res$status <- 400
    return(list(error = "Incorect data format: needs to be a dataframe or json"))
  }
  if(class(data) == "json") {
  tryCatch(jsonlite::parse_json(req$postBody, simplifyVector = TRUE),
           error = function(e) NULL)
  }
  if(class(data) == "json") {
    tryCatch(jsonlite::parse_json(req$postBody, simplifyVector = TRUE),
             error = function(e) NULL)
  }
  
prediction_df <- predict(model, data)%>%
   bind_cols(data %>% select_all()) 

return(prediction_df)
}


