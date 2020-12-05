library(plumber)
library(randomForest)

#* @apiTitle Fake Job Postings API

model <- readRDS("rf_model.RDS")

#* Log some information about incoming request
#* @filter logger

function(req) {
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", request$REMOTE_ADDR, "\n")
  
  #forward the request
  forward()
}

#* Parse and predict on model for future endpoints
#* @filter predict
function(req, res) {
  
  if(str_detect(req$PATH_INFO, "predict")) {
        req$predict_data <- as_tibble(req$postBody)
      
    if(is.na(req$predict_data)) {
      res$status <- 400
      return(list(error = "No data included in request body."))
    }
    
  else #predict values based in postBody and store in req
        req$predicted_values(model, req$predict_data)
        
  }
  
   #forward the request
  forward()
}

#* Predict whether fradulent job posting
#* @post predict/values
function(req) {
  req$predicted_values
}

#* predicted values in HTML table
#* @html
#* @post
function(req, res) {
  table_data <- colu
}


#* Return whether job posting if fake 
#* @param title text
#* @param company_profile text
#* @param description text
#* @param requirements text
#* @param telecommuting numeric (1 = yes, 0 = no)
#* @param has_company_logo numeric (1 = yes, 0 = no)
#* @param has_questions numeric (1 = yes, 0 = no)
#* @param employment_type factor (Other, Full-time, unknown, Contract, Temporary, Part-time)
#* @param required_experience factor (Internship, Not Applicable, unknown, Mid-Senior level Associate, 
#* Entry level, Executive, Director)
#* @param required_education (unknown, Bachelor's Degree, Master's Degree, High School or equivalent,
#* Unspecified, Some College Coursework Completed, Vocational, Certification, Associate Degree, Professional,
#* Doctorate, Some High School Coursework, Vocational - Degree, Vocational - HS Diploma, Some High School Coursework)
#* @param department_present numeric (1 = yes, 0 = no)
#* @param salary_range_present numeric (1 = yes, 0 = no)
#* @param benefits_present numeric (1 = yes, 0 = no)
#* @param industry_present numeric (1 = yes, 0 = no)
#* @param function_present numeric (1 = yes, 0 = no)
#* @param country_present numeric (1 = yes, 0 = no)
#* @param state_present numeric (1 = yes, 0 = no)
#* @param city_present numeric (1 = yes, 0 = no)
#* @post /predict
function(title, company_profile, description, requirements, telecommuting, has_company_logo, has_questions,
         employment_type, required_experience, required_education, department_present, salary_range_present, 
         benefits_present, industry_present, function_present, country_present, 
         state_present, city_present) {

    model_data <- dplyr::tibble(
      title = as.character(title),
      company_profile = as.character(company_profile),
      description = as.character(description),
      requirements = as.character(requirements),
      telecommuting = as.numeric(telecommuting),
      has_company_logo = as.numeric(has_company_logo),
      has_questions = as.numeric(has_questions),
      employment_type = as_factor(employment_type),
      required_experience = as_factor(required_experience),
      required_education = as_factor(required_education),
      department_present = as.numeric(department_present),
      salary_range_present = as.numeric(salary_range_present),
      benefits_present = as.numeric(benefits_present),
      industry_present = as.numeric(industry_present),
      function_present = as.numeric(function_present),
      country_present = as.numeric(country_present),
      state_present = as.numeric(state_present),
      city_present = as.numeric(city_present)
    )

predictions <-  predict(model, model_data) %>% case_when(.pred = )

return(predictions)

}



plumb(file='plumber.R')$run()