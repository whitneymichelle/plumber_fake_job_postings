library(plumber)
library(xgboost)
library(tidyverse)
library(tidymodels)

#* @apiTitle Fake Job Postings API
model <- readRDS("D:/fake_job_postings_project/xgb_model.RDS")
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


predict(model, model_data) %>% mutate(.pred_class = case_when(.pred_class == 1 ~ "fake job posting",
                                      .pred_class == 0 ~ "real job posting"))
    

}


