library(plumber)

pr <- plumb("plumber_api_files/plumber_api.R")

pr$run(swagger = function(pr, spec, ...) {
  spec <- yaml::read_yaml("plumber_api_files/plumber_api_settings.yaml")
  return(spec)
})





