# plumber.R

# Read model data.
model <- readRDS(file = 'ranger_model.RDS')

prepped_recipe <- model$pre$actions$recipe$recipe %>% prep()


#* @param wt
#* @param qsec
#* @param am
#* @post /predict


