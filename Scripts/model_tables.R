
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")
rattle_mod <- readRDS("Output/Models/rattle_model_litters.rds")
rattle_mod_test <- readRDS("Output/Models/rattle_model_litters_TEST.rds")


#function to pull results from models and compile into a table

make_table_brms <- function(mod){
  
  #make summary of model
  c <- summary(mod)
  
  #get fixed effect table
  c_fixed <- as.data.table(c$fixed, 2)
  
  #split up response and effect columns
  c_fixed[, c("Response", "Effect") := tstrsplit(rn, "std_", fixed = TRUE)][, rn := NULL]
  
  #get first random effect
  c_random1 <- as.data.table(c$random$gridyear, 2)
  c_random1[, Effect := "gridyear"][, Response := gsub("std_Intercept", "", rn)][, rn := NULL]
  
  #get second random effect
  c_random2 <- as.data.table(c$random$mother_byear, 2)
  c_random2[, Effect := "mother_byear"][, Response := gsub("std_Intercept", "", rn)][, rn := NULL]
  
  #get covariance 
  c_cov <- as.data.table(c$rescor_pars)
  c_cov[, Response := "rescor"]
  
  #rbind all four tables
  table <- rbind(c_fixed, c_random1, c_random2, c_cov, fill = TRUE)
  
  setcolorder(table, c("Response", "Effect"))
  setorder(table, "Response")
  
  return(table)
  
}



control_table <- make_table_brms(control_mod)
food_table <- make_table_brms(food_mod)
cache_table <- make_table_brms(cache_mod)
rattle_table <- make_table_brms(rattle_mod)
rattle_test_table <- make_table_brms(rattle_mod_test)


# save --------------------------------------------------------------------

write.csv(control_table, "Output/Tables/control_model_table.csv")
write.csv(food_table, "Output/Tables/food_model_table.csv")
write.csv(cache_table, "Output/Tables/cache_model_table.csv")
write.csv(rattle_table, "Output/Tables/rattle_model_table.csv")
write.csv(rattle_test_table, "Output/Tables/rattle_model_TEST_table.csv")

