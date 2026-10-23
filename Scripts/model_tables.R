
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
  c_fixed <- as.data.table(round(c$fixed, 2), 2)
  
  #split up response and effect columns
  c_fixed[, c("Response", "Effect") := tstrsplit(rn, "std_", fixed = TRUE)][, rn := NULL]
  c_fixed[, Type := "Fixed"]
  
  #get first random effect
  c_random1 <- as.data.table(round(c$random$gridyear, 2), 2)
  c_random1[, Effect := "gridyear"][, Response := gsub("std_Intercept", "", rn)][, rn := NULL]
  c_random1[, Type := "Random"]
  
  #get second random effect
  c_random2 <- as.data.table(round(c$random$mother_byear, 2), 2)
  c_random2[, Effect := "mother_byear"][, Response := gsub("std_Intercept", "", rn)][, rn := NULL]
  c_random2[, Type := "Random"]
  
  #get covariance 
  c_cov <- as.data.table(round(c$rescor_pars, 2))
  c_cov[, Effect := "rescor"][, Response := "rescor"]
  c_cov[, Type := "Random"]
  
  #rbind all four tables
  table <- rbind(c_fixed, c_random1, c_random2, c_cov, fill = TRUE)
  
  #replace growth rate and litter size with abrv. and remove _std from Effects
  table[, Response := gsub("growthrate", "GR", Response)]
  table[, Response := gsub("littersize", "LS", Response)]
  table[, Effect := gsub("_std", "", Effect)]
  
  table[(`l-95% CI` > 0 & `u-95% CI` > 0) | (`l-95% CI` < 0 & `u-95% CI` < 0), Sig := "yes"]
  table[is.na(Sig), Sig := "-"]
  
  table2 <- table[, .(Type, Response, Effect, Estimate, CI = paste0("(", `l-95% CI`, ", ", `u-95% CI`, ")"), Sig, Rhat)]
  setorder(table2, Response)

  return(table2)
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

