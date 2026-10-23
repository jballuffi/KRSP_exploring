
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
  
  #replace characters with more concise versions
  table[, Response := gsub("growthrate", "GR", Response)]
  table[, Response := gsub("littersize", "LS", Response)]
  table[, Effect := gsub("_std", "", Effect)]
  table[, Effect := gsub("treatment_indNoBucket", "NoBucket", Effect)]
  table[, Effect := gsub("treatment_indBucket", "Bucket", Effect)]
  table[, Effect := gsub("cache_catmedium", "Cache_Med", Effect)]
  table[, Effect := gsub("cache_catlarge", "Cache_large", Effect)]
  table[, Effect := gsub("treatment_grRattle", "Rattle", Effect)]
  table[, Effect := gsub("treatment_indChickadee", "Chickadee", Effect)] #only for chickadee test
  table[, Effect := gsub("treatment_indRattle", "Rattle", Effect)] #only for chickadee test
  
  #create a significance col based on credible intervals
  table[(`l-95% CI` > 0 & `u-95% CI` > 0) | (`l-95% CI` < 0 & `u-95% CI` < 0), Sig := "yes"]
  table[is.na(Sig), Sig := "-"]
  
  #take cols of interest for final table, in the right order
  table2 <- table[, .(Type, Response, Effect, Estimate, CI = paste0("(", `l-95% CI`, ", ", `u-95% CI`, ")"), Sig)]
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

