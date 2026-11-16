

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(brms)
library(tidybayes)

# library(cmdstanr)
# check_cmdstan_toolchain()
# install_cmdstan(cores = 2)
# set_cmdstan_path(PATH_TO_CMDSTAN)


#read in finalized data
food <- readRDS("Output/Data/food_cleaned_litters.rds")



#write out model formulas
f_GR <- bf(
  growthrate_std ~ density_std + cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

f_LS <- bf(
  littersize_std ~ density_std + cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())


#create a different model for each treatment
food_control_model <- brm(
  formula = f_GR + f_LS,
  data = food[treatment_ind == "Control"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

food_bucket_model <- brm(
  formula = f_GR + f_LS,
  data = food[treatment_ind == "Bucket"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

food_nobucket_model <- brm(
  formula = f_GR + f_LS,
  data = food[treatment_ind == "No Bucket"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

summary(food_control_model)
summary(food_bucket_model)
summary(food_nobucket_model)

saveRDS(food_control_model, "Output/Models/alternative_models/food_control.rds")
saveRDS(food_bucket_model, "Output/Models/alternative_models/food_bucket.rds")
saveRDS(food_nobucket_model, "Output/Models/alternative_models/food_nobucket.rds")

