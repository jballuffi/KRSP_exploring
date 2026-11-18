

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(brms)
library(tidybayes)

#read in finalized data
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")



# control density models ----------------------------------------------------------

#formula for growth rate
d_GR <- bf(
  growthrate_std ~ cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#formula for litter size
d_LS <- bf(
  littersize_std ~ cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#create a different model for each treatment
density_low_model <- brm(
  formula = d_GR + d_LS,
  data = controls[density_cat == "low"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

density_medium_model <- brm(
  formula = d_GR + d_LS,
  data = controls[density_cat == "medium"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

density_high_model <- brm(
  formula = d_GR + d_LS,
  data = controls[density_cat == "high"],
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

summary(density_low_model)
summary(density_medium_model)
summary(density_high_model)



# Food models -------------------------------------------------------------

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




# cache models ------------------------------------------------------------

#write out model formulas
c_GR <- bf(
  growthrate_std ~ density_std + cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

c_LS <- bf(
  littersize_std ~ density_std + cones_std
  + mother_age_std + mother_age2_std + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

cache_small_model <- brm(
  formula = c_GR + c_LS,
  data = cache[cache_cat == "small"],
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

cache_medium_model <- brm(
  formula = c_GR + c_LS,
  data = cache[cache_cat == "medium"],
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

cache_large_model <- brm(
  formula = c_GR + c_LS,
  data = cache[cache_cat == "large"],
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)



# rattle models -----------------------------------------------------------

#First test if there is a difference in chickadees and controls
r_GR <- bf(
  growthrate_std ~ density_std + cones_std
  + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

r_LS <- bf(
  littersize_std ~ density_std + cones_std
  + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

rattle_rattle_model <- brm(
  formula = r_GR + r_LS,
  data = rattle[treatment_gr == "Rattle"],
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

rattle_control_model <- brm(
  formula = r_GR + r_LS,
  data = rattle[treatment_gr == "Control"],
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

# rattle_chickadee_model <- brm(
#   formula = r_GR + r_LS,
#   data = rattle[treatment_ind == "Chickadee"],
#   chains = chain,
#   cores = core,
#   iter = iters,
#   warmup = warms
# )

summary(rattle_control_model)
summary(rattle_rattle_model)



# save alternative models in separate folder ------------------------------

saveRDS(density_low_model, "Output/Models/alternative_models/density_low.rds")
saveRDS(density_medium_model, "Output/Models/alternative_models/density_medium.rds")
saveRDS(density_high_model, "Output/Models/alternative_models/density_high.rds")

saveRDS(food_control_model, "Output/Models/alternative_models/food_control.rds")
saveRDS(food_bucket_model, "Output/Models/alternative_models/food_bucket.rds")
saveRDS(food_nobucket_model, "Output/Models/alternative_models/food_nobucket.rds")

saveRDS(cache_small_model, "Output/Models/alternative_models/cache_small.rds")
saveRDS(cache_medium_model, "Output/Models/alternative_models/cache_medium.rds")
saveRDS(cache_large_model, "Output/Models/alternative_models/cache_large.rds")

saveRDS(rattle_rattle_model, "Output/Models/alternative_models/rattle_rattle.rds")
saveRDS(rattle_control_model, "Output/Models/alternative_models/rattle_control.rds")
#saveRDS(rattle_chickadee_model, "Output/Models/alternative_models/rattle_chickadee.rds")



