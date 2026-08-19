
#load library
library(data.table)
library(brms)
library(tidybayes)
library(dplyr)

# library(cmdstanr)
# check_cmdstan_toolchain()
# install_cmdstan(cores = 2)
# set_cmdstan_path(PATH_TO_CMDSTAN)


#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")



# ANALYSIS 1: CONTROL DATA -----------------------------------

b_continuous <- bf(
  growthrate ~ density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

b_ordinal <- bf(
  littersize ~ density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

biv_control_model <- brm(
  formula = b_continuous + b_ordinal,
  data = controls,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 1500, #how many times are you going to sample, bella says 1000
  warmup = 500 #if model is having a hard time to converge this will help, having some is good.
  
)

summary(biv_control_model)



# ANALYSIS 2: FOOD ADD DATA -----------------------------------------------

f_continuous <- bf(
  growthrate ~ treatment_food*density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

f_ordinal <- bf(
  littersize ~ treatment_food*density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#this takes 16 min on work computer and that makes sense because it's very complicated (bella)
biv_food_model <- brm(
  formula = f_continuous + f_ordinal,
  data = food,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 1500, #how many times are you going to sample, bella says 1000
  warmup = 500 #if model is having a hard time to converge this will help, having some is good.
  
)

summary(biv_food_model)




# ANALYSIS 3: CONE CACHE DATA --------------------------------------------------------------

#Data from KL/SU to assess impact of hoarded cones on co-variance.

c_continuous <- bf(
  growthrate ~ cache_std*density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

c_ordinal <- bf(
  littersize ~ cache_std*density_std + cones_tree_prev
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

biv_cache_model <- brm(
  formula = c_continuous + c_ordinal,
  data = cache,
  chains = 2,
  cores = 2, # Parallel processing
  iter = 1500,
  warmup = 500
)

summary(biv_cache_model)


saveRDS(biv_control_model, "Output/Models/control_model_litters.rds")
saveRDS(biv_food_model, "Output/Models/foodtreatment_model_litters.rds")
saveRDS(biv_cache_model, "Output/Models/cache_model_litters.rds")

