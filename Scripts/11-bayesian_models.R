
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
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")



# ANALYSIS 1: CONTROL DATA -----------------------------------

#formula for growth rate
b_continuous <- bf(
  growthrate_std ~ density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#formula for litter size
b_ordinal <- bf(
  littersize_std ~ density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#run model using brm function. chains, cores, iterations, and warm ups are pre-loaded in the 'R/bayesian_values.R'
biv_control_model <- brm(
  formula = b_continuous + b_ordinal, #merge both formulas
  data = controls, #call data set
  chains = chain, #The more chains the longer it will take to run, bella said 4 chains is ideal
  cores = core, # Parallel processing, dependent on computer you are using
  iter = iters, #how many times are you going to sample
  warmup = warms #how many warm ups to help model converge
)

summary(biv_control_model)



# ANALYSIS 2: FOOD ADD DATA -----------------------------------------------

f_continuous <- bf(
  growthrate_std ~ treatment_ind*density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

f_ordinal <- bf(
  littersize_std ~ treatment_ind*density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

biv_food_model <- brm(
  formula = f_continuous + f_ordinal,
  data = food,
  chains = chain, 
  cores = core, 
  iter = iters, 
  warmup = warms 
)

summary(biv_food_model)




# ANALYSIS 3: CONE CACHE DATA --------------------------------------------------------------

#Data from KL/SU to assess impact of hoarded cones on co-variance.

c_continuous <- bf(
  growthrate_std ~ cache_cat*density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

c_ordinal <- bf(
  littersize_std ~ cache_cat*density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

biv_cache_model <- brm(
  formula = c_continuous + c_ordinal,
  data = cache,
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

summary(biv_cache_model)



# Analysis 4: Rattle Analysis ---------------------------------------------

r_continuous <- bf(
  growthrate_std ~ treatment_ind*density_std + cones_std
  + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

r_ordinal <- bf(
  littersize_std ~ treatment_ind*density_std + cones_std
  + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

biv_rattle_model <- brm(
  formula = r_continuous + r_ordinal,
  data = rattle,
  chains = chain,
  cores = core,
  iter = iters,
  warmup = warms
)

summary(biv_rattle_model)



saveRDS(biv_control_model, "Output/Models/control_model_litters.rds")
saveRDS(biv_food_model, "Output/Models/foodtreatment_model_litters.rds")
saveRDS(biv_cache_model, "Output/Models/cache_model_litters.rds")
saveRDS(biv_rattle_model, "Output/Models/rattle_model_litters.rds")
