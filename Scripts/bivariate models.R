
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
dat <- readRDS("Output/Data/final_data.rds")


# subset data for different analyses --------------------------------------

#make food treatment dataset
food <- dat[food == 0 | food == 1]  #just food add and control grids

#make cache dataset
cacheyears <- dat[!is.na(cache_prev), unique(year)]  #get years with cache size data
cache <- dat[(grid == "SU" | grid == "KL") & year %in% cacheyears]  #only long term control grids

#make rattle experiment dataset
rattleyears <- dat[food == 19, unique(year)] #get years with rattles
rattle <- dat[year %in% rattleyears & !grid == "AG"] #take all data from year of rattle playbacks but 



# test model for learning bayesian  ---------------------------------------


test_model <- brm(
  formula = growthrate_std ~ treatment_food*cones_tree_std*density_std + mother_age_std + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  data = food,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 200, #how many times are you going to sample, bella says 1000
  warmup = 50 #if model is having a hard time to converge this will help, having some is good.
  #backend = "cmdstanr" #bella uses this
)

summary(test_model)
#when I run without standardized variables the Rhats are way above 1.
#when I run with standardized variables the Rhats are all 1

plot(test_model)


conditional_effects(test_model)

posterior_epred(test_model)

pp_check(test_model)

mcmc_plot(test_model, type = "trace")



# ANALYSIS 1 -----------------------------------

#formula:
# growth rate OR litter size ~ treatment*tree cones*density + mother age + mother age^2 + sex + mast

f_continuous <- bf(
  growthrate_std ~ treatment_food*cones_tree_std*density_std + mother_age_std + mother_age_std^2 + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = gaussian())

f_ordinal <- bf(
  littersize ~ treatment_food*cones_tree_std*density_std + mother_age_std + mother_age_std^2 + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = cumulative("logit"))

#this takes 16 min on work computer and that makes sense because it's very complicated (bella)
biv_food_model <- brm(
  formula = f_continuous + f_ordinal,
  data = food,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 200, #how many times are you going to sample, bella says 1000
  warmup = 50 #if model is having a hard time to converge this will help, having some is good.
  #backend = "cmdstanr" #bella uses this
)
#total iterations = iterations - warmup


summary(biv_food_model)

saveRDS(biv_food_model, "Output/Models/foodtreatment_model.rds")

#output: posterior distribution is density plot of probabilities, distribution of probabilities
#frequentist: point estimate with standard error bars




# ANALYSIS 2 --------------------------------------------------------------

#Data from KL/SU to assess impact of hoarded cones on co-variance.

#formula:
#growth rate OR litter size ~ cones cached*tree cones*density + mother age + mother age^2 + sex + mast

#look at colinearity between cache and cones of prev years
c_continuous <- bf(
  growthrate_std ~ cache_std*cones_tree_std*density_std + mother_age_std + mother_age_std^2 + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = gaussian())

c_ordinal <- bf(
  littersize ~ cache_std*cones_tree_std*density_std + mother_age_std + mother_age_std^2 + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = cumulative("logit"))

biv_cache_model <- brm(
  formula = c_continuous + c_ordinal,
  data = cache,
  chains = 2,
  cores = 2, # Parallel processing
  iter = 200,
  warmup = 50
)

summary(biv_cache_model)
saveRDS(biv_cache_model, "Output/Models/cache_model.rds")



# ANALYSIS 3 --------------------------------------------------------------

#2018-2020 data from JO (rattle playbacks) vs. controls (BT, KL, SU) to assess social cues 
#of density on co-variance between litter size and offspring growth.

#HITTING ERRORS HERE. ISSUES WITH VARIABLES. I THINK IT'S MOTHER AGE

r_continuous <- bf(
  growthrate_std ~ treatment*cones_tree_std*density_std + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = gaussian())

r_ordinal <- bf(
  littersize ~ treatment*cones_tree_std*density_std + sex + mast 
  + (1|litter_id/mother_id) + (1|gridyear) + (1|mother_byear),
  family = cumulative("logit"))

biv_rattle_model <- brm(
  formula = r_continuous + r_ordinal,
  data = rattle,
  chains = 2,
  cores = 2, # Parallel processing
  iter = 200,
  warmup = 50
)


summary(biv_rattle_model)
saveRDS(biv_rattle_model, "Output/Models/rattle_model.rds")





# look at models ----------------------------------------------------------


fmod <- readRDS("Output/Models/foodtreatment_model.rds")
cmod <- readRDS("Output/Models/cache_model.rds")
rmod <- readRDS("Output/Models/rattle_model.rds")



summary(fmod)




