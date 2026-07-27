
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
dat <- readRDS("Output/Data/final_data_litters.rds")



# subset data for different analyses --------------------------------------

#make a control only dataset
controls <- dat[grid == "SU" | grid == "KL"]

#make food treatment data set, All food add except the weird two years in KL
food <- dat[food == 1 & !grid == "KL"]  #just food add

#make cache data set
cacheyears <- dat[!is.na(cache_prev), unique(year)]  #get years with cache size data
cache <- dat[(grid == "SU" | grid == "KL") & year %in% cacheyears]  #only long term control grids

#make rattle experiment dataset
rattleyears <- dat[food == 19, unique(year)] #get years with rattles
rattle <- dat[year %in% rattleyears & !grid == "AG"] #take all data from year of rattle playbacks but 



# ANALYSIS 1: CONTROL DATA -----------------------------------






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
  iter = 200, #how many times are you going to sample, bella says 1000
  warmup = 50 #if model is having a hard time to converge this will help, having some is good.
  
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
  iter = 200,
  warmup = 50
)

summary(biv_cache_model)



saveRDS(biv_food_model, "Output/Models/foodtreatment_model_litters.rds")
saveRDS(biv_cache_model, "Output/Models/cache_model_litters.rds")

