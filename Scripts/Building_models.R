
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
dat <- readRDS("Output/Data/full_cleaned_juvs.rds")
controls <- readRDS("Output/Data/control_cleaned_juvs.rds")
food <- readRDS("Output/Data/food_cleaned_juvs.rds")
cache <- readRDS("Output/Data/cache_cleaned_juvs.rds")



######### NOTES

# z-score within grid year and random effect grid year (change the family)
# log transformed and random effect grid year (change the family)
# don't transform
# just transform litter size as log

# do we want litters of 0? Is that even possible to add?
# right now growth rate uses the gamma distribution and litter size uses the gaussian distribution

#family option 1:
# for growth rate: family = Gamma(link = "log"))
# for litter size: littersize | trunc(lb = 1) ~ AND family = gaussian(link = "identity")

#family option 2:
# both gaussian

#alternative families for litter size: 
# family = com_poisson(link = "log") #Conway-Maxwell-Poisson distribution for under dispersion
# family = cumulative("logit")

# litter size is using the same data as growth rates
# covariance between an individual and the group it came from
# litter size of 8 has 8 data points
# do we remove litter id from random effect?

# cumulative for litter and gaussian for growth rate

########



# ANALYSIS 1 -----------------------------------

#formula:
# growth rate OR litter size ~ treatment*tree cones*density + mother age + mother age^2 + birth date + sex + mast

f_continuous <- bf(
  growthrate ~ treatment_food*density_std 
  + mother_age_std + mother_age_std^2 + bjulian_std + sex + mast 
  + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
  family = Gamma(link = "log"))

f_ordinal <- bf(
  littersize ~ treatment_food*density_std 
  + mother_age_std + mother_age_std^2 + bjulian_std + mast
  + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
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
#total iterations = iterations - warmup


summary(biv_food_model)

#output: posterior distribution is density plot of probabilities, distribution of probabilities
#frequentist: point estimate with standard error bars




# ANALYSIS 2 --------------------------------------------------------------

#Data from KL/SU to assess impact of hoarded cones on co-variance.

#formula:
#growth rate OR litter size ~ cones cached*tree cones*density + mother age + mother age^2 + sex + mast

#look at colinearity between cache and cones of prev years
c_continuous <- bf(
  growthrate ~ cache_std*cones_tree_std*density_std 
  + mother_age_std + mother_age_std^2 + bjulian_std + sex + mast 
  + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
  family = Gamma(link = "log"))

c_ordinal <- bf(
  littersize | trunc(lb = 1) ~ cache_std*cones_tree_std*density_std 
  + mother_age_std + mother_age_std^2 + bjulian_std + mast 
  + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
  family = gaussian(link = "identity"))

biv_cache_model <- brm(
  formula = c_continuous + c_ordinal,
  data = cache,
  chains = 2,
  cores = 2, # Parallel processing
  iter = 200,
  warmup = 50
)

summary(biv_cache_model)



# ANALYSIS 3 --------------------------------------------------------------

#2018-2020 data from JO (rattle playbacks) vs. controls (BT, KL, SU) to assess social cues 
#of density on co-variance between litter size and offspring growth.


# r_continuous <- bf(
#   growthrate ~ treatment*cones_tree_std*density_std 
#   + mother_age_std + mother_age_std^2 + bjulian_std + sex + mast 
#   + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
#   family = Gamma(link = "log"))
# 
# r_ordinal <- bf(
#   littersize | trunc(lb = 1) ~ treatment*cones_tree_std*density_std 
#   + mother_age_std + mother_age_std^2 + bjulian_std + mast 
#   + (1|mother_id/litter_id) + (1|gridyear) + (1|mother_byear),
#   family = gaussian(link = "identity"))
# 
# biv_rattle_model <- brm(
#   formula = r_continuous + r_ordinal,
#   data = rattle,
#   chains = 2,
#   cores = 2, # Parallel processing
#   iter = 200,
#   warmup = 50
# )
# 
# 
# summary(biv_rattle_model)



# save models -------------------------------------------------------------

saveRDS(biv_food_model, "Output/Models/foodtreatment_model.rds")
saveRDS(biv_cache_model, "Output/Models/cache_model.rds")
#saveRDS(biv_rattle_model, "Output/Models/rattle_model.rds")


