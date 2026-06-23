

#source the R folder to load any packages and functions
#lapply(dir('R', '*.R', full.names = TRUE), source)

library(data.table)

#read in finalized data
dat <- readRDS("Output/Data/final_data.rds")

#subset for different analyses
food <- dat[food == 0 | food == 1]  #just food add and control grids


#in this script I am using brm package to run the bivariate response mixed model. It uses Bayesian stats I believe.
#brm requires Rtools45 and this was a headache to figure out. I was stuck in a "Stan-loop" whatever that means
#it's now installed on my work laptop we will see how it works.
#alternative approach is to use the MCMCglmm package that delaney cited

#library(MCMCglmm) #used by delaney, bayesian approach
#library(mcglm) #frequentist approach



# ANALYSIS 1 -----------------------------------

#formula:
# growth rate OR litter size ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast + (1|mother_id) + (1|litter_id) + (1|year)

#basic frequentest approach
#using base R to get an idea of things but can't use random effects with cbind
biv_lm <- lm(cbind(littersize, growthrate) ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast, data = food)
summary(biv_lm)


#Bayesian approach
library(brms) 

f_continuous <- bf(
  growthrate ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast 
  + (1|mother_id) + (1|litter_id) + (1|year), 
  family = gaussian())

f_ordinal <- bf(
  littersize ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast 
  + (1|mother_id) + (1|litter_id) + (1|year), 
  family = cumulative("logit"))

bivariate_mixed_model <- brm(
  formula = f_continuous + f_ordinal,
  data = food,
  chains = 2,
  cores = 2, # Parallel processing
  iter = 200,
  warmup = 100
  #,backend = "cmdstanr"
)


summary(bivariate_mixed_model)




# ANALYSIS 2 --------------------------------------------------------------

#All long-term data from KL/SU to assess impact of hoarded cones on co-variance between litter size and offspring growth.
#Fixed effects include part date, conestm-1, local density, female age, female age^2, grid, local density x conestm1. 
#Random effects: Grid/Year/Female/Litter. Not including part date as it is inconsistently available.






# ANALYSIS 3 --------------------------------------------------------------

#2018-2020 data from JO (rattle playbacks) vs. controls (BT, KL, SU) to assess social cues of density on co-variance between litter size and offspring growth.
#Fixed effects include part date, conestm-1, local density, playbacks heard, female age, female age^2, grid, local density x conestm1, playbacks heard x conestm1. 
#Random effects: Grid/Year/Female/Litter. Not including part date as it is inconsistently available.





