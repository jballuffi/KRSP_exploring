

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in finalized data
dat <- readRDS("Output/Data/final_data.rds")

#subset for different analyses
food <- dat[food == 0 | food == 1]  #just food add and control grids


#library(MCMCglmm) #used by delaney, bayesian approach
#library(mcglm) #frequentist approach
library(brms) #bayesian approach


#formula:
# growth rate OR litter size ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast + (1|mother_id) + (1|litter_id) + (1|year)



# frequentist and not mixed -----------------------------------------------

#using base R to get an idea of things but can't use random effects with cbind
biv_lm <- lm(cbind(littersize, growthrate) ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast, data = food)

# 3. View the full summary
summary(biv_lm)




# bayesian approach with random effects -----------------------------------


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
  chains = 4,
  cores = 4, # Parallel processing
  iter = 3000,
  warmup = 1500
)


summary(bivariate_mixed_model)

# biv_brm <- brm(
#   mvbind(littersize, growthrate) ~ treatment_food*cones_tree_prev*density + mother_age + mother_age^2 + sex + mast + (1 | mother_id), 
#                data = food)










