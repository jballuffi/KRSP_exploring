
#load library
library(data.table)
library(brms)
library(tidybayes)
library(dplyr)
library(ggplot2)




# control model -----------------------------------------------------------

controls <- readRDS("Output/Data/control_cleaned_litters.rds")

#get line predicting correlation
corline <- lm(growthrate ~ littersize, controls)
int <- corline$coefficients[1] #intercept
slope <- corline$coefficients[2] #slope

#plot correlation using intercept and slope
ggplot(controls)+
  geom_point(aes(x = littersize, y = growthrate))+
  geom_abline(aes(intercept = int, slope = slope))

#based on line, make growth rate predictions
controls[, growthrate_pred := littersize*slope + int]

#get residuals based on those predicted values and actual values
controls[, growthrate_resid := growthrate - growthrate_pred]

#create model structure
baytest <- bf(
  growthrate_resid ~ density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#run model
baytest_model <- brm(
  formula = baytest,
  data = controls,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 5000, #how many times are you going to sample, bella says 1000
  warmup = 500 #if model is having a hard time to converge this will help, having some is good.
  
)

#summarize and plot
summary(baytest_model)
conditional_effects(baytest_model)



# now on food model -------------------------------------------------------

food <- readRDS("Output/Data/food_cleaned_litters.rds")

#get line predicting correlation
corlineF <- lm(growthrate ~ littersize, food)
intF <- corlineF$coefficients[1]
slopeF <- corlineF$coefficients[2]

#plot correlation using intercept and slope
ggplot(food)+
  geom_point(aes(x = littersize, y = growthrate))+
  geom_abline(aes(intercept = intF, slope = slopeF))

#based on line, make growth rate predictions
food[, growthrate_pred := littersize*slopeF + intF]

#get residuals based on those predicted values and actual values
food[, growthrate_resid := growthrate - growthrate_pred]

#make bayesian model
food_test <- bf(
  growthrate_resid ~ treatment_ind*density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())

#run model
foodtest_model <- brm(
  formula = food_test,
  data = food,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 5000, #how many times are you going to sample, bella says 1000
  warmup = 500 #if model is having a hard time to converge this will help, having some is good.
  
)

summary(foodtest_model)
conditional_effects(foodtest_model)





