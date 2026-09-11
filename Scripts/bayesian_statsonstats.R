
#load library
library(data.table)
library(brms)
library(tidybayes)
library(dplyr)
library(ggplot2)


controls <- readRDS("Output/Data/control_cleaned_litters.rds")



corline <- lm(growthrate ~ littersize, controls)
summary(corline)
int <- corline$coefficients[1]
slope <- corline$coefficients[2]

ggplot(controls)+
  geom_point(aes(x = littersize, y = growthrate))+
  geom_abline(aes(intercept = int, slope = slope))

controls[, growthrate_pred := littersize*slope + int]
controls[, growthrate_resid := growthrate - growthrate_pred]


baytest <- bf(
  growthrate_resid ~ density_std + cones_std
  + mother_age_std + mother_age_std^2 + bjulian_std + mast + sexratio
  + (1|gridyear) + (1|mother_byear),
  family = gaussian())


baytest_model <- brm(
  formula = baytest,
  data = controls,
  chains = 2, #are the chains fuzzy? The more chains the longer, ideal is 4. (bella)
  cores = 2, # Parallel processing, dependent on computer you are using
  iter = 5000, #how many times are you going to sample, bella says 1000
  warmup = 500 #if model is having a hard time to converge this will help, having some is good.
  
)

summary(baytest_model)
conditional_effects(baytest_model)






