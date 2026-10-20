
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(brms)
library(bayesplot)
library(purrr)
library(sjPlot)

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")
rattle_mod <- readRDS("Output/Models/rattle_model_litters.rds")
rattle_mod_test <- readRDS("Output/Models/rattle_model_litters_TEST.rds")




summary(control_mod)
fixef(control_mod)
ranef(control_mod)

tab_model(control_mod, title = "Control Model", )


#this is what we want
c <- summary(control_mod)

#get fixed effects
fixed_tab <- round(c$fixed, 2)


#get random effects
round(c$random$gridyear, 2)
round(c$random$mother_byear, 2)

#get covariance 
round(c$rescor_pars, 2)







