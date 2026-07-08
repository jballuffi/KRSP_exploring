
library(brms)


food_mod <- readRDS("Output/Models/foodtreatment_model.rds")
cache_mod <- readRDS("Output/Models/cache_model.rds")
rattle_mod <- readRDS("Output/Models/rattle_model.rds")




summary(food_mod)

conditional_effects(food_mod)



plot(food_mod, variable = c("b_density_std", "b_treatment_food"))

plot(conditional_effects(fit1, effects = "zBase:Trt"))

plot(food_mod)
