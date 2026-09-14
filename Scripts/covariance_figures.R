
#load library
library(brms)
library(bayesplot)
library(ggplot2)
library(ggeffects)
library(data.table)
library(ggpubr)
library(purrr)
library(dplyr)


#rescor = residual correlation parameter



# read in data and models ------------------------------------------------------------

#read in finalized data
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")
rattle_mod <- readRDS("Output/Models/rattle_model_litters.rds")



# Control model ---------------------------------------------

newdat <- data.frame(
  density_std = seq(min(controls$density_std), max(controls$density_std), length.out = 100),
  cones_std = mean(controls$cones_std, na.rm = TRUE),
  mother_age_std = mean(controls$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(controls$bjulian_std, na.rm = TRUE),
  mast = "no",
  sexratio = mean(controls$sexratio),
  gridyear = "SU-1999",
  mother_byear = 1997
)

#used control model to predict based on the new data
pred <- posterior_epred(control_mod, newdata = newdat)

#Take different predictions
pred_growth <- pred[,,1]     # growthrate predictions
pred_litter <- pred[,,2]     # littersize predictions


cov_by_x_full <- map_dfr(seq_len(ncol(pred_growth)), function(i) {
  
  g <- pred_growth[, i]
  l <- pred_litter[, i]
  
  tibble(
    density_std = newdat$density_std[i],
    cov = g * l   # product of draws approximates covariance structure
  )
})

cov_summary <- cov_by_x_full %>%
  group_by(density_std) %>%
  summarise(
    median = median(cov),
    lower = quantile(cov, 0.1),
    upper = quantile(cov, 0.9)
  )

#plot covariance
ggplot(cov_summary, aes(density_std, median)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance (80% CI)"
  ) +
  theme_minimal(base_size = 14)





# Food Model --------------------------------------------------------------






# Step 1: build prediction grid


newdat <- expand.grid(
  density_std = seq(min(food$density_std), max(food$density_std), length.out = 100),
  treatment_ind = levels(food$treatment_ind),
  cones_std = mean(food$cones_std, na.rm = TRUE),
  mother_age_std = mean(food$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(food$bjulian_std),
  mast = "no",              # or the modal value in your data
  sexratio = mean(food$sexratio),
  gridyear = NA,            # NA → population-level prediction
  mother_byear = NA
)



# Step 2 — Get expected posterior predictions

ep <- posterior_epred(food_mod,          #what's the model we are predicting off of
                      newdata = newdat,  #whats the data we are basing the prediction on
                      re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep <- ep[, , 1]   # grab the expected growth rate
litter_ep <- ep[, , 2]   # expected littersize



# Step 3 — Compute covariance at each density × treatment (uses dplyr and purrr)

cov_df <- map_dfr(seq_len(nrow(newdat)), function(i) {
  data.frame(
    density_std = newdat$density_std[i],
    treatment_ind = newdat$treatment_ind[i],
    cov = cov(growth_ep[, i], litter_ep[, i])
  )
})


# Step 4 — Plot covariance vs. density, with 3 treatment lines

ggplot(cov_df, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)




