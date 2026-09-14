
#load library
library(brms)
library(bayesplot)
library(ggplot2)
library(ggeffects)
library(data.table)
library(ggpubr)
library(purrr)
library(dplyr)


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





# FOOD MODEL COV AND COR --------------------------------------------------

# Step 1: build prediction grid

fooddat <- expand.grid(
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
                      newdata = fooddat,  #whats the data we are basing the prediction on
                      re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep <- ep[, , 1]   # grab the expected growth rate
litter_ep <- ep[, , 2]   # expected littersize


# Step 3 — Compute covariance and correlation at each density × treatment (uses dplyr and purrr)

#compute covariance
cov_df <- map_dfr(seq_len(nrow(fooddat)), function(i) {
  data.frame(
    density_std = fooddat$density_std[i],
    treatment_ind = fooddat$treatment_ind[i],
    cov = cov(growth_ep[, i], litter_ep[, i])
  )
})

#compute correlation
cor_df <- map_dfr(seq_len(nrow(fooddat)), function(i) {
  data.frame(
    density_std = fooddat$density_std[i],
    treatment_ind = fooddat$treatment_ind[i],
    cor = cor(growth_ep[, i], litter_ep[, i])
  )
})


# Step 4 — Plot covariance vs. density and correlation vs. density with 3 treatment lines

#covariance
ggplot(cov_df, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)

#correlation
ggplot(cor_df, aes(density_std, cor, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)




# CACHE MODEL COV AND COR -------------------------------------------------

# Step 1: build prediction grid

#make category for cache size
cachedat[, cac]

cachedat <- expand.grid(
  density_std = seq(min(cache$density_std), max(cache$density_std), length.out = 100),
  cache_std = mean(cache$cache_std),
  cones_std = mean(cache$cones_std, na.rm = TRUE),
  mother_age_std = mean(cache$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(cache$bjulian_std),
  mast = "no",              # or the modal value in your data
  sexratio = mean(cache$sexratio),
  gridyear = NA,            # NA → population-level prediction
  mother_byear = NA
)


# Step 2 — Get expected posterior predictions

ep_cache <- posterior_epred(cache_mod,          #what's the model we are predicting off of
                      newdata = cachedat,  #whats the data we are basing the prediction on
                      re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep_cache <- ep_cache[, , 1]   # grab the expected growth rate
litter_ep_cache <- ep_cache[, , 2]   # expected littersize


# Step 3 — Compute covariance and correlation at each density × treatment (uses dplyr and purrr)

#compute covariance
cov_df_cache <- map_dfr(seq_len(nrow(cachedat)), function(i) {
  data.frame(
    density_std = cachedat$density_std[i],
    #treatment_ind = fooddat$treatment_ind[i],
    cov = cov(growth_ep_cache[, i], litter_ep_cache[, i])
  )
})

#compute correlation
cor_df_cache <- map_dfr(seq_len(nrow(cachedat)), function(i) {
  data.frame(
    density_std = cachedat$density_std[i],
    #treatment_ind = fooddat$treatment_ind[i],
    cor = cor(growth_ep_cache[, i], litter_ep_cache[, i])
  )
})


# Step 4 — Plot covariance vs. density and correlation vs. density with 3 treatment lines

#covariance
ggplot(cov_df_cache, aes(density_std, cov)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)"
  ) +
  theme_minimal(base_size = 15)

#correlation
ggplot(cor_df_cache, aes(density_std, cor)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)"
  ) +
  theme_minimal(base_size = 15)
