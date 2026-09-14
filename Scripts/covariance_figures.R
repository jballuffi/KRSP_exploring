
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

# Step 1: create the data to base the prediction on 

fooddat <- expand.grid(
  density_std = seq(min(food$density_std), max(food$density_std), length.out = 100), #full range of density
  treatment_ind = levels(food$treatment_ind),  #all treatments
  cones_std = mean(food$cones_std, na.rm = TRUE),  #full range of tree cone count
  mother_age_std = mean(food$mother_age_std, na.rm = TRUE),  #avg mother age
  bjulian_std = mean(food$bjulian_std),  #avg birth date
  mast = "no",  # predicting for a non-mast year
  sexratio = mean(food$sexratio),  #avg sex ratio
  gridyear = NA,  #Using NA because we want a population-level prediction
  mother_byear = NA  # same as grid year
)


# Step 2 — Make an array of the expected posterior predictions

ep_food <- posterior_epred(food_mod,  #the model we are using to create predictions
                      newdata = fooddat,  #the data from step 1
                      re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep_food <- ep_food[, , 1]   # grab the expected growth rate from array
litter_ep_food <- ep_food[, , 2]   # expected litter size from array


# Step 3 — Compute covariance and correlation at each density × treatment (uses dplyr and purrr)

#compute covariance
cov_df_food <- map_dfr(seq_len(nrow(fooddat)), function(i) {
  data.frame(
    density_std = fooddat$density_std[i],
    treatment_ind = fooddat$treatment_ind[i],
    cov = cov(growth_ep_food[, i], litter_ep_food[, i])
  )
})

#compute correlation
cor_df_food <- map_dfr(seq_len(nrow(fooddat)), function(i) {
  data.frame(
    density_std = fooddat$density_std[i],
    treatment_ind = fooddat$treatment_ind[i],
    cor = cor(growth_ep_food[, i], litter_ep_food[, i])
  )
})


# Step 4 — Plot covariance vs. density and correlation vs. density with 3 treatment lines

#covariance
ggplot(cov_df_food, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)

#correlation
ggplot(cor_df_food, aes(density_std, cor, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)




# CACHE MODEL COV AND COR -------------------------------------------------

# Step 1:

cachedat <- expand.grid(
  density_std = seq(min(cache$density_std), max(cache$density_std), length.out = 100),
  cache_cat = levels(cache$cache_cat),  #instead of treatment we are using categories of cache size
  cones_std = mean(cache$cones_std, na.rm = TRUE),
  mother_age_std = mean(cache$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(cache$bjulian_std),
  mast = "no",              
  sexratio = mean(cache$sexratio),
  gridyear = NA, 
  mother_byear = NA
)


# Step 2 — Get expected posterior predictions

ep_cache <- posterior_epred(cache_mod,          
                      newdata = cachedat,  
                      re_formula = NA)   

growth_ep_cache <- ep_cache[, , 1]   
litter_ep_cache <- ep_cache[, , 2]   


# Step 3 — Compute covariance and correlation at each density × cache size (uses dplyr and purrr)

#compute covariance
cov_df_cache <- map_dfr(seq_len(nrow(cachedat)), function(i) {
  data.frame(
    density_std = cachedat$density_std[i],
    cache_cat = cachedat$cache_cat[i],
    cov = cov(growth_ep_cache[, i], litter_ep_cache[, i])
  )
})

#compute correlation
cor_df_cache <- map_dfr(seq_len(nrow(cachedat)), function(i) {
  data.frame(
    density_std = cachedat$density_std[i],
    cache_cat = cachedat$cache_cat[i],
    cor = cor(growth_ep_cache[, i], litter_ep_cache[, i])
  )
})


# Step 4 — Plot covariance vs. density and correlation vs. density with 3 categories of cache size

#covariance
ggplot(cov_df_cache, aes(density_std, cov, color = cache_cat)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)",
    color = "Cache level"
  ) +
  theme_minimal(base_size = 15)

#correlation
ggplot(cor_df_cache, aes(density_std, cor, color = cache_cat)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)",
    color = "Cache level"
  ) +
  theme_minimal(base_size = 15)



# CACHE MODEL COV AND COR -------------------------------------------------

# Step 1: build prediction grid

rattdat <- expand.grid(
  density_std = seq(min(rattle$density_std), max(rattle$density_std), length.out = 100),
  treatment_ind = levels(rattle$treatment_ind),  #using treatment of rattle vs control
  cones_std = mean(rattle$cones_std, na.rm = TRUE),
  mother_age_std = mean(rattle$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(rattle$bjulian_std),
  mast = "no",
  sexratio = mean(rattle$sexratio),
  gridyear = NA,
  mother_byear = NA
)


# Step 2 — Get expected posterior predictions

ep_rattle <- posterior_epred(rattle_mod,
                      newdata = rattdat,
                      re_formula = NA) 

growth_ep_rattle <- ep_rattle[, , 1]  
litter_ep_rattle <- ep_rattle[, , 2] 


# Step 3 — Compute covariance and correlation at each density × treatment (uses dplyr and purrr)

#compute covariance
cov_df_rattle <- map_dfr(seq_len(nrow(rattdat)), function(i) {
  data.frame(
    density_std = rattdat$density_std[i],
    treatment_ind = rattdat$treatment_ind[i],
    cov = cov(growth_ep_rattle[, i], litter_ep_rattle[, i])
  )
})

#compute correlation
cor_df_rattle <- map_dfr(seq_len(nrow(rattdat)), function(i) {
  data.frame(
    density_std = rattdat$density_std[i],
    treatment_ind = rattdat$treatment_ind[i],
    cor = cor(growth_ep_rattle[, i], litter_ep_rattle[, i])
  )
})


# Step 4 — Plot covariance vs. density and correlation vs. density with 3 treatment lines

#covariance
ggplot(cov_df_rattle, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)

#correlation
ggplot(cor_df_rattle, aes(density_std, cor, color = treatment_ind)) +
  geom_line(size = 1.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15)

