
#script for visualizing the covariance and correlation in response to treatments and density

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(brms)
library(bayesplot)
library(purrr)



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



# Create data sets to base predictions off of ------------------------------

#use function to make three diff data sets
controldat <- make_dataset(x = controls, t = controls$treatment_ind)
fooddat <- make_dataset(x = food, t = food$treatment_ind)
cachedat <- make_dataset(x = cache, t = cache$cache_cat)
rattdat <- make_dataset(x = rattle, t = rattle$treatment_ind)

#rename treatment col in cache dataset
setnames(cachedat, "treatment_ind", "cache_cat")



# Control model covariance and correlation predictions --------------------------------------------------

#Make an array of the expected posterior predictions
ep_control <- posterior_epred(control_mod,  #the model we are using to create predictions
                           newdata = controldat,  #the data from step 1
                           re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep_control <- ep_control[, , 1]   # grab the expected growth rate from array
litter_ep_control <- ep_control[, , 2]   # expected litter size from array

#Compute covariance and correlation at each density × treatment (uses dplyr and purrr)
#compute covariance
df_control <- map_dfr(seq_len(nrow(controldat)), function(i) {
  data.frame(
    density_std = controldat$density_std[i],
    cov = cov(growth_ep_control[, i], litter_ep_control[, i]),
    cor = cor(growth_ep_control[, i], litter_ep_control[, i])
  )
})

#Plot covariance vs. density and correlation vs. density with 3 treatment lines
#covariance
(control_cov <- ggplot(df_control, aes(density_std, cov)) +
    geom_line(size = 1.2) +
    labs(subtitle = "Control model", x = "", y = "", color = "Treatment") +
    theme1+
    theme(legend.position = "inside", legend.position.inside = c(0.25, 0.3))
)

#correlation
(control_cor <- ggplot(df_control, aes(density_std, cor)) +
    geom_line(size = 1.2) +
    labs(subtitle = "Control model", x = "", y = "", color = "Treatment") +
    theme1)



# Food model covariance and correlation predictions --------------------------------------------------

#Make an array of the expected posterior predictions
ep_food <- posterior_epred(food_mod,  #the model we are using to create predictions
                      newdata = fooddat,  #the data from step 1
                      re_formula = NA)   #use this argument to drop random effects and predict for the whole population

growth_ep_food <- ep_food[, , 1]   # grab the expected growth rate from array
litter_ep_food <- ep_food[, , 2]   # expected litter size from array


#Compute covariance and correlation at each density × treatment (uses dplyr and purrr)
df_food <- map_dfr(seq_len(nrow(fooddat)), function(i) {
  data.frame(
    density_std = fooddat$density_std[i],
    treatment_ind = fooddat$treatment_ind[i],
    cov = cov(growth_ep_food[, i], litter_ep_food[, i]),
    cor = cor(growth_ep_food[, i], litter_ep_food[, i])
  )
})


#Plot covariance vs. density and correlation vs. density with 3 treatment lines
#covariance
(food_cov <- ggplot(df_food, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = treatment_ind_cols) +
  labs(subtitle = "Food model", x = "", y = "", color = "Treatment") +
  theme1+
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.3))
  )

#correlation
(food_cor <- ggplot(df_food, aes(density_std, cor, color = treatment_ind)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = treatment_ind_cols, guide = NULL) +
  labs(subtitle = "Food model", x = "", y = "", color = "Treatment") +
  theme1)



# Cache model covariance and correlation predictions -------------------------------------------------

# Get posterior predictions
ep_cache <- posterior_epred(cache_mod,          
                      newdata = cachedat,  
                      re_formula = NA)   

growth_ep_cache <- ep_cache[, , 1]   
litter_ep_cache <- ep_cache[, , 2]   


#Compute covariance and correlation at each density × cache size (uses dplyr and purrr)
df_cache <- map_dfr(seq_len(nrow(cachedat)), function(i) {
  data.frame(
    density_std = cachedat$density_std[i],
    cache_cat = cachedat$cache_cat[i],
    cov = cov(growth_ep_cache[, i], litter_ep_cache[, i]),
    cor = cor(growth_ep_cache[, i], litter_ep_cache[, i])
  )
})


#Plot covariance vs. density and correlation vs. density with 3 categories of cache size
#covariance
(cache_cov <- ggplot(df_cache, aes(density_std, cov, color = cache_cat)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = cache_cols) +
    labs(subtitle = "Cache model", x = "", y = "Posterior covariance\n(GR × LS)", color = "Cache level") +
    theme1+
    theme(legend.position = "inside", legend.position.inside = c(0.25, 0.3))
  )

#correlation
(cache_cor <- ggplot(df_cache, aes(density_std, cor, color = cache_cat)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cache_cols, guide = NULL) +
  labs(subtitle = "Cache model", x = "", y = "Posterior correlation\n(GR × LS)", color = "Cache level") +
    theme1)



# Rattle model covariance and correlation predictions -------------------------------------------------

#Get expected posterior predictions
ep_rattle <- posterior_epred(rattle_mod,
                      newdata = rattdat,
                      re_formula = NA) 

growth_ep_rattle <- ep_rattle[, , 1]  
litter_ep_rattle <- ep_rattle[, , 2] 


#Compute covariance and correlation at each density × treatment (uses dplyr and purrr)
df_rattle <- map_dfr(seq_len(nrow(rattdat)), function(i) {
  data.frame(
    density_std = rattdat$density_std[i],
    treatment_ind = rattdat$treatment_ind[i],
    cov = cov(growth_ep_rattle[, i], litter_ep_rattle[, i]),
    cor = cor(growth_ep_rattle[, i], litter_ep_rattle[, i])
  )
})

#Plot covariance vs. density and correlation vs. density with 3 treatment lines
#covariance
(rattle_cov <- ggplot(df_rattle, aes(density_std, cov, color = treatment_ind)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = treatment_ind_cols) +
  labs(subtitle = "Rattle model", x = "Density (standardized)", y = "", color = "Treatment") +
  theme1+
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.2))
  )

#correlation
(rattle_cor <- ggplot(df_rattle, aes(density_std, cor, color = treatment_ind)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = treatment_ind_cols, guide = NULL) +
  labs(subtitle = "Rattle model", x = "Density (standardized)", y = "", color = "Treatment") +
  theme1)



# merge and save ----------------------------------------------------------

#collect all cor and cov prediction figures
cor_cov_fig <- ggarrange(food_cov, food_cor, cache_cov, cache_cor, rattle_cov, rattle_cor, ncol = 2, nrow = 3, align = "hv")

ggsave("Output/Figures/covariance_correlation_predictions.jpeg", cor_cov_fig, width = 7, height = 9, unit = "in")

