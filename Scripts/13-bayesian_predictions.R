
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

#function that makes data based on a dataframe (x) and a treatment/category (t)
make_dataset <- function(x, t){
  dat <- as.data.table(expand.grid(
    density_std = seq(min(x$density_std), max(x$density_std), length.out = 100), #full range of density
    treatment_ind = levels(t),  #all treatments
    cones_std = mean(x$cones_std, na.rm = TRUE),  #full range of tree cone count
    mother_age_std = mean(x$mother_age_std, na.rm = TRUE),  #avg mother age
    bjulian_std = mean(x$bjulian_std),  #avg birth date
    mast = "no",  # predicting for a non-mast year
    sexratio = mean(x$sexratio),  #avg sex ratio
    gridyear = NA,  #Using NA because we want a population-level prediction
    mother_byear = NA  # same as grid year
    ))
  return(dat)
}

#use function to make three diff data sets
controldat <- make_dataset(x = controls, t = controls$treatment_ind)
fooddat <- make_dataset(x = food, t = food$treatment_ind)
cachedat <- make_dataset(x = cache, t = cache$cache_cat)
rattdat <- make_dataset(x = rattle, t = rattle$treatment_ind)

#rename treatment col in cache dataset
setnames(cachedat, "treatment_ind", "cache_cat")



# Food Model Ellipse ------------------------------------------------------

#Generate posterior predictions for controls
control_gr <- posterior_predict(food_mod, resp = "growthratestd", newdata = fooddat[treatment_ind == "Control"], re_formula = NA)
control_ls <- posterior_predict(food_mod, resp = "littersizestd", newdata = fooddat[treatment_ind == "Control"], re_formula = NA)
control_joint <- data.frame(
  gr_pred = rowMeans(control_gr),
  ls_pred = rowMeans(control_ls),
  treatment_ind = "Control"
)

#generate posterior predictions for buckets
bucket_gr <- posterior_predict(food_mod, resp = "growthratestd", newdata = fooddat[treatment_ind == "Bucket"], re_formula = NA)
bucket_ls <- posterior_predict(food_mod, resp = "littersizestd", newdata = fooddat[treatment_ind == "Bucket"], re_formula = NA)
bucket_joint <- data.frame(
  gr_pred = rowMeans(bucket_gr),
  ls_pred = rowMeans(bucket_ls),
  treatment_ind = "Bucket"
)

#generate posterior predictions for no buckets
nobucket_gr <- posterior_predict(food_mod, resp = "growthratestd", newdata = fooddat[treatment_ind == "No Bucket"], re_formula = NA)
nobucket_ls <- posterior_predict(food_mod, resp = "littersizestd", newdata = fooddat[treatment_ind == "No Bucket"], re_formula = NA)
nobucket_joint <- data.frame(
  gr_pred = rowMeans(nobucket_gr),
  ls_pred = rowMeans(nobucket_ls),
  treatment_ind = "No Bucket"
)


#rbind all predictions together
food_joint <- rbind(control_joint, bucket_joint, nobucket_joint)

#plot growth rate against litter size and add ellipse
(food_ellipse <- ggplot(food_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = treatment_ind), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = treatment_ind), method = "lm", fill = NA)+
    stat_ellipse(aes(color = treatment_ind), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_ind_cols) +
    labs(subtitle = "A) Food Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment") +
    theme1)



# Cache Model Ellipse -----------------------------------------------------

#Generate posterior predictions for small cache sizes
small_gr <- posterior_predict(cache_mod, resp = "growthratestd", newdata = cachedat[cache_cat == "small"], re_formula = NA)
small_ls <- posterior_predict(cache_mod, resp = "littersizestd", newdata = cachedat[cache_cat == "small"], re_formula = NA)
small_joint <- data.frame(
  gr_pred = rowMeans(small_gr),
  ls_pred = rowMeans(small_ls),
  cache_cat = "small"
)

#Generate posterior predictions for medium cache sizes
medium_gr <- posterior_predict(cache_mod, resp = "growthratestd", newdata = cachedat[cache_cat == "medium"], re_formula = NA)
medium_ls <- posterior_predict(cache_mod, resp = "littersizestd", newdata = cachedat[cache_cat == "medium"], re_formula = NA)
medium_joint <- data.frame(
  gr_pred = rowMeans(medium_gr),
  ls_pred = rowMeans(medium_ls),
  cache_cat = "medium"
)

#Generate posterior predictions for controls
large_gr <- posterior_predict(cache_mod, resp = "growthratestd", newdata = cachedat[cache_cat == "large"], re_formula = NA)
large_ls <- posterior_predict(cache_mod, resp = "littersizestd", newdata = cachedat[cache_cat == "large"], re_formula = NA)
large_joint <- data.frame(
  gr_pred = rowMeans(large_gr),
  ls_pred = rowMeans(large_ls),
  cache_cat = "large"
)

#rbind all three
cache_joint <- rbind(small_joint, medium_joint, large_joint)

#plot growth rate against litter size and add ellipse
(cache_ellipse <- ggplot(cache_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = cache_cat), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = cache_cat), method = "lm", fill = NA)+
    stat_ellipse(aes(color = cache_cat), level = 0.95, size = 1.2) +
    scale_color_manual(values = cache_cols) +
    labs(subtitle = "B) Cache Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Cache")+
    theme1)



# Rattle Model Ellipse ----------------------------------------------------------

#Generate posterior predictions for controls
controlrat_gr <- posterior_predict(rattle_mod, resp = "growthratestd", newdata = rattdat[treatment_ind == "Control"], re_formula = NA)
controlrat_ls <- posterior_predict(rattle_mod, resp = "littersizestd", newdata = rattdat[treatment_ind == "Control"], re_formula = NA)
controlrat_joint <- data.frame(
  gr_pred = rowMeans(controlrat_gr),
  ls_pred = rowMeans(controlrat_ls),
  treatment_ind = "Control"
)

#Generate posterior predictions for controls
rattle_gr <- posterior_predict(rattle_mod, resp = "growthratestd", newdata = rattdat[treatment_ind == "Rattle"], re_formula = NA)
rattle_ls <- posterior_predict(rattle_mod, resp = "littersizestd", newdata = rattdat[treatment_ind == "Rattle"], re_formula = NA)
rat_joint <- data.frame(
  gr_pred = rowMeans(rattle_gr),
  ls_pred = rowMeans(rattle_ls),
  treatment_ind = "Rattle"
)

rattle_joint <- rbind(controlrat_joint, rat_joint)

#plot growth rate against litter size and add ellipse
(rattle_ellipse <- ggplot(rattle_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = treatment_ind), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = treatment_ind), method = "lm", fill = NA)+
    stat_ellipse(aes(color = treatment_ind), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_ind_cols) +
    #lims(y = c(-1, 1), x = c(-1, 1))+
    labs(subtitle = "C) Rattle Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment")+
    theme1)




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



# compile all figures into one --------------------------------------------

#collect all ellipse figures
ellipse_fig <- ggarrange(food_ellipse, cache_ellipse, rattle_ellipse, ncol = 1, nrow = 3, align = "hv")

#collect all cor and cov prediction figures
cor_cov_fig <- ggarrange(food_cov, food_cor, cache_cov, cache_cor, rattle_cov, rattle_cor, ncol = 2, nrow = 3, align = "hv")

ggsave("Output/Figures/ellipse_predictions.jpeg", ellipse_fig, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/covariance_correlation_predictions.jpeg", cor_cov_fig, width = 7, height = 9, unit = "in")

