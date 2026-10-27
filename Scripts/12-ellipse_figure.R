
#Script to make ellipse figures based on predictions from bayesian models

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
rattle_mod_test <- readRDS("Output/Models/rattle_model_litters_TEST.rds")



# Create data sets to base predictions off of ------------------------------

#use function from R/ folder to make four different data sets, one per bayesian model
controldat <- make_dataset(x = controls, t = controls$treatment_ind)
fooddat <- make_dataset(x = food, t = food$treatment_ind)
cachedat <- make_dataset(x = cache, t = cache$cache_cat)
rattdat <- make_dataset(x = rattle, t = rattle$treatment_gr)
rattdat_test <- make_dataset(x = rattle, t = rattle$treatment_ind)

#rename treatment col in cache data set
setnames(cachedat, "treatment_ind", "cache_cat")

#rename treatment col in rattle data set
setnames(rattdat, "treatment_ind", "treatment_gr")



# Control Model  ---------------------------------------------------

#make a categorical variable for density in the dummy data set
controldat[, density_cat := cut(density_std, breaks = 3, labels = c("low", "medium", "high"))]

#Generate posterior predictions for controls with low density
lowD_gr <- posterior_predict(control_mod, resp = "growthratestd", newdata = controldat[density_cat == "low"], re_formula = NA)
lowD_ls <- posterior_predict(control_mod, resp = "littersizestd", newdata = controldat[density_cat == "low"], re_formula = NA)
lowD_joint <- data.frame(
  gr_pred = rowMeans(lowD_gr),
  ls_pred = rowMeans(lowD_ls),
  density = "low"
)

#Generate posterior predictions for controls with low density
medD_gr <- posterior_predict(control_mod, resp = "growthratestd", newdata = controldat[density_cat == "medium"], re_formula = NA)
medD_ls <- posterior_predict(control_mod, resp = "littersizestd", newdata = controldat[density_cat == "medium"], re_formula = NA)
medD_joint <- data.frame(
  gr_pred = rowMeans(medD_gr),
  ls_pred = rowMeans(medD_ls),
  density = "medium"
)

#Generate posterior predictions for controls with low density
highD_gr <- posterior_predict(control_mod, resp = "growthratestd", newdata = controldat[density_cat == "high"], re_formula = NA)
highD_ls <- posterior_predict(control_mod, resp = "littersizestd", newdata = controldat[density_cat == "high"], re_formula = NA)
highD_joint <- data.frame(
  gr_pred = rowMeans(highD_gr),
  ls_pred = rowMeans(highD_ls),
  density = "high"
)

#join all three predictions together
control_joint <- rbind(lowD_joint, medD_joint, highD_joint)

#plot growth rate against litter size and add ellipse
(control_ellipse <- ggplot(control_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = density), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = density), method = "lm", fill = NA)+
    stat_ellipse(aes(color = density), level = 0.95, size = 1.2) +
    scale_color_manual(values = density_cols) +
    labs(subtitle = "A) Control Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Density") +
    theme1)



# Food Model  ------------------------------------------------------

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
    labs(subtitle = "B) Food Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment") +
    theme1)



# Cache Model -----------------------------------------------------

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
    labs(subtitle = "C) Cache Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Cache")+
    theme1)



# Rattle Model TEST for supplemental ----------------------------------------------------------

#Generate posterior predictions for controls
Tcontrolrat_gr <- posterior_predict(rattle_mod_test, resp = "growthratestd", newdata = rattdat_test[treatment_ind == "Control"], re_formula = NA)
Tcontrolrat_ls <- posterior_predict(rattle_mod_test, resp = "littersizestd", newdata = rattdat_test[treatment_ind == "Control"], re_formula = NA)
Tcontrolrat_joint <- data.frame(
  gr_pred = rowMeans(Tcontrolrat_gr),
  ls_pred = rowMeans(Tcontrolrat_ls),
  treatment_ind = "Control"
)

#Generate posterior predictions for controls
Tchick_gr <- posterior_predict(rattle_mod_test, resp = "growthratestd", newdata = rattdat_test[treatment_ind == "Chickadee"], re_formula = NA)
Tchick_ls <- posterior_predict(rattle_mod_test, resp = "littersizestd", newdata = rattdat_test[treatment_ind == "Chickadee"], re_formula = NA)
Tchick_joint <- data.frame(
  gr_pred = rowMeans(Tchick_gr),
  ls_pred = rowMeans(Tchick_ls),
  treatment_ind = "Chickadee"
)

#Generate posterior predictions for controls
Trattle_gr <- posterior_predict(rattle_mod_test, resp = "growthratestd", newdata = rattdat_test[treatment_ind == "Rattle"], re_formula = NA)
Trattle_ls <- posterior_predict(rattle_mod_test, resp = "littersizestd", newdata = rattdat_test[treatment_ind == "Rattle"], re_formula = NA)
Trat_joint <- data.frame(
  gr_pred = rowMeans(Trattle_gr),
  ls_pred = rowMeans(Trattle_ls),
  treatment_ind = "Rattle"
)

Trattle_joint <- rbind(Tcontrolrat_joint, Trat_joint, Tchick_joint)

#plot growth rate against litter size and add ellipse
(Trattle_ellipse <- ggplot(Trattle_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = treatment_ind), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = treatment_ind), method = "lm", fill = NA)+
    stat_ellipse(aes(color = treatment_ind), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_ind_cols) +
    labs(subtitle = "D) Rattle Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment")+
    theme1)



# Rattle Model ----------------------------------------------------------

#Generate posterior predictions for controls
controlrat_gr <- posterior_predict(rattle_mod, resp = "growthratestd", newdata = rattdat[treatment_gr == "Control"], re_formula = NA)
controlrat_ls <- posterior_predict(rattle_mod, resp = "littersizestd", newdata = rattdat[treatment_gr == "Control"], re_formula = NA)
controlrat_joint <- data.frame(
  gr_pred = rowMeans(controlrat_gr),
  ls_pred = rowMeans(controlrat_ls),
  treatment_gr = "Control"
)

#Generate posterior predictions for controls
rattle_gr <- posterior_predict(rattle_mod, resp = "growthratestd", newdata = rattdat[treatment_gr == "Rattle"], re_formula = NA)
rattle_ls <- posterior_predict(rattle_mod, resp = "littersizestd", newdata = rattdat[treatment_gr == "Rattle"], re_formula = NA)
rat_joint <- data.frame(
  gr_pred = rowMeans(rattle_gr),
  ls_pred = rowMeans(rattle_ls),
  treatment_gr = "Rattle"
)

rattle_joint <- rbind(controlrat_joint, rat_joint)

#plot growth rate against litter size and add ellipse
(rattle_ellipse <- ggplot(rattle_joint, aes(x = ls_pred, y = gr_pred)) +
    geom_point(aes(color = treatment_gr), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = treatment_gr), method = "lm", fill = NA)+
    stat_ellipse(aes(color = treatment_gr), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_gr_cols) +
    labs(subtitle = "D) Rattle Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment")+
    theme1)



# merge and save --------------------------------------------

#collect all ellipse figures
ellipse_fig <- ggarrange(control_ellipse, food_ellipse, cache_ellipse, rattle_ellipse, ncol = 2, nrow = 2, align = "hv")

ggsave("Output/Figures/ellipse_predictions.jpeg", ellipse_fig, width = 10, height = 7, unit = "in")
ggsave("Output/Figures/ellipse_rattle_test.jpeg", Trattle_ellipse, width = 6, height = 4, unit = "in")
