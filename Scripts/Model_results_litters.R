
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



# re-prep data ------------------------------------------------------------

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
food2 <- readRDS("Output/Data/food2_cleaned_litters.rds")



# Read in model and summarize -----------------------------------------------------------

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")
food2_mod <- readRDS("Output/Models/food2_model_litters.rds")

#look at summary of models
summary(control_mod)
summary(food_mod)
summary(food2_mod)
summary(cache_mod)

#fuzzy caterpillars of models
mcmc_plot(control_mod, type = "trace")
mcmc_plot(food_mod, type = "trace")
mcmc_plot(food2_mod, type = "trace")
mcmc_plot(cache_mod, type = "trace")

#results plots
conditional_effects(control_mod)
conditional_effects(food_mod)
conditional_effects(food2_mod)
conditional_effects(cache_mod)



# POSTERIOR PREDICTIONS -----------------------------------------------------------

#These would be the best linear unbiased predictors
#Each point is a litter, how the model measures covariance
#Accounts for all fixed effects and random effects, this is how they relate after that
#These models do a lot of work as is. We know the system well.
#We've already partitioned a ton of variance.
#Can add line for predicted geometric mean regression
#standardize axes or layer elipses on top of each other

#CONTROL MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_control <- posterior_predict(control_mod, resp = "growthrate")
pred_litter_control <- posterior_predict(control_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint_control <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth_control),
  Litter_Size_Pred = rowMeans(pred_litter_control)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
(PostPred_control <- 
  ggplot(df_joint_control, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
  stat_ellipse(level = 0.50, color = "red", size = 1.2) +
  theme_bw() +
  labs(
    title = "Control Model",
    x = "Predicted Growth Rate",
    y = "Predicted Litter Size"
  )
)

#FOOD MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_food <- posterior_predict(food_mod, resp = "growthrate")
pred_litter_food <- posterior_predict(food_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint_food <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth_food),
  Litter_Size_Pred = rowMeans(pred_litter_food)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
(PostPred_food <-
    ggplot(df_joint_food, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
    stat_ellipse(level = 0.50, color = "red", size = 1.2) +
    theme_bw() +
    labs(
      title = "Food Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)

#CACHE MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_cache <- posterior_predict(cache_mod, resp = "growthrate")
pred_litter_cache <- posterior_predict(cache_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint_cache <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth_cache),
  Litter_Size_Pred = rowMeans(pred_litter_cache)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
(PostPred_cache <- 
    ggplot(df_joint_cache, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
    stat_ellipse(level = 0.50, color = "red", size = 1.2) +
    theme_bw() +
    labs(
      title = "Cache Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)

#FOOD OPTION 2
# 1. Generate posterior predictions for both variables across your data
pred_growth_food2 <- posterior_predict(food2_mod, resp = "growthrate")
pred_litter_food2 <- posterior_predict(food2_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint_food2 <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth_food2),
  Litter_Size_Pred = rowMeans(pred_litter_food2)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
(PostPred_food2 <-
    ggplot(df_joint_food2, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
    stat_ellipse(level = 0.50, color = "red", size = 1.2) +
    theme_bw() +
    labs(
      title = "Food 2 Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)


#merge all
PostPred <- ggarrange(PostPred_control, PostPred_food, PostPred_food2, PostPred_cache, 
                      nrow = 2, ncol = 2, align = "hv")
PostPred




# POSTERIOR DISTRIBUTIONS OF RESPONSE COVARIANCE -------------------------------------------------

#Can add them all to the same panel or set the axes distribution
#not about the specific data just model prediction

#CONTROL MODEL
# 1. extract draws from the model
draws_contol <- as_draws_df(control_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_control <- draws_contol$sigma_growthrate
sig_ls_control <- draws_contol$sigma_littersize
rho_control  <- draws_contol$rescor__growthrate__littersize

# 3. convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_control <- rho_control * sig_gr_control * sig_ls_control
#cov_draws_df <- as.data.frame(cov_draws_control)

# 4. plot using Using bayesplot
(PostDist_control <- mcmc_areas(data.frame(covariance = cov_draws_control), pars = "covariance", prob = 0.95) +
  labs(title = "Control Model", x = "Covariance (GR, LS)")
)


#FOOD MODEL
# 1. extract draws from the model
draws_food <- as_draws_df(food_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_food <- draws_food$sigma_growthrate
sig_ls_food <- draws_food$sigma_littersize
rho_food  <- draws_food$rescor__growthrate__littersize

# 3. Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_food <- rho_food * sig_gr_food * sig_ls_food
#cov_draws_df <- as.data.frame(cov_draws)

# 4. plot using Using bayesplot
(PostDist_food <- mcmc_areas(data.frame(covariance = cov_draws_food), pars = "covariance", prob = 0.95) +
    labs(title = "Food Model", x = "Covariance (GR, LS)")
)

#CACHE MODEL
# 1. extract draws from the model
draws_cache <- as_draws_df(cache_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_cache <- draws_cache$sigma_growthrate
sig_ls_cache <- draws_cache$sigma_littersize
rho_cache  <- draws_cache$rescor__growthrate__littersize

# 3. convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_cache <- rho_cache * sig_gr_cache * sig_ls_cache
#cov_draws_df <- as.data.frame(cov_draws_control)

# 4. plot using Using bayesplot
(PostDist_cache <- mcmc_areas(data.frame(covariance = cov_draws_cache), pars = "covariance", prob = 0.95) +
    labs(title = "Cache Model", x = "Covariance (GR, LS)")
)


#FOOD OPTION 2 MODEL
# 1. extract draws from the model
draws_food2 <- as_draws_df(food2_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_food2 <- draws_food2$sigma_growthrate
sig_ls_food2 <- draws_food2$sigma_littersize
rho_food2  <- draws_food2$rescor__growthrate__littersize

# 3. Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_food2 <- rho_food2 * sig_gr_food2 * sig_ls_food2
#cov_draws_df <- as.data.frame(cov_draws)

# 4. plot using Using bayesplot
(PostDist_food2 <- mcmc_areas(data.frame(covariance = cov_draws_food2), pars = "covariance", prob = 0.95) +
    labs(title = "Food 2 Model", x = "Covariance (GR, LS)")
)

#merge 
PostDist <- ggarrange(PostDist_control, PostDist_food, PostDist_food2, PostDist_cache, 
                      nrow = 2, ncol = 2, align = "hv")
PostDist



# SIMPLE PREDICTIONS -------------------------------------------------------------

#CONTROL MODEL
#create predictions for both responses
control_pred <- as.data.table(ggpredict(control_mod, terms = c("density_std"))) #soluble biomass (sb)

#rename response variables
control_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
control_pred[response.level == "littersize", response.level := "Litter size"]

#plot
(Pred_control <- 
  ggplot(control_pred)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted), linewidth = 1)+
  facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
  labs(x = "Density (stnd)", y = "Predicted value", title = "Control Model")+
  theme_bw()
)

#FOOD MODEL
#create predictions for both responses
food_pred <- as.data.table(ggpredict(food_mod, terms = c("density_std", "treatment_food"))) #soluble biomass (sb)

#rename response variables
food_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
food_pred[response.level == "littersize", response.level := "Litter size"]

#plot
(Pred_food <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    labs(x = "Density (stnd)", y = "Predicted value", title = "Food Model", color = "Food treatment", fill = "Food treatment")+
    theme_bw()
    #theme(legend.position = "top")
)

#CACHE MODEL
#create predictions for both responses
cache_pred <- as.data.table(ggpredict(cache_mod, terms = c("density_std", "cache_std"))) #soluble biomass (sb)

#rename response variables
cache_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
cache_pred[response.level == "littersize", response.level := "Litter size"]

#plot
(Pred_cache <- 
    ggplot(cache_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    labs(x = "Density (stnd)", y = "Predicted value", title = "Cache Model", color = "Cache level", fill = "Cache level")+
    theme_bw()
    #theme(legend.position = "top")
)

#FOOD2 MODEL
#create predictions for both responses
food2_pred <- as.data.table(ggpredict(food2_mod, terms = c("density_std", "treatment_food"))) #soluble biomass (sb)

#rename response variables
food2_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
food2_pred[response.level == "littersize", response.level := "Litter size"]

#plot
(Pred_food2 <- 
    ggplot(food2_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    labs(x = "Density (stnd)", y = "Predicted value", title = "Food Model", color = "Food treatment", fill = "Food treatment")+
    theme_bw()
    #theme(legend.position = "top")
)


#merge all
Predictions <- ggarrange(Pred_control, Pred_food, Pred_food2, Pred_cache, nrow = 2, ncol = 2, align = "h")
Predictions







# COVARIANCE FIGURES ------------------------------------------------------

#Posterior covariance plot is spring board, two things are affecting it
#build on it from spring board


#PLOT COVARIANCE PREDICTIONS
#First create new data based on density range and mean values
newdat <- data.frame(
  density_std = seq(min(dat$density_std), max(dat$density_std), length.out = 100),
  cones_tree_prev = mean(dat$cones_tree_prev, na.rm = TRUE),
  mother_age_std = mean(dat$mother_age_std, na.rm = TRUE),
  bjulian_std = mean(dat$bjulian_std, na.rm = TRUE),
  mast = "no",
  sexratio = mean(dat$sexratio),
  gridyear = "KL-1999",
  mother_byear = 1997
)

#used control model to predict based on the new data
pred <- posterior_epred(control_mod, newdata = newdat)

#Take different predictions
pred_growth <- pred[,,1]     # growthrate predictions
pred_litter <- pred[,,2]     # littersize predictions


cov_by_x <- map_dfr(seq_len(ncol(pred_growth)), function(i) {
  
  g <- pred_growth[, i]   # growthrate draws at X[i]
  l <- pred_litter[, i]   # littersize draws at X[i]
  
  tibble(
    density_std = newdat$density_std[i],
    cov = cov(g, l),
    cor = cor(g, l)
  )
})

#plot covariance
ggplot(cov_by_x, aes(density_std, cov)) +
  geom_line(size = 1.1) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance\n(growthrate × littersize)"
  ) +
  theme_minimal(base_size = 14)

#plot correlation
ggplot(cov_by_x, aes(density_std, cor)) +
  geom_line(size = 1.1) +
  labs(
    x = "Density (standardized)",
    y = "Posterior correlation\n(growthrate × littersize)"
  ) +
  theme_minimal(base_size = 14)


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


#this is the starting point and then the elipses explain whats going on.

ggplot(cov_summary, aes(density_std, median)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(
    x = "Density (standardized)",
    y = "Posterior covariance (80% CI)"
  ) +
  theme_minimal(base_size = 14)







# save Figures ---------------------------------------------------------

ggsave("Output/Figures/Posterior_predictions.jpeg", PostPred, width = 10, height = 8, unit = "in")
ggsave("Output/Figures/Posterior_distributions.jpeg", PostDist, width = 10, height = 8, unit = "in")
ggsave("Output/Figures/Bayesian_predictions.jpeg", Predictions, width = 9, height = 12, unit = "in")
