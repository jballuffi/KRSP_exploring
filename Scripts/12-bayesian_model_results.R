
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
#rescor = residual correlation parameter


# Summarize models -----------------------------------------------------------

#look at summary of models
summary(control_mod)
summary(food_mod)
summary(cache_mod)
summary(rattle_mod)

# #fuzzy caterpillars of models
# mcmc_plot(control_mod, type = "trace")
# mcmc_plot(food_mod, type = "trace")
# mcmc_plot(cache_mod, type = "trace")
# mcmc_plot(rattle_mod, type = "trace")
# 
# #results plots
# conditional_effects(control_mod)
# conditional_effects(food_mod)
# conditional_effects(cache_mod)
# conditional_effects(rattle_mod)


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

#RATTLE MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_rattle <- posterior_predict(rattle_mod, resp = "growthrate")
pred_litter_rattle <- posterior_predict(rattle_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint_rattle <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth_rattle),
  Litter_Size_Pred = rowMeans(pred_litter_rattle)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
(PostPred_rattle <-
    ggplot(df_joint_rattle, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
    stat_ellipse(level = 0.50, color = "red", size = 1.2) +
    theme_bw() +
    labs(
      title = "Rattle Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)


#merge all
PostPred <- ggarrange(PostPred_control, PostPred_food, PostPred_cache, PostPred_rattle,
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

#RATTLE MODEL
# 1. extract draws from the model
draws_rattle <- as_draws_df(rattle_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_rattle <- draws_rattle$sigma_growthrate
sig_ls_rattle <- draws_rattle$sigma_littersize
rho_rattle  <- draws_rattle$rescor__growthrate__littersize

# 3. Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_rattle <- rho_rattle * sig_gr_rattle * sig_ls_rattle
#cov_draws_df <- as.data.frame(cov_draws)

# 4. plot using Using bayesplot
(PostDist_rattle <- mcmc_areas(data.frame(covariance = cov_draws_rattle), pars = "covariance", prob = 0.95) +
    labs(title = "Rattle Model", x = "Covariance (GR, LS)")
)

#merge 
PostDist <- ggarrange(PostDist_control, PostDist_food, PostDist_cache, PostDist_rattle, 
                      nrow = 2, ncol = 2, align = "hv")
PostDist



# SEPARATED PREDICTIONS -------------------------------------------------------------

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
food_pred <- as.data.table(ggpredict(food_mod, terms = c("density_std", "treatment_ind"))) #soluble biomass (sb)

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
cache_pred <- as.data.table(ggpredict(cache_mod, terms = c("density_std", "cache_cat"))) #soluble biomass (sb)

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

#RATTLE MODEL
#create predictions for both responses
rattle_pred <- as.data.table(ggpredict(rattle_mod, terms = c("density_std", "treatment_ind"))) #soluble biomass (sb)

#rename response variables
rattle_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
rattle_pred[response.level == "littersize", response.level := "Litter size"]

#plot
(Pred_rattle <- 
    ggplot(rattle_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    labs(x = "Density (stnd)", y = "Predicted value", title = "Rattle Model", color = "Rattle treatment", fill = "Rattle treatment")+
    theme_bw()
    #theme(legend.position = "top")
)


#merge all
Predictions <- ggarrange(Pred_control, Pred_food, Pred_cache, Pred_rattle, nrow = 2, ncol = 2, align = "hv")
Predictions



# save Figures ---------------------------------------------------------

ggsave("Output/Figures/Posterior_predictions.jpeg", PostPred, width = 10, height = 8, unit = "in")
ggsave("Output/Figures/Posterior_distributions.jpeg", PostDist, width = 10, height = 8, unit = "in")
ggsave("Output/Figures/Bayesian_predictions.jpeg", Predictions, width = 9, height = 12, unit = "in")
