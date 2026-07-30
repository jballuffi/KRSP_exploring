
#load library
library(brms)
library(bayesplot)
library(ggplot2)
library(ggeffects)
library(data.table)

#rescor = residual correlation parameter



# re-prep data ------------------------------------------------------------


#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")



# Read in model and summarize -----------------------------------------------------------

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")

#look at summary of models
summary(control_mod)
summary(food_mod)

#fuzzy caterpillars of models
mcmc_plot(control_mod, type = "trace")
mcmc_plot(food_mod, type = "trace")

#results plots
conditional_effects(control_mod)
conditional_effects(food_mod)



# Control Model -----------------------------------------------------------

# #look at all the variabes in the food model
# control_vars <- variables(control_mod)
# 
# #grab any names that have growth rate
# grep("growthrate", control_vars, value = TRUE) 


#POSTERIOR PREDICTIONS
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
ggplot(df_joint_control, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
  stat_ellipse(level = 0.50, color = "red", size = 1.2) +
  theme_bw() +
  labs(
    title = "Implied Covariance Response via Posterior Predictions: Control Model",
    x = "Predicted Growth Rate",
    y = "Predicted Litter Size"
  )

#POSTERIOR DISTRIBUTION OF RESPONSE COVARIANCE
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
mcmc_areas(data.frame(covariance = cov_draws_control), pars = "covariance", prob = 0.95) +
  labs(title = "Posterior Distribution of Response Covariance", x = "Covariance (GR, LS)")


#PLOT PREDICTIONS

#create predictions for both responses
control_pred <- as.data.table(ggpredict(control_mod, terms = c("density_std"))) #soluble biomass (sb)

#rename response variables
control_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
control_pred[response.level == "littersize", response.level := "Litter size"]

#plot
ggplot(control_pred)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted), linewidth = 1)+
  facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
  labs(x = "Density (stnd)", y = "Predicted value")+
  theme_bw()




# Food Model -------------------------------------

# #look at all the variabes in the food model
# food_vars <- variables(food_mod)
# grep("growthrate", food_vars, value = TRUE) 

#POSTERIOR PREDICTIONS
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
ggplot(df_joint_food, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
  stat_ellipse(level = 0.50, color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Implied Covariance Response via Posterior Predictions",
    x = "Predicted Growth Rate",
    y = "Predicted Litter Size"
  )

#POSTERIOR DISTRIBUTION OF RESPONSE COVARIANCE
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
mcmc_areas(data.frame(covariance = cov_draws_food), pars = "covariance", prob = 0.95) +
  labs(title = "Posterior Distribution of Response Covariance", x = "Covariance (GR, LS)")



#FINAL PREDICTIONS

#create predictions for both responses
food_pred <- as.data.table(ggpredict(food_mod, terms = c("density_std", "treatment_food"))) #soluble biomass (sb)

#rename response variables
food_pred[response.level == "growthrate", response.level := "Growth rate (g/day)"]
food_pred[response.level == "littersize", response.level := "Litter size"]

#plot
ggplot(food_pred)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
  facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
  labs(x = "Density (stnd)", y = "Predicted value", color = "Food treatment", fill = "Food treatment")+
  theme_bw()






# plotting covariance in response to density ------------------------------

library(tidybayes)
library(dplyr)

# 1. Create a grid of values for your predictor (e.g., x_var)
nd <- data.frame(density = seq(min(controls$density), max(controls$density), length.out = 120),
                 treatment = unique(controls$treatment_food))

# 2. Extract expected value draws for both responses
epreds <- control_mod %>%
  add_epred_draws(newdata = nd) 

# 3. Compute covariance across posterior draws for each level of x_var
cov_df <- epreds %>%
  group_by(x_var, .draw) %>%
  pivot_wider(names_from = .category, values_from = .epred) %>% 
  group_by(x_var) %>%
  summarize(
    cov_mean = cov(y1, y2),
    .groups = "drop"
  )

# 4. Plot
ggplot(cov_df, aes(x = x_var, y = cov_mean)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    x = "Independent Variable (X)",
    y = "Covariance [E(Y1), E(Y2)]",
    title = "Covariance of Expected Responses across X"
  ) +
  theme_minimal()

