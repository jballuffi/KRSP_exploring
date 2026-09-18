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




# All results from control model ------------------------------------------

# get conditional effects plots from control mod
ce <- conditional_effects(control_mod)
plots <- plot(ce, plot = FALSE)

#use the patchwork package to paste them together
combined_plot <- patchwork::wrap_plots(plots)



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
pred_growth_control <- posterior_predict(control_mod, resp = "growthratestd")
pred_litter_control <- posterior_predict(control_mod, resp = "littersizestd")

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
      subtitle = "Control Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)

#FOOD MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_food <- posterior_predict(food_mod, resp = "growthratestd")
pred_litter_food <- posterior_predict(food_mod, resp = "littersizestd")

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
      subtitle = "A) Food Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)

#CACHE MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_cache <- posterior_predict(cache_mod, resp = "growthratestd")
pred_litter_cache <- posterior_predict(cache_mod, resp = "littersizestd")

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
      subtitle = "B) Cache Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)

#RATTLE MODEL
# 1. Generate posterior predictions for both variables across your data
pred_growth_rattle <- posterior_predict(rattle_mod, resp = "growthratestd")
pred_litter_rattle <- posterior_predict(rattle_mod, resp = "littersizestd")

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
      subtitle = "C) Rattle Model",
      x = "Predicted Growth Rate",
      y = "Predicted Litter Size"
    )
)


#merge all
PostPred <- ggarrange(PostPred_food, PostPred_cache, PostPred_rattle, nrow = 3, ncol = 1, align = "hv")
PostPred

ggsave("Output/Figures/Posterior_predictions.jpeg", PostPred, width = 5, height = 9, unit = "in")