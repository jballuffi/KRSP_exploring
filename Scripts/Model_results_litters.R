
#load library
library(brms)
library(bayesplot)
library(ggplot2)
library(data.table)



# re-prep data ------------------------------------------------------------

#read in finalized data
dat <- readRDS("Output/Data/final_data_litters.rds")

#make litter size numeric instead of integer
dat[, littersize := as.numeric(littersize)]

#make a control only dataset
controls <- dat[grid == "SU" | grid == "KL"]

#make food treatment data set
food <- dat[food == 0 | food == 1]  #just food add and control grids



# Read in model and summarize -----------------------------------------------------------

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")

#look at summary of models
summary(control_mod)
summary(food_mod)

#fuzzy caterpillars of models
mcmc_plot(control_mod, type = "trace")
mcmc_plot(food_mod, type = "trace")

#results plots
conditional_effects(control_mod)
conditional_effects(food_mod)



# Plot posteriors against one another -------------------------------------

#look at all the variabes in the food model
all_vars <- variables(food_mod)
grep("growthrate", all_vars, value = TRUE) 

# 1. Generate posterior predictions for both variables across your data
pred_growth <- posterior_predict(food_mod, resp = "growthrate")
pred_litter <- posterior_predict(food_mod, resp = "littersize")

# 2. Average the predictions across all rows to get a single vector per MCMC draw
# This isolates the overall joint behavior of your variables
df_joint <- data.frame(
  Growth_Rate_Pred = rowMeans(pred_growth),
  Litter_Size_Pred = rowMeans(pred_litter)
)

# 3. Plot the joint distribution with a 95% Credible Ellipse
ggplot(df_joint, aes(x = Growth_Rate_Pred, y = Litter_Size_Pred)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  stat_ellipse(level = 0.95, color = "orange", size = 1.2) +
  stat_ellipse(level = 0.50, color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Implied Covariance Response via Posterior Predictions",
    x = "Predicted Growth Rate",
    y = "Predicted Litter Size"
  )



# plotting the covariance -------------------------------------------

# get the residual correlation parameter (rescor).

# extract draws from the model
draws <- as_draws_df(food_mod)

# For a bivariate model with responses
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig1 <- draws$sigma_growthrate
sig2 <- draws$sigma_littersize
rho  <- draws$rescor__growthrate__littersize

# Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws <- rho * sig1 * sig2

#make a data frame version
cov_draws_df <- as.data.frame(cov_draws)

# plot using Using bayesplot
mcmc_areas(data.frame(covariance = cov_draws), pars = "covariance", prob = 0.95) +
  labs(title = "Posterior Distribution of Response Covariance", x = "Covariance (y1, y2)")



# plotting covariance in response to density ------------------------------


# Option A: Quick built-in plot
conditional_effects(food_mod, effects = "growthrate:littersize", dpar = "rescor")

#havent tried this yet. It is from gemini 

library(brms)
library(tidybayes)
library(dplyr)
library(ggplot2)

# 1. Create a grid of values for your predictor (e.g., x_var)
nd <- data.frame(density = seq(min(food$density), max(food$density), length.out = 120),
                 treatment = unique(food$treatment_food))

# 2. Extract expected value draws for both responses
epreds <- food_mod %>%
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

