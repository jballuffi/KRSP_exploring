
#load library
library(brms)
library(bayesplot)
library(ggplot2)

#read in models
food_mod <- readRDS("Output/Models/foodtreatment_model.rds")
cache_mod <- readRDS("Output/Models/cache_model.rds")
rattle_mod <- readRDS("Output/Models/rattle_model.rds")

#look at summary of models
summary(food_mod)
summary(cache_mod)
summary(rattle_mod)

#fuzzy caterpillars of models
mcmc_plot(food_mod, type = "trace")
mcmc_plot(cache_mod, type = "trace")
mcmc_plot(rattle_mod, type = "trace")


#other plots
plot(food_mod, variable = c("b_density_std", "b_treatment_food"))
conditional_effects(food_mod)

# summary(test_model)
# #when I run without standardized variables the Rhats are way above 1.
# #when I run with standardized variables the Rhats are all 1
# 
# plot(test_model)
# 
# 
# conditional_effects(test_model)
# 
# posterior_epred(test_model)
# 
# pp_check(test_model)
# 
# mcmc_plot(test_model, type = "trace")



# trying to plot the covariance -------------------------------------------

#you cannot estimate a direct residual correlation parameter (rescor).
#Mathematically, brms only fits residual correlations if both response variables are Gaussian or Student-t.
#Because there is no parameter named rescor, your variables are instead forced to co-vary exclusively through your random effects structure (cov_ranef)

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



# Advanced formula where the covariance itself can respond to squirrel density
bf_growth <- bf(growthratestd ~ squirrel_density + (1 | p | litter_id))
bf_litter <- bf(littersize   ~ squirrel_density + (1 | p | litter_id))




# Assuming you have a covariance parameter named 'Sigma'
# Extract and average the matrices across draws
sigma_draws <- subset_draws(post_draws, variable = "^Sigma") 
mean_cov_matrix <- apply(sigma_draws, 2, mean)

# Visualize using bayesplot
# Example for MCMC intervals if you are looking at specific covariance coefficients
mcmc_plot(fit_model, variable = "Sigma", regex = TRUE)
