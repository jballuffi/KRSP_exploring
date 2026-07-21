
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




# trying to plot the covariance -------------------------------------------

summary(food_mod, pars = "cov_ranef")

all_vars <- variables(food_mod)
grep("rescor__growthratestd__littersize", all_vars, value = TRUE) 

grep("growthratestd.*littersize|littersize.*growthratestd|rescor", names(food_mod), value = TRUE)



# Extracting the posterior draws
post_draws <- as_draws(food_mod)

# Assuming you have a covariance parameter named 'Sigma'
# Extract and average the matrices across draws
sigma_draws <- subset_draws(post_draws, variable = "^Sigma") 
mean_cov_matrix <- apply(sigma_draws, 2, mean)

# Visualize using bayesplot
# Example for MCMC intervals if you are looking at specific covariance coefficients
mcmc_plot(fit_model, variable = "Sigma", regex = TRUE)
