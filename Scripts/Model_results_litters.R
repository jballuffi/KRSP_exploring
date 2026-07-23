
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

#make food treatment data set
food <- dat[food == 0 | food == 1]  #just food add and control grids

#make cache data set
cacheyears <- dat[!is.na(cache_prev), unique(year)]  #get years with cache size data
cache <- dat[(grid == "SU" | grid == "KL") & year %in% cacheyears]  #only long term control grids

#make rattle experiment dataset
rattleyears <- dat[food == 19, unique(year)] #get years with rattles
rattle <- dat[year %in% rattleyears & !grid == "AG"] #take all data from year of rattle playbacks but 



# Read in model and summarize -----------------------------------------------------------

food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")

#look at summary of models
summary(food_mod)

#fuzzy caterpillars of models
mcmc_plot(food_mod, type = "trace")

#results plots
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





# trying to plot the covariance -------------------------------------------

#get the residual correlation parameter (rescor).


# Assuming you have a covariance parameter named 'Sigma'
# Extract and average the matrices across draws
sigma_draws <- subset_draws(post_draws, variable = "^Sigma") 
mean_cov_matrix <- apply(sigma_draws, 2, mean)

# Visualize using bayesplot
# Example for MCMC intervals if you are looking at specific covariance coefficients
mcmc_plot(fit_model, variable = "Sigma", regex = TRUE)
