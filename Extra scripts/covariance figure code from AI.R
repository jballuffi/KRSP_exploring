
#load library
library(brms)
library(bayesplot)
library(ggplot2)
library(ggeffects)
library(data.table)
library(ggpubr)
library(purrr)
library(dplyr)


control_mod <- readRDS("Output/Models/control_model_litters.rds")
dat <- readRDS("Output/Data/control_cleaned_litters.rds")




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





# Food model --------------------------------------------------------------







