
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



# POSTERIOR DISTRIBUTIONS OF RESPONSE COVARIANCE -------------------------------------------------

#Can add them all to the same panel or set the axes distribution
#not about the specific data just model prediction

#CONTROL MODEL
# 1. extract draws from the model
draws_contol <- as_draws_df(control_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_control <- draws_contol$sigma_growthratestd
sig_ls_control <- draws_contol$sigma_littersizestd
rho_control  <- draws_contol$rescor__growthratestd__littersizestd

# 3. convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_control <- rho_control * sig_gr_control * sig_ls_control
#cov_draws_df <- as.data.frame(cov_draws_control)

# 4. plot using Using bayesplot
(PostDist_control <- mcmc_areas(data.frame(covariance = cov_draws_control), pars = "covariance", prob = 0.95) +
  labs(subtitle = "Control Model", x = NULL)
)

#FOOD MODEL
# 1. extract draws from the model
draws_food <- as_draws_df(food_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_food <- draws_food$sigma_growthratestd
sig_ls_food <- draws_food$sigma_littersizestd
rho_food  <- draws_food$rescor__growthratestd__littersizestd

# 3. Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_food <- rho_food * sig_gr_food * sig_ls_food
#cov_draws_df <- as.data.frame(cov_draws)

# 4. plot using Using bayesplot
(PostDist_food <- mcmc_areas(data.frame(covariance = cov_draws_food), pars = "covariance", prob = 0.95) +
    labs(subtitle = "Food Model", x = NULL)
)

#CACHE MODEL
# 1. extract draws from the model
draws_cache <- as_draws_df(cache_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_cache <- draws_cache$sigma_growthratestd
sig_ls_cache <- draws_cache$sigma_littersizestd
rho_cache  <- draws_cache$rescor__growthratestd__littersizestd

# 3. convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_cache <- rho_cache * sig_gr_cache * sig_ls_cache
#cov_draws_df <- as.data.frame(cov_draws_control)

# 4. plot using Using bayesplot
(PostDist_cache <- mcmc_areas(data.frame(covariance = cov_draws_cache), pars = "covariance", prob = 0.95) +
    labs(subtitle = "Cache Model", x = NULL)
)

#RATTLE MODEL
# 1. extract draws from the model
draws_rattle <- as_draws_df(rattle_mod)

# 2. get residual SDs
# brms names residual SDs as sigma_y1 and sigma_y2, and correlation as rescor__y1__y2
sig_gr_rattle <- draws_rattle$sigma_growthratestd
sig_ls_rattle <- draws_rattle$sigma_littersizestd
rho_rattle  <- draws_rattle$rescor__growthratestd__littersizestd

# 3. Convert the correlation and standard deviations into the covariance metric for each posterior draw:
cov_draws_rattle <- rho_rattle * sig_gr_rattle * sig_ls_rattle
#cov_draws_df <- as.data.frame(cov_draws)

# 4. plot using Using bayesplot
(PostDist_rattle <- mcmc_areas(data.frame(covariance = cov_draws_rattle), pars = "covariance", prob = 0.95) +
    labs(subtitle = "Rattle Model", x = "Covariance (GR, LS)")
)

#merge 
PostDist <- ggarrange(PostDist_control, PostDist_food, PostDist_cache, PostDist_rattle, 
                      ncol = 1, nrow = 4, align = "hv")
PostDist



# SEPARATED PREDICTIONS -------------------------------------------------------------

#CONTROL MODEL
#create predictions for both responses
control_pred <- as.data.table(ggpredict(control_mod, terms = c("density_std"))) #soluble biomass (sb)

#rename response variables
control_pred[response.level == "growthratestd", response.level := "Growth rate"]
control_pred[response.level == "littersizestd", response.level := "Litter size"]

#plot
(Pred_control <- 
  ggplot(control_pred)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted), linewidth = 1)+
  labs(x = "Density", y = "Predicted value", subtitle = "A) Control Model")+
  facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
  theme1
)

#FOOD MODEL
#create predictions for both responses
food_pred <- as.data.table(ggpredict(food_mod, terms = c("density_std", "treatment_ind"))) #soluble biomass (sb)

#rename response variables
food_pred[response.level == "growthratestd", response.level := "Growth rate"]
food_pred[response.level == "littersizestd", response.level := "Litter size"]

#plot
(Pred_food <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    scale_color_manual(values = treatment_ind_cols)+
    scale_fill_manual(values = treatment_ind_cols)+
    guides(color = guide_legend(nrow = 3), fill = guide_legend(nrow = 3))+
    labs(x = "Density", y = NULL, subtitle = "B) Food model", color = "Treatment", fill = "Treatment")+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    theme1+
    theme(legend.position = "bottom", legend.key.size = unit(.4, "cm"))
)

#CACHE MODEL
#create predictions for both responses
cache_pred <- as.data.table(ggpredict(cache_mod, terms = c("density_std", "cache_cat"))) #soluble biomass (sb)

#rename response variables
cache_pred[response.level == "growthratestd", response.level := "Growth rate"]
cache_pred[response.level == "littersizestd", response.level := "Litter size"]

#plot
(Pred_cache <- 
    ggplot(cache_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    scale_color_manual(values = cache_cols)+
    scale_fill_manual(values = cache_cols)+
    guides(color = guide_legend(nrow = 3), fill = guide_legend(nrow = 3))+
    labs(x = "Density", y = NULL, subtitle = "C) Cache model", color = "Cache", fill = "Cache")+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    theme1+
    theme(legend.position = "bottom", legend.key.size = unit(.4, "cm"))
)

#RATTLE MODEL
#create predictions for both responses
rattle_pred <- as.data.table(ggpredict(rattle_mod, terms = c("density_std", "treatment_ind"))) #soluble biomass (sb)

#rename response variables
rattle_pred[response.level == "growthratestd", response.level := "Growth rate"]
rattle_pred[response.level == "littersizestd", response.level := "Litter size"]

#plot
(Pred_rattle <- 
    ggplot(rattle_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    scale_color_manual(values = treatment_ind_cols)+
    scale_fill_manual(values = treatment_ind_cols)+
    guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))+
    labs(x = "Density", y = NULL, subtitle = "D) Rattle model", color = "Treatment", fill = "Treatment")+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    theme1+
    theme(legend.position = "bottom", legend.key.size = unit(.4, "cm"))
)


#merge all
Predictions <- ggarrange(Pred_control, Pred_food, Pred_cache, Pred_rattle, nrow = 1, ncol = 4, align = "hv")
Predictions



# save Figures ---------------------------------------------------------

ggsave("Output/Figures/Control_model_full_results.jpeg", combined_plot, width = 10, height = 6, unit = "in")
ggsave("Output/Figures/Posterior_distributions.jpeg", PostDist, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/Bayesian_separate_predictions.jpeg", Predictions, width = 10, height = 6, unit = "in")
