
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(brms)
library(bayesplot)
library(purrr)

#write function to collect covariance draws for density plot
get_draws <- function(mod, catvar){
  draws <- as_draws_df(mod)
  sig_gr <- draws$sigma_growthratestd
  sig_ls <- draws$sigma_littersizestd
  rho  <- draws$rescor__growthratestd__littersizestd
  cov_draws <- as.data.table(rho * sig_gr * sig_ls)
  names(cov_draws) <- c("Covariance")
  cov_draws[, Treatment := catvar]
  return(cov_draws)
}


#write function to get posterior predictions for ellipse figures
get_post <- function(mod, catvar){
  gr <- posterior_predict(mod, resp = "growthratestd")
  ls <- posterior_predict(mod, resp = "littersizestd")
  joint <- data.frame(
    gr = rowMeans(gr),
    ls = rowMeans(ls),
    Treatment = catvar)
  return(joint)
}




# Control density models --------------------------------------------------

low_dens_mod <- readRDS("Output/Models/alternative_models/density_low.rds")
med_dens_mod <- readRDS("Output/Models/alternative_models/density_medium.rds")
high_dens_mod <- readRDS("Output/Models/alternative_models/density_high.rds")

#get draws for each model
cov_draws_lowdens <- get_draws(low_dens_mod, "low")
cov_draws_meddens <- get_draws(med_dens_mod, "medium")
cov_draws_highdens <- get_draws(high_dens_mod, "high")

cov_density <- rbind(cov_draws_lowdens, cov_draws_meddens, cov_draws_highdens)

#get density plot
(density_dist <- ggplot(cov_density)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = density_cols)+
  labs(subtitle = "A) Density", x = "Covariance (GR, LS)", y = "Density", fill = "Density")+
  theme1)

#get posterior predictions for each model
post_lowd <- get_post(low_dens_mod, "low")
post_medd <- get_post(med_dens_mod, "medium")
post_highd <- get_post(high_dens_mod, "high")

#rbind all predictions together
post_density <- rbind(post_lowd, post_medd, post_highd)

#plot growth rate against litter size and add ellipse
(density_ellipse <- ggplot(post_density) +
    geom_point(aes(x = ls, y = gr, color = Treatment), shape = 1, alpha = 0.1) +
    geom_smooth(aes(x = ls, y = gr, color = Treatment), method = "lm", fill = NA)+
    stat_ellipse(aes(x = ls, y = gr, color = Treatment), level = 0.95, size = 1.2) +
    scale_color_manual(values = density_cols) +
    labs(subtitle = "A) Density", x = "Predicted litter size", y = "Predicted growth rate", color = "Density") +
    theme1)



# Food models -------------------------------------------------------------

food_control_mod <- readRDS("Output/Models/alternative_models/food_control.rds")
bucket_mod <- readRDS("Output/Models/alternative_models/food_bucket.rds")
nobucket_mod <- readRDS("Output/Models/alternative_models/food_nobucket.rds")

#get draws for each model
cov_draws_c <- get_draws(food_control_mod, "Control")
cov_draws_b <- get_draws(bucket_mod, "Bucket")
cov_draws_nb <- get_draws(nobucket_mod, "No Bucket")

#rbind all three model draws
cov_food <- rbind(cov_draws_c, cov_draws_b, cov_draws_nb)

#get density plot
(food_dist <- ggplot(cov_food)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = treatment_ind_cols)+
  labs(subtitle = "B) Food treatment", x = "Covariance (GR, LS)", y = "Density")+
  theme1)

#get posterior predictions for each model
post_c <- get_post(food_control_mod, "Control")
post_b <- get_post(bucket_mod, "Bucket")
post_nb <- get_post(nobucket_mod, "No Bucket")

#rbind all predictions together
post_food <- rbind(post_c, post_b, post_nb)

#plot growth rate against litter size and add ellipse
(food_ellipse <- ggplot(post_food) +
    geom_point(aes(x = ls, y = gr, color = Treatment), shape = 1, alpha = 0.1) +
    geom_smooth(aes(x = ls, y = gr, color = Treatment), method = "lm", fill = NA)+
    stat_ellipse(aes(x = ls, y = gr, color = Treatment), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_ind_cols) +
    labs(subtitle = "B) Food treatment", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment") +
    theme1)



# cache models -----------------------------------------------------------

#read in models
small_mod <- readRDS("Output/Models/alternative_models/cache_small.rds")
med_mod <- readRDS("Output/Models/alternative_models/cache_medium.rds")
large_mod <- readRDS("Output/Models/alternative_models/cache_large.rds")

#get draws for each model
cov_draws_small <- get_draws(small_mod, "small")
cov_draws_med <- get_draws(med_mod, "medium")
cov_draws_large <- get_draws(large_mod, "large")

#rbind all three treatments
cov_cache <- rbind(cov_draws_small, cov_draws_med, cov_draws_large)

#get density plot
(cache_dist <- ggplot(cov_cache)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = cache_cols)+
  labs(subtitle = "C) Cache size", x = "Covariance (GR, LS)", y = "Density", fill = "Cache size")+
  theme1)

#get posterior predictions for each model
post_small <- get_post(small_mod, "small")
post_med <- get_post(med_mod, "medium")
post_large <- get_post(large_mod, "large")

#rbind all predictions together
post_cache <- rbind(post_small, post_med, post_large)

#plot growth rate against litter size and add ellipse
(cache_ellipse <- ggplot(post_cache, aes(x = ls, y = gr)) +
    geom_point(aes(color = Treatment), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = Treatment), method = "lm", fill = NA)+
    stat_ellipse(aes(color = Treatment), level = 0.95, size = 1.2) +
    scale_color_manual(values = cache_cols) +
    labs(subtitle = "C) Cache Size", x = "Predicted litter size", y = "Predicted growth rate", color = "Cache size") +
    theme1)



# Rattle models ----------------------------------------------------------

#read models
rattle_mod <- readRDS("Output/Models/alternative_models/rattle_rattle.rds")
rattle_control_mod <- readRDS("Output/Models/alternative_models/rattle_control.rds")
#chickadee_mod <- readRDS("Output/Models/alternative_models/rattle_chickadee.rds")

#get draws for each model
cov_draws_rattle <- get_draws(rattle_mod, "Rattle")
cov_draws_rattlec <- get_draws(rattle_control_mod, "Control")
#cov_draws_chick <- get_draws(chickadee_mod, "Chickadee")

#bind together
cov_playback <- rbind(cov_draws_rattle, cov_draws_rattlec)

#get density plot
(playback_dist <- ggplot(cov_playback)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = treatment_ind_cols)+
  xlim(-1.5, 1)+
  labs(subtitle = "Playback treatment", x = "Covariance (GR, LS)", y = "Density")+
  theme1)

#get posterior predictions for each model
post_r <- get_post(rattle_mod, "Rattle")
post_rc <- get_post(rattle_control_mod, "Control")
#post_chick <- get_post(chickadee_mod, "Chickadee")

#bind together
post_playback <- rbind(post_r, post_rc)

#plot growth rate against litter size and add ellipse
(playback_ellipse <- ggplot(post_playback, aes(x = ls, y = gr)) +
    geom_point(aes(color = Treatment), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = Treatment), method = "lm", fill = NA)+
    stat_ellipse(aes(color = Treatment), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_gr_cols) +
    labs(subtitle = "D) Playback treatment", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment")+
    theme1)



dist_fig <- ggarrange(density_dist, food_dist, cache_dist, playback_dist, ncol = 1, nrow = 4)

ellipse_fig <- ggarrange(density_ellipse, food_ellipse, cache_ellipse, playback_ellipse, ncol = 1, nrow = 4)

ggsave("Output/Figures/Posterior_distributions_alternative.jpeg", dist_fig, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/ellipse_predictions_alternative.jpeg", ellipse_fig, width = 4, height = 10, unit = "in")
