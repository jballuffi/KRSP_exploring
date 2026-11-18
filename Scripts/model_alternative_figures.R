
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
ggplot(cov_food)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = treatment_ind_cols)+
  labs(x = "Covariance (GR, LS)", y = "Density")+
  theme1

#get posterior predictions for each model
post_c <- get_post(food_control_mod, "Control")
post_b <- get_post(bucket_mod, "Bucket")
post_nb <- get_post(nobucket_mod, "No Bucket")

#rbind all predictions together
post_food <- as.data.frame(rbind(post_c, post_b, post_nb))

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
ggplot(cov_cache)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = cache_cols)+
  labs(x = "Covariance (GR, LS)", y = "Density", fill = "Cache size")+
  theme1

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
    labs(subtitle = "B) Cache Size", x = "Predicted litter size", y = "Predicted growth rate", color = "Cache size") +
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
ggplot(cov_playback)+
  geom_density(aes(x = Covariance, fill = Treatment), alpha = 0.5)+
  scale_fill_manual(values = treatment_ind_cols)+
  xlim(-1.5, 1)+
  labs(x = "Covariance (GR, LS)", y = "Density")+
  theme1

#get posterior predictions for each model
post_r <- get_post(rattle_mod, "Rattle")
post_rc <- get_post(rattle_control_mod, "Control")
#post_chick <- get_post(chickadee_mod, "Chickadee")

#bind together
post_rattle <- rbind(post_r, post_rc)

#plot growth rate against litter size and add ellipse
(Rattle_ellipse <- ggplot(post_rattle, aes(x = ls, y = gr)) +
    geom_point(aes(color = Treatment), shape = 1, alpha = 0.1) +
    geom_smooth(aes(color = Treatment), method = "lm", fill = NA)+
    stat_ellipse(aes(color = Treatment), level = 0.95, size = 1.2) +
    scale_color_manual(values = treatment_ind_cols) +
    labs(subtitle = "D) Rattle Model", x = "Predicted litter size", y = "Predicted growth rate", color = "Treatment")+
    theme1)


