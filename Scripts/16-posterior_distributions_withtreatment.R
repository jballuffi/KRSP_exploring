

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load library
library(tidybayes)
library(brms)
library(bayesplot)
library(purrr)


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



# Food model --------------------------------------------------------------

ep <- posterior_epred(control_mod)
dim(ep)
treat <- controls[!is.na(littersize_std), treatment_gr]
length(treat)

cov_draws <- lapply(unique(treat), function(trt){
  
  idx <- which(treat == trt)
  
  data.frame(
    treatment = trt,
    covariance = apply(ep[, idx, ], 1, function(x){
      
      n <- length(idx)
      
      growth <- x[1:n]
      litter <- x[(n+1):(2*n)]
      
      cov(growth, litter)
      
    })
  )
  
})


cov_draws <- do.call(rbind, cov_draws)

(controlplot <- ggplot(cov_draws, aes(x = covariance, y = treatment, fill = treatment))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.1, outliers = FALSE)+
  scale_fill_manual(values = treatment_ind_cols, guide = NULL)+
  labs(subtitle = "Control model", y = NULL, x = "GR:LS Covariance")+
  theme1)



# Food model --------------------------------------------------------------

ep_food <- posterior_epred(food_mod)
treat_food <- food$treatment_ind


cov_draws_food <- lapply(unique(treat_food), function(trt){
  
  idx <- which(treat_food == trt)
  
  data.frame(
    treatment = trt,
    covariance = apply(ep_food[, idx, ], 1, function(x){
      
      n <- length(idx)
      
      growth <- x[1:n]
      litter <- x[(n+1):(2*n)]
      
      cov(growth, litter)
      
    })
  )
  
})


cov_draws_food <- do.call(rbind, cov_draws_food)

(foodplot <- ggplot(cov_draws_food, aes(x = covariance, y = treatment, fill = treatment))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.1, outliers = FALSE)+
  scale_fill_manual(values = treatment_ind_cols, guide = NULL)+
  labs(subtitle = "Food model", y = NULL, x = "GR:LS Covariance")+
  theme1)



# Cache model --------------------------------------------------------------

# Posterior expected values
ep_cache <- posterior_epred(cache_mod)
dim(ep_cache)

# Treatment variable
cache_cat <- cache[!is.na(littersize_std), cache_cat]


cov_draws_cache <- lapply(unique(cache_cat), function(trt){
  
  idx <- which(cache_cat == trt)
  
  data.frame(
    treatment = trt,
    covariance = apply(ep_cache[, idx, ], 1, function(x){
      
      n <- length(idx)
      
      growth <- x[1:n]
      litter <- x[(n+1):(2*n)]
      
      cov(growth, litter)
      
    })
  )
  
})

cov_draws_cache <- do.call(rbind, cov_draws_cache)

(cacheplot <- ggplot(cov_draws_cache, aes(x = covariance, y = treatment, fill = treatment))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.1, outliers = FALSE)+
  scale_fill_manual(values = cache_cols, guide = NULL)+
  labs(subtitle = "Cache model", y = NULL, x = "GR:LS Covariance")+
  theme1)



# Rattle model ------------------------------------------------------------

# Posterior expected values
ep_rattle <- posterior_epred(rattle_mod)
dim(ep_rattle)

treat_rattle <- rattle[!is.na(littersize_std), treatment_gr]
length(treat_rattle)


cov_draws_rattle <- lapply(unique(treat_rattle), function(trt){
  
  idx <- which(treat_rattle == trt)
  
  data.frame(
    treatment = trt,
    covariance = apply(ep_rattle[, idx, ], 1, function(x){
      
      n <- length(idx)
      
      growth <- x[1:n]
      litter <- x[(n+1):(2*n)]
      
      cov(growth, litter)
      
    })
  )
  
})


cov_draws_rattle <- do.call(rbind, cov_draws_rattle)

(rattleplot <- ggplot(cov_draws_rattle, aes(x = covariance, y = treatment, fill = treatment))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.1, outliers = FALSE)+
  scale_fill_manual(values = treatment_gr_cols, guide = NULL)+
  labs(subtitle = "Rattle model", y = NULL, x = "GR:LS Covariance")+
  theme1)




#build full figure

plot <- ggarrange(controlplot, foodplot, cacheplot, rattleplot, ncol = 1, nrow = 4)

ggsave("Output/Figures/Posterior_distributions_treatment.jpeg", plot, width = 5, height = 11, unit = "in")
