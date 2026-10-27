
#Script to generate prediction figure for the growth rate and litter size separately

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in models
control_mod <- readRDS("Output/Models/control_model_litters.rds")
food_mod <- readRDS("Output/Models/foodtreatment_model_litters.rds")
cache_mod <- readRDS("Output/Models/cache_model_litters.rds")
rattle_mod <- readRDS("Output/Models/rattle_model_litters.rds")



# CONTROL MODEL -------------------------------------------------------------

#create predictions for both responses
control_pred <- as.data.table(ggpredict(control_mod, terms = c("density_std"))) #soluble biomass (sb)

#rename response variables
control_pred[response.level == "growthratestd", response.level := "Growth rate"]
control_pred[response.level == "littersizestd", response.level := "Litter size"]

(Pred_control <- 
    ggplot(control_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted), linewidth = 1)+
    labs(x = "Density", y = "Predicted value", subtitle = "A) Control Model")+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    theme1
)



# FOOD MODEL --------------------------------------------------------------

#create predictions
food_pred <- as.data.table(ggpredict(food_mod, terms = c("density_std", "treatment_ind"))) #soluble biomass (sb)

#rename response variables
food_pred[response.level == "growthratestd", response.level := "Growth rate"]
food_pred[response.level == "littersizestd", response.level := "Litter size"]

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



# CACHE MODEL -------------------------------------------------------------

#create predictions
cache_pred <- as.data.table(ggpredict(cache_mod, terms = c("density_std", "cache_cat"))) #soluble biomass (sb)

#rename response variables
cache_pred[response.level == "growthratestd", response.level := "Growth rate"]
cache_pred[response.level == "littersizestd", response.level := "Litter size"]

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



# RATTLE MODEL ------------------------------------------------------------

#create predictions for both responses
rattle_pred <- as.data.table(ggpredict(rattle_mod, terms = c("density_std", "treatment_gr"))) #soluble biomass (sb)

#rename response variables
rattle_pred[response.level == "growthratestd", response.level := "Growth rate"]
rattle_pred[response.level == "littersizestd", response.level := "Litter size"]

(Pred_rattle <- 
    ggplot(rattle_pred)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2)+
    geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
    scale_color_manual(values = treatment_gr_cols)+
    scale_fill_manual(values = treatment_gr_cols)+
    guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))+
    labs(x = "Density", y = NULL, subtitle = "D) Rattle model", color = "Treatment", fill = "Treatment")+
    facet_wrap(~response.level, ncol = 1, nrow = 2, scales = "free")+
    theme1+
    theme(legend.position = "bottom", legend.key.size = unit(.4, "cm"))
)



# merge figures and save --------------------------------------------------------------------

#merge all panels
Predictions <- ggarrange(Pred_control, Pred_food, Pred_cache, Pred_rattle, nrow = 1, ncol = 4, align = "hv")
Predictions

#save
ggsave("Output/Figures/Bayesian_separate_predictions.jpeg", Predictions, width = 10, height = 6, unit = "in")
