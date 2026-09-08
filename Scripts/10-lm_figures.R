#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in and prep data ---------------------------------------------------

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")

#get density quantiles
cache25Q <- cache[, round(quantile(cache_std)[2], 2)]
cache75Q <- cache[, round(quantile(cache_std)[4], 2)]

#make factor with levels
cache[, cache_cat := factor(density_cat, levels = c("low", "medium", "high"))]



# Figure 1: Control data --------------------------------------------------

# #this model is Growth Rate ~ Litter Size x Local Density Category
# bmod <- lm(growthrate ~ littersize*density_cat, controls)
# anova(bmod)
# bmod_pred <- as.data.table(ggpredict(bmod, terms = c("littersize", "density_cat"))) #soluble biomass (sb)
# 
# controls[, .N]
# 
# #plot regressions on top of raw data
# (control_fig <- ggplot() +
#   geom_point(aes(x = littersize, y = growthrate, color = density_cat), alpha = 0.3, data = controls) +
#   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, data = bmod_pred) +
#   geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, data = bmod_pred)+
#   labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Control Grids Only", color = "Density", fill = "Density") +
#   theme_bw())



# Figure 2: Food Add data -----------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Density Category x Food Treatment
fmod <- lm(growthrate ~ littersize*density_cat*treatment_food, food)
anova(fmod)

food[, .N, treatment_food]

#get effects table
food_effs <- as.data.table(ggpredict(fmod, terms = c("littersize", "density_cat", "treatment_food")))

#plot regressions on top of raw data
(food_fig <- ggplot(food_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Food Add Grids Only", color = "Density", fill = "Density")+
  facet_wrap(~facet, ncol = 1)+
  theme_bw())



# merge figures and save --------------------------------------------------


fig <- ggarrange(control_fig, food_fig, ncol = 1, nrow = 2)

ggsave("Output/Figures/lm_results.jpeg", width = 7, height = 9, unit = "in")


