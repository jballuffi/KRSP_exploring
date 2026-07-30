#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in and prep data ---------------------------------------------------

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")

#get density quantiles
dens25Q <- dat[, round(quantile(density_std)[2], 2)]
dens75Q <- dat[, round(quantile(density_std)[4], 2)]

#categorize density based on quantiles
controls[density_std < dens25Q, density_cat := "low"]
controls[density_std > dens25Q & density_std < dens75Q, density_cat := "medium"]
controls[density_std > dens75Q, density_cat := "high"]

#categorize density based on quantiles
food[density_std < dens25Q, density_cat := "low"]
food[density_std > dens25Q & density_std < dens75Q, density_cat := "medium"]
food[density_std > dens75Q, density_cat := "high"]

#make factor with levels
controls[, density_cat := factor(density_cat, levels = c("low", "medium", "high"))]
food[, density_cat := factor(density_cat, levels = c("low", "medium", "high"))]



# Figure 1: Control data --------------------------------------------------

#this model is Growth Rate ~ Litter Size x Local Density Category
bmod <- lm(growthrate ~ littersize*density_cat, controls)
anova(bmod)
bmod_pred <- as.data.table(ggpredict(bmod, terms = c("littersize", "density_cat"))) #soluble biomass (sb)

#plot regressions on top of raw data
ggplot() +
  #geom_point(aes(x = littersize, y = growthrate, color = density_cat), alpha = 0.3, data = controls) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, data = bmod_pred) +
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, data = bmod_pred)+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Food Add Grids Only", color = "Density", fill = "Density") +
  theme_bw()



# Figure 2: Food Add data -----------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Density Category x Food Treatment
fmod <- lm(growthrate ~ littersize*density_cat*treatment_food, food)
anova(fmod)

#get effects table
food_effs <- as.data.table(ggpredict(fmod, terms = c("littersize", "density_cat", "treatment_food")))

#plot regressions on top of raw data
ggplot(food_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Food Add Grids Only", color = "Density", fill = "Density")+
  facet_wrap(~facet)+
  theme_bw()


