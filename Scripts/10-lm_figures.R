#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in and prep data ---------------------------------------------------

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")







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


