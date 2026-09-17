#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in and prep data ---------------------------------------------------

#read in finalized data
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")



# Figure 1: Control and Food Add data set -----------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Density Category x Individual Treatment
fmod <- lm(growthrate ~ littersize*density_cat*treatment_ind, food)

#get effects table
food_effs <- as.data.table(ggpredict(fmod, terms = c("littersize", "density_cat", "treatment_ind")))

#plot regressions 
(food_fig <- ggplot(food_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_fill_discrete(guide = NULL)+
  scale_color_discrete(guide = NULL)+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Food treatment", color = "Density", fill = "Density")+
  facet_wrap(~facet, ncol = 1)+
  theme_bw()+
    theme(strip.background = element_blank()))



# Rattle data set ----------------------------------------------------------

#create model that interacts litter size, density and rattle treatment
rmod <- lm(growthrate ~ littersize*density_cat*treatment_ind, rattle)

#get effects table
rattle_effs <- as.data.table(ggpredict(rmod, terms = c("littersize", "density_cat", "treatment_ind")))

#plot regressions 
(rattle_fig <- ggplot(rattle_effs)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
    labs(x = "Litter size", y = "", subtitle = "Rattle treatment", color = "Density", fill = "Density")+
    facet_wrap(~facet, ncol = 1, nrow = 3)+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(strip.background = element_blank()))



# Cache data set -----------------------------------------------------------

#create model that interacts litter size, density and cache size
cmod <- lm(growthrate ~ littersize*density_cat*cache_cat, cache)

#get effects table
cache_effs <- as.data.table(ggpredict(cmod, terms = c("littersize", "density_cat", "cache_cat")))

#plot regressions 
(cache_fig <- ggplot(cache_effs)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
    scale_fill_discrete(guide = NULL)+
    scale_color_discrete(guide = NULL)+
    labs(x = "Litter size", y = "", subtitle = "Cache size", color = "Density", fill = "Density")+
    facet_wrap(~facet, ncol = 1)+
    theme_bw()+
    theme(strip.background = element_blank()))



fullfig <- ggarrange(food_fig, cache_fig, rattle_fig, ncol = 3, nrow = 1, align = "hv")
fullfig



# save figures --------------------------------------------------

ggsave("Output/Figures/lm_full.jpeg", fullfig, width = 9, height = 6, unit = "in")

# ggsave("Output/Figures/lm_foodtreament.jpeg", food_fig, width = 5, height = 10, unit = "in")
# ggsave("Output/Figures/lm_cachesize.jpeg", cache_fig, width = 5, height = 10, unit = "in")
# ggsave("Output/Figures/lm_rattletreatment.jpeg", rattle_fig, width = 5, height = 7, unit = "in")


