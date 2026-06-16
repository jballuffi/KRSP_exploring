
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#create more complicated models:

#standardized litter size and growth rates by grid year
#standardize cones and  density globally
#look at examples of age est.



#read in data from merge script
dat <- readRDS("Output/Data/final_data.rds")




#this model is Growth Rate ~ Litter Size x Density x Grid-level Treatment
mod <- lmer(growthrate ~ littersize*density*treatment +
               mother_age + 
               sex + 
               mast + 
               bjulian + 
               (1|gridyear), dat)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


mod_pred <- as.data.table(ggpredict(mod, terms = c("littersize", "density", "treatment")))

ggplot(mod_pred)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_color_discrete(name = "Density (ind/ha)")+
  scale_fill_discrete(name = "Density (ind/ha)")+
  labs(x = "Litter size", y = "Growth rate (g/day)")+
  facet_wrap(~facet)+
  theme_bw()



