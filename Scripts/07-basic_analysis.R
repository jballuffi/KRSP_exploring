

library(ggplot2)
library(data.table)
library(lubridate)
library(rsq)
library(ggpubr)
library(ggeffects)



# read in prepped data  ----------------------------------------------------

#read in data from merge script
dat <- readRDS("Output/Data/final_data.rds")

#subset for different analyses
control <- dat[grid == "SU" | grid == "KL"] #just controls
food <- dat[food == 1]                      #just food add grids
playback <- dat[food == 18 | food == 19]    #just playback experiment



# MODEL 1: control only model --------------------------------------------------------------

#this model is Growth Rate ~ Local Density x Litter Size
bmod <- lm(growthrate ~ density*litterf, control)
summary(bmod)

#very low explanatory power
rsq(bmod)
danova <- anova(bmod)

#get effects table
base_effs <- as.data.table(ggpredict(bmod, terms = c("density", "litterf")))

#plot results from model
(b <- ggplot()+
  geom_point(aes(x = density, y = growthrate, color = litterf), alpha = 0.2, data = control)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = base_effs)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = base_effs)+
  scale_color_discrete(name = "Litter size")+
  scale_fill_discrete(guide = NULL)+
  labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "SU and KL only")+
  theme_bw())



# Add in grid treatment --------------------------------------------------------


tmod <- lm(growthrate ~ density*litterf*treatment, dat)
summary(tmod)


#get effects table
treatment_effs <- as.data.table(ggpredict(tmod, terms = c("density", "litterf", "treatment")))

ggplot(treatment_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  facet_wrap(~facet)






# Analysis for effect of having a bucket -----------------------------------------

#make model for growth ~ litter size * food add
#use the integer version of litter size
fmod <- lm(growthrate ~ density*litterf*bucket, food)

#summaries
summary(fmod)

#get effects table for model
feffs <- as.data.table(ggpredict(fmod, terms = c("density", "litterf", "bucket")))

(f <- ggplot(feffs)+
  #geom_point(aes(x = littersize, y = growthrate, color = food), alpha = 0.2, data = food)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_fill_discrete(guide = NULL)+
  labs(x = "Local density (ind/ha)", y = "Growth rate (g/day)", subtitle = "Controls + Food Add")+
  facet_wrap(~facet)+
  theme_bw()
  )
  


# Analysis for playback ----------------------------------------

pmod <- lm(growthrate ~ density*litterf*treatment, playback)
summary(pmod)

#get effects table for model
peffs <- as.data.table(ggpredict(pmod, terms = c("density", "litterf", "treatment")))

(p <- ggplot(peffs)+
    #geom_point(aes(x = littersize, y = growthrate, color = playback), alpha = 0.3, playback)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
    scale_fill_discrete(guide = NULL)+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Callback experiment")+
    facet_wrap(~facet)+
    theme_bw()
  )

