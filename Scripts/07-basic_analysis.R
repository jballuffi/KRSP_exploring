

library(ggplot2)
library(data.table)
library(lubridate)
library(rsq)
library(ggpubr)
library(ggeffects)



# read in prepped data  ----------------------------------------------------

#read in data from merge script
dat <- readRDS("Output/Data/final_data.rds")

#change name of treatment
setnames(dat, "treatment", "treatment_grid")

#make an ind treatment (either control, rattle, or has bucket)
dat[, treatment_ind := bucket]
dat[treatment_ind == 1, treatment_ind := "Bucket"]
dat[treatment_ind == 0, treatment_ind := "Control"]
dat[is.na(treatment_ind), treatment_ind := treatment_grid]
dat[, treatment_ind := factor(treatment_ind)]

#make a 4-cat treatment
dat[, treatment_bucket := factor(paste0(treatment_grid, "_", bucket))]

#sample size
dat[, .N, treatment_grid]
dat[, .N, treatment_ind]
dat[, .N, treatment_bucket]


#subset for different analyses
control <- dat[grid == "SU" | grid == "KL"] #just controls
food <- dat[food == 1]                      #just food add grids
playback <- dat[food == 18 | food == 19]    #just playback experiment



# MODEL 1: Control Only with litter size as interaction --------------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Local Density
bmod <- lm(growthrate ~ litterf*density, control)
summary(bmod)

#very low explanatory power
rsq(bmod)
danova <- anova(bmod)

#get effects table
base_effs <- as.data.table(ggpredict(bmod, terms = c("density", "litterf")))

#plot results from model
ggplot()+
  geom_point(aes(x = density, y = growthrate, color = litterf), alpha = 0.2, data = control)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = base_effs)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = base_effs)+
  scale_color_discrete(name = "Litter size")+
  scale_fill_discrete(name = "Litter size")+
  labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "SU and KL, interacting")+
  theme_bw()



# MODEL 2: Control Only with not interaction --------------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Local Density
bmod2 <- lm(growthrate ~ litterf + density, control)
summary(bmod2)

#very low explanatory power
rsq(bmod2)
danova <- anova(bmod2)

#get effects table
base_effs2 <- as.data.table(ggpredict(bmod2, terms = c("density", "litterf")))

#plot results from model
ggplot()+
    geom_point(aes(x = density, y = growthrate, color = litterf), alpha = 0.2, data = control)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = base_effs2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = base_effs2)+
    scale_color_discrete(name = "Litter size")+
    scale_fill_discrete(name = "Litter size")+
    labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "SU and KL, non-interacting")+
    theme_bw()


#this tells me that litter size doesn't need to be an interaction. It mainly just changes the intercept.


# MODEL 3: Grid Treatment --------------------------------------------------------

#this model is Growth Rate ~ Litter Size x Density x Grid-level Treatment
gmod <- lm(growthrate ~ litterf + density*treatment_grid, dat)
summary(gmod)
anova(gmod)

#get effects table
grid_effs <- as.data.table(ggpredict(gmod, terms = c("density", "litterf", "treatment_grid")))

ggplot(grid_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_color_discrete(name = "Litter size")+
  scale_fill_discrete(name = "Litter size")+
  labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "Grid Treatment")+
  facet_wrap(~facet)+
  theme_bw()



# MODEL 4: Individual Treatment -----------------------------------------

#make model for growth ~ litter size * food add
imod <- lm(growthrate ~ litterf + density*treatment_ind, dat)

#summaries
summary(imod)
anova(imod)

#get effects table for model
ind_effs <- as.data.table(ggpredict(imod, terms = c("density", "litterf", "treatment_ind")))

ggplot(ind_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_color_discrete(name = "Litter size")+
  scale_fill_discrete(name = "Litter size")+
  labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "Individual Treatment")+
  facet_wrap(~facet)+
  theme_bw()



# MODEL 5: Individual Treatment -----------------------------------------

#make model for growth ~ litter size * food add
tbmod <- lm(growthrate ~ litterf + density*treatment_bucket, dat)

#summaries
summary(tbmod)
anova(tbmod)

#get effects table for model
tb_effs <- as.data.table(ggpredict(tbmod, terms = c("density", "litterf", "treatment_bucket")))

ggplot(tb_effs)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
  scale_color_discrete(name = "Litter size")+
  scale_fill_discrete(name = "Litter size")+
  labs(x = "Local density (squirrels/ha)", y = "Growth rate (g/day)", subtitle = "Individual Treatment")+
  facet_wrap(~facet)+
  theme_bw()

