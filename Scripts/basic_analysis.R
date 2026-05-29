

library(ggplot2)
library(data.table)
library(lubridate)
library(rsq)
library(ggpubr)
library(ggeffects)


# read in prepped data  ----------------------------------------------------




# Base model --------------------------------------------------------------

#this model is Growth Rate ~ Local Density x Litter Size

bmod <- lm(growthrate ~ density*litterf, dat2)
summary(dmod)

#very low explanatory power
rsq(dmod)
danova <- anova(dmod)

#get effects table
deffs <- as.data.table(ggpredict(dmod, terms = c("density", "litterf")))

#plot results from model
(d <- ggplot()+
  geom_point(aes(x = density, y = growthrate, color = litterf), alpha = 0.2, data = control)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = deffs)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = deffs)+
  scale_fill_discrete(guide = NULL)+
  labs(x = "Local density (ind/grid)", y = "Growth rate (g/day)", subtitle = "Controls only")+
  theme_bw())

#this is using a rough idea of density, just the total number of squirrels known on the grid that year.
#probably need a more local assessment? Number of neighbors for example?



# Analysis for effect of food add -----------------------------------------

#sample size
food[!is.na(growthrate), .N]

#sample size by litter size
food[!is.na(growthrate), .N, food]

#sample size by litter size
food[!is.na(growthrate), .N, littersize]

#make model for growth ~ litter size * food add
#use the integer version of litter size
fmod <- lm(growthrate ~ littersize*food, food)

#summaries
summary(fmod)
rsq(fmod)
fanova <- anova(fmod)

#get effects table for model
feffs <- as.data.table(ggpredict(fmod, terms = c("littersize", "food")))

(f <- ggplot()+
  geom_point(aes(x = littersize, y = growthrate, color = food), alpha = 0.2, food)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = feffs)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = feffs)+
  scale_fill_discrete(guide = NULL)+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls + Food Add only")+
  theme_bw()
  )
  


# Analysis for playback experiment ----------------------------------------

#samplesize
playback[!is.na(growthrate), .N]
playback[!is.na(growthrate), .N, playback]


ggplot(playback)+
  geom_boxplot(aes(x = playback, y = growthrate))+
  theme_bw()

ggplot(playback)+
  geom_point(aes(x = density, y = growthrate, color = playback))+
  theme_bw()

#need to fix local density before I can incorporate it into a model with playback
#for now will only focus on litter size and playback

pmod <- lm(growthrate ~ littersize*playback, playback)
summary(pmod)
rsq(pmod)
panova <- anova(pmod)

#get effects table for model
peffs <- as.data.table(ggpredict(pmod, terms = c("littersize", "playback")))

(p <- ggplot()+
    geom_point(aes(x = littersize, y = growthrate, color = playback), alpha = 0.3, playback)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = peffs)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = peffs)+
    scale_fill_discrete(guide = NULL)+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Callback experimenet")+
    theme_bw()
  )

