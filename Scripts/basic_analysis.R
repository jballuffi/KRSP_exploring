

library(ggplot2)
library(data.table)
library(lubridate)
library(rsq)
library(ggpubr)
library(ggeffects)


# read in prepped data ----------------------------------------------------

#read in prepped data
dens <- readRDS("Output/Data/grid_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")

#merge data by year and grid
dat <- merge(dens, growth, by = c("yr", "grid"), all.y = TRUE)



# Further data prep (eventually move this to earlier script) -----------------------------------------------------

#assume anything with an NA food is a control???
dat[is.na(food), food := 0]

#make factors
dat[, food := as.factor(food)][, br := as.factor(br)][, sex := as.factor(sex)]

#figure out what to do with litter size
#we have up to 8! Should be a factor with levels I imagine?
#first I will set anything 6 and above as being one category
dat[littersize >= 6, littersize := 6]

#make littersize a character
dat[, littersize := as.character(littersize)]

#change litters of 6 to 6+
dat[littersize == "6", littersize := "6+"]

#next I will make levels with 
dat[, littersize := as.factor(littersize)]



# Get basic summaries -----------------------------------------------------

#How many growth rates?
dat[!is.na(growthrate), .N]

#Get sample size by treatment
dat[!is.na(growthrate), .N, food]



# subset data by treatment ------------------------------------------------

control <- dat[food == 0]
food <- dat[food == 0 | food == 1]
playback <- dat[food == 18 | food == 19]



# density analysis --------------------------------------------------------

#look at growth rate in response to litter size and density

#sample size
control[!is.na(growthrate), .N]

#sample size by litter size
control[!is.na(growthrate), .N, littersize]

#growth rate in response to litter size
ggplot(control)+
  geom_boxplot(aes(x = littersize, y = growthrate))+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls only")+
  theme_minimal()

#run model for growth ~ litter size x local density
dmod <- lm(growthrate ~ density*littersize, dat)
summary(dmod)

#very low explanatory power
rsq(dmod)
danova <- anova(dmod)

#get effects table
deffs <- as.data.table(ggpredict(dmod, terms = c("density", "littersize")))

d <- ggplot()+
  geom_point(aes(x = density, y = growthrate, color = littersize), alpha = 0.2, data = control)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2, data = deffs)+
  geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1, data = deffs)+
  scale_fill_discrete(guide = NULL)+
  labs(x = "Local density (ind/grid)", y = "Growth rate (g/day)")+
  theme_bw()
d



