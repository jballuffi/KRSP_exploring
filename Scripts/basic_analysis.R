

library(ggplot2)
library(data.table)
library(lubridate)
library(rsq)
library(ggpubr)
library(ggeffects)


# read in prepped data  ----------------------------------------------------

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")



# initial merge and clean -------------------------------------------------

#remove NA growth rates
growth <- growth[!is.na(growthrate)]

#test if there are dup ids in growth rate data
growth[duplicated(squirrel_id) == TRUE]

#take only moms from density data and remove sex col
moms <- dens[sex == "F"][, sex := NULL]

#remove duplicated rows of data
#WHY ARE THESE HERE
moms <- moms[duplicated(moms) == FALSE]

#merge data by year TRUE#merge data by year and grid
dat <- merge(moms, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#check for duplication after merge
dat[duplicated(squirrel_id) == TRUE]

#make factors
dat[, br := as.factor(br)]



# Create litter factor -----------------------------------------------------

#make a new col for litter as a factor
dat[, litterf := littersize]

#first I will set anything 6 and above as being one category
dat[litterf >= 6, litterf := 6]

#make litter size a character
dat[, litterf := as.character(litterf)]

#change litters of 6 to 6+
dat[litterf == "6", litterf := "6+"]

#next I will make levels with 
dat[, litterf := as.factor(litterf)]



# subset to certain grids and treatments ----------------------------------

dat2 <- 




# Get basic summaries -----------------------------------------------------

#How many growth rates?
dat[!is.na(growthrate), .N]

#Get sample size by treatment
dat[!is.na(growthrate), .N, food]



# subset data by treatment ------------------------------------------------

#subset for different analyses
control <- dat[food == 0]
food <- dat[food == 0 | food == 1]
playback <- dat[food == 18 | food == 19]

#food as factor
food[, food := as.factor(food)]

#redo playback categories
playback[food == 18, playback := "Chickadee"]
playback[food == 19, playback := "Rattle"]

#make playback a factor
playback[, playback := as.factor(playback)]



## density analysis --------------------------------------------------------

#look at growth rate in response to litter size and density
#use the factor version of litter size


#flip litter size and density. Categorize density and use that as the factor keep litter size an integer


#sample size
control[!is.na(growthrate), .N]

#sample size by litter size
control[!is.na(growthrate), .N, litterf]

#growth rate in response to litter size
(ls <- ggplot(control)+
  geom_boxplot(aes(x = litterf, y = growthrate))+
  labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls only")+
  theme_minimal())

#run model for growth ~ litter size x local density
dmod <- lm(growthrate ~ density*litterf, control)
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

