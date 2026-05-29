

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

#take only moms from density data and remove sex col
moms <- dens[sex == "F"][, sex := NULL]

#remove duplicated rows of data
#WHY ARE THESE HERE
moms <- moms[duplicated(moms) == FALSE]

#merge data by year TRUE#merge data by year and grid
dat <- merge(moms, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#check for duplication after merge
dat[duplicated(squirrel_id) == TRUE]



# subset data ------------------------------------------------

#list the grids we are interested in
grids <- c("KL", "SU", "AG", "JO", "LL", "BT")
foods <- c(1, 0, 18, 19)

#subset to that list of grids and treatments, excluding data where density is missing
dat2 <- dat[grid %in% grids & food %in% foods & !is.na(density)]

#convert food codes to written treatments
dat2[food == 0, treatment := "Control"][food == 1, treatment := "Food add"][food == 18, treatment := "Chickadee"][food == 19, treatment := "Rattle"]

#create a table showing sample sizes by categories for non-control grids
summary <- dat2[!(grid == "SU" | grid == "KL"), .N, by = .(grid, year, treatment)]
setorder(summary, grid, year)

#save and send to ben
write.csv(summary, "Output/Data/N_summary_treatments.csv")

#MISSING DATA FROM BT IN 2022???

#subset for different analyses
control <- dat[grid == "SU" | grid == "KL"]
food <- dat[food == 0 | food == 1]
playback <- dat[food == 18 | food == 19]



## Basic figures --------------------------------------------------------




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

