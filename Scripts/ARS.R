
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in my data and McAdam's data
mcadam <- fread("Litter_data_mcadam2019.csv")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")


#make data match
mcadam <- mcadam[!is.na(mean_growth)]
controls2 <- controls[year < 2018 & year > 1988]



# fix my data to try and match --------------------------------------------

controls2[, littersize_std_2 := littersize_std^2, gridyear]
controls2[, littersize2 := littersize^2]

controls2[, .N]
controls2[, length(unique(mother_id))]
controls2[, length(unique(year))]

mcadam[, .N]
mcadam[, length(unique(squirrel_id))]
mcadam[, length(unique(year))]



# run model on mcadam data ------------------------------------------------

mcadam[, std_LS2 := std_LS^2]
mcadam[, ls2 := ls^2]

ggplot(mcadam)+
  geom_boxplot(aes(x = mast, y = litter_fit))

ggplot(mcadam)+
  geom_point(aes(x = mean_growth, y = litter_fit))

ggplot(mcadam)+
  geom_point(aes(x = ls, y = litter_fit, color = mast))



mod2 <- glmer(litter_fit ~ grid + mast + std_Julian + mean_growth + ls + ls2 + ls*mast + (1|squirrel_id) + (1|year),
             data = mcadam[!is.na(mean_growth)],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod2)



# run model on my data ----------------------------------------------------

ggplot(controls2)+
  geom_boxplot(aes(x = mast, y = ars))

ggplot(controls2)+
  geom_point(aes(x = growthrate_std, y = ars))

mod <- glmer(ars ~ grid + mast + bjulian_std + growthrate + littersize + littersize2 + littersize*mast + (1|mother_id) + (1|year),
             data = controls2,
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)






# Exploring ARS in non-mast years -------------------------------------------------


mod3 <- glmer(ars ~ grid + bjulian_std + growthrate_std*littersize_std + littersize2 + (1|mother_id) + (1|year),
             data = controls2[mast == "no"],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod3)






#get line predicting correlation
corline <- lm(growthrate ~ littersize, controls)
int <- corline$coefficients[1] #intercept
slope <- corline$coefficients[2] #slope

#plot correlation using intercept and slope
ggplot(controls)+
  geom_point(aes(x = littersize, y = growthrate))+
  geom_abline(aes(intercept = int, slope = slope))

#based on line, make growth rate predictions
controls[, growthrate_pred := littersize*slope + int]

#get residuals based on those predicted values and actual values
controls[, growthrate_resid := growthrate - growthrate_pred]


ggplot(controls)+
  geom_point(aes(x = growthrate_resid, y = ars))


mod4 <- glmer(ars ~ grid + bjulian_std + growthrate_resid + (1|mother_id) + (1|year),
             data = controls[mast == "no"],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))
summary(mod4)







