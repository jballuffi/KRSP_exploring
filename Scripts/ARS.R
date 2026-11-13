
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in my data and McAdam's data
mcadam <- fread("Litter_data_mcadam2019.csv")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
dat <- readRDS("Output/Data/full_cleaned_litters.rds")



# fix my data to try and match --------------------------------------------

#square litter size
controls[, littersize_std_2 := littersize_std^2, gridyear]
controls[, littersize2 := littersize^2]

#make data match
mcadam <- mcadam[!is.na(mean_growth)]
controls2 <- controls[year < 2018 & year > 1988]

controls2[, .N]
controls2[, length(unique(mother_id))]
controls2[, length(unique(year))]

mcadam[, .N]
mcadam[, length(unique(squirrel_id))]
mcadam[, length(unique(year))]



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






# Exploring ARS in non-mast years -------------------------------------------------


mod_nonmast <- glmer(ars ~ grid + bjulian_std + growthrate + littersize + (1|year),
             data = controls[mast == "no"],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod_nonmast)

nonmast_pred <- as.data.table(ggpredict(mod_nonmast, terms = c("growthrate", "littersize")))
nonmast_pred_bday <- as.data.table(ggpredict(mod_nonmast, terms = c("bjulian_std")))

controls[, littersize_int := as.factor(littersize)]

nonmast <- ggplot()+
  geom_point(aes(x = growthrate, y = ars, color = littersize_int), alpha = 0.2, controls[mast == "no"])+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, nonmast_pred)+
  labs(subtitle = "Non-mast years", x = "Growth rate (g/day)", y = "ARS", color = "Litter size")+
  theme1

nonmast_birth <- ggplot()+
  geom_point(aes(x = bjulian_std, y = ars), alpha = 0.2, controls[mast == "no"])+
  geom_line(aes(x = x, y = predicted), linewidth = 1, nonmast_pred_bday)+
  labs(subtitle = "Non-mast years", x = "Birth date (Julian)", y = "ARS", color = "Litter size")+
  theme1


####

mod_mast <- glmer(ars ~ grid + bjulian_std + growthrate + littersize + (1|year),
                     data = controls[mast == "yes"],
                     family = poisson(link = "log"),
                     control = glmerControl(optimizer = "bobyqa"))

summary(mod_mast)

mast_pred <- as.data.table(ggpredict(mod_mast, terms = c("growthrate", "littersize")))
mast_pred_bday <- as.data.table(ggpredict(mod_mast, terms = c("bjulian_std")))


mast <- ggplot()+
  geom_point(aes(x = growthrate, y = ars, color = littersize_int), alpha = 0.2, controls[mast == "yes"])+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, mast_pred)+
  labs(subtitle = "Mast years", x = "Growth rate (g/day)", y = "ARS", color = "Litter size")+
  theme1

mast_birth <- ggplot()+
  geom_point(aes(x = bjulian_std, y = ars), alpha = 0.2, controls[mast == "yes"])+
  geom_line(aes(x = x, y = predicted), linewidth = 1, mast_pred_bday)+
  labs(subtitle = "Mast years", x = "Birth date (Julian)", y = "ARS", color = "Litter size")+
  theme1



ars_plot <- ggarrange(nonmast, mast, nonmast_birth, mast_birth)

ggsave("Output/Figures/ars_by_mastyear.jpeg", ars_plot, width = 8, height = 7, unit = "in")




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







