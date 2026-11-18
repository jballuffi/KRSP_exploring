
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in my data and McAdam's data
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")


#square litter size
controls[, littersize2 := littersize^2]



# Exploring ARS in non-mast years -------------------------------------------------

mod_mast <- glmer(ars ~ grid + bjulian_std + growthrate*littersize + mast + littersize2 + (1|year),
                     data = controls,
                     family = poisson(link = "log"),
                     control = glmerControl(optimizer = "bobyqa"))

summary(mod_mast)

mast_pred <- as.data.table(ggpredict(mod_mast, terms = c("growthrate", "littersize", "mast")))
setorder(mast_pred, group, x)


ggplot(mast_pred)+
  #geom_point(aes(x = growthrate, y = ars, color = littersize_int), alpha = 0.2, controls)+
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
  labs(x = "Growth rate (g/day)", y = "ARS", color = "Litter size", fill = "Litter size")+
  facet_wrap(~facet, scales = "free")+
  theme1






mod_nonmast <- glmer(ars ~ grid + bjulian_std + growthrate + littersize + (1|year),
             data = controls[mast == "no"],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

nonmast_pred <- as.data.table(ggpredict(mod_nonmast, terms = c("growthrate", "littersize")))

controls[, littersize_int := as.factor(littersize)]

nonmast <- ggplot()+
  geom_point(aes(x = growthrate, y = ars, color = littersize_int), alpha = 0.2, controls[mast == "no"])+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, nonmast_pred)+
  labs(subtitle = "Non-mast years", x = "Growth rate (g/day)", y = "ARS", color = "Litter size")+
  theme1




####

mod_mast <- glmer(ars ~ grid + bjulian_std + growthrate + littersize + (1|year),
                     data = controls[mast == "yes"],
                     family = poisson(link = "log"),
                     control = glmerControl(optimizer = "bobyqa"))

summary(mod_mast)

mast_pred <- as.data.table(ggpredict(mod_mast, terms = c("growthrate", "littersize")))


mast <- ggplot()+
  geom_point(aes(x = growthrate, y = ars, color = littersize_int), alpha = 0.2, controls[mast == "yes"])+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1, mast_pred)+
  labs(subtitle = "Mast years", x = "Growth rate (g/day)", y = "ARS", color = "Litter size")+
  theme1


ars_plot <- ggarrange(nonmast, mast)

















ggsave("Output/Figures/ars_by_mastyear.jpeg", ars_plot, width = 8, height = 7, unit = "in")







