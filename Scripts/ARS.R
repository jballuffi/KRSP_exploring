
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in my data and McAdam's data
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
rattle <- readRDS("Output/Data/rattle_cleaned_litters.rds")


#square litter size
controls[, littersize2 := littersize^2]
controls[, littersize_int := as.factor(littersize)]


#alternative model where the response is the rate
# model <- glmer(
#   cbind(ars, littersize - ars) ~ grid + bjulian_std + growthrate + littersize + mast + (1|year),
#   family = binomial(link = "logit"),
#   data = controls
# )



# Exploring ARS in controls for different conditions, mast vs. non-mast and low density vs. high density -------------------------------------------------

#change mast year
controls[mast == "yes", mast := "mast"]
controls[mast == "no", mast := "non-mast"]

med_dens <- median(controls$density)

#create binary category for density
controls[density > med_dens, density_bin := "high density"]
controls[density <= med_dens, density_bin := "low density"]
controls[, .N, density_bin]

#create a mast-dens category
controls[, mast_dens := paste0(mast, " + ", density_bin)]
controls[, .N, mast_dens]

mod <- glmer(ars ~ grid + bjulian_std + offset(log(littersize))*growthrate*mast_dens +  (1|year),
                     data = controls,
                     family = poisson(link = "log"),
                     control = glmerControl(optimizer = "bobyqa"))

summary(mod)

pred <- as.data.table(ggpredict(mod, terms = c("growthrate", "littersize", "mast_dens")))
setorder(pred, group, x)

ggplot(pred)+
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
  labs(x = "Growth rate (g/day)", y = "ARS", color = "Litter size", fill = "Litter size")+
  ylim(0, 6)+
  facet_wrap(~facet, scales = "free")+
  theme1





# exploring ARS in different food add conditions --------------------------

mod_food <- glmer(ars ~ grid + bjulian_std + offset(log(littersize))*treatment_ind*growthrate + (1|year),
             data = food,
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

anova(mod_food)

pred_food <- as.data.table(ggpredict(mod_food, terms = c("growthrate", "littersize", "treatment_ind")))

ggplot(pred_food)+
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
  labs(x = "Growth rate (g/day)", y = "ARS", color = "Litter size", fill = "Litter size")+
  facet_wrap(~facet, scales = "free")+
  theme1





# exploring ARS and cache size --------------------------------------------

mod_cache <- glmer(ars ~ grid + bjulian_std + offset(log(littersize))*cache_cat*growthrate + (1|year),
                  data = cache,
                  family = poisson(link = "log"),
                  control = glmerControl(optimizer = "bobyqa"))

anova(mod_cache)

pred_cache <- as.data.table(ggpredict(mod_cache, terms = c("growthrate", "littersize", "cache_cat")))

ggplot(pred_cache)+
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.2)+
  geom_line(aes(x = x, y = predicted, color = group), linewidth = 1)+
  labs(x = "Growth rate (g/day)", y = "ARS", color = "Litter size", fill = "Litter size")+
  facet_wrap(~facet, scales = "free")+
  theme1




