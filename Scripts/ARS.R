
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

mcadam <- fread("Litter_data_mcadam2019.csv")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")




# fix my data to try and match --------------------------------------------

controls2 <- controls[year < 2018 & year > 1988]

controls2[, littersize_std_2 := littersize_std^2, gridyear]

controls2[, .N]
controls2[, length(unique(mother_id))]
controls2[, length(unique(year))]



# run model on mcadam data ------------------------------------------------

mcadam[, std_LS2 := scale(ls^2), by = .(grid, year)]

ggplot(mcadam)+
  geom_boxplot(aes(x = mast, y = litter_fit))

mod <- glmer(litter_fit ~ grid + mast + std_Julian + std_Growt + std_LS + std_LS2 + std_LS*mast + (1|squirrel_id) + (1|year),
             data = mcadam[!is.na(mean_growth)],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)


# run model on my data ----------------------------------------------------

ggplot(controls2)+
  geom_boxplot(aes(x = mast, y = ars))


mod <- glmer(ars ~ grid + mast + bjulian_std + growthrate_std + littersize_std + littersize_std_2 + littersize_std*mast + (1|mother_id) + (1|year),
             data = controls2,
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)






# Cov of LS and GR on ARS -------------------------------------------------


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


mod2 <- glmer(ars ~ grid + mast + bjulian_std + growthrate_std*littersize_std + (1|mother_id) + (1|year),
             data = controls,
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))
summary(mod2)



# 2. Perform PCA 
# Always use scale. = TRUE if variables are on different units or scales
pca_result <- prcomp(controls[, .(growthrate, littersize)], center = TRUE, scale. = TRUE)

# 3. View Summary (Variance explained by PC1 and PC2)
summary(pca_result)

head(pca_result$x)
pca_result$x












