#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in and prep data ---------------------------------------------------

#read in finalized data
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")
dat <- readRDS("Output/Data/full_cleaned_litters.rds")



# Figure 1: Control, Food Add, and Rattl -----------------------------------------------------------

#make model for just controls
Cmod <- lm(growthrate ~ littersize*density_cat, dat[treatment_ind == "Control"])
Ceffs <- as.data.table(ggpredict(Cmod, terms = c("littersize", "density_cat")))
Ceffs[, facet := "Control"]

#make model for just buckets
Bmod <- lm(growthrate ~ littersize*density_cat, dat[treatment_ind == "Bucket"])
Beffs <- as.data.table(ggpredict(Bmod, terms = c("littersize", "density_cat")))
Beffs[, facet := "Bucket"]

#make model for just no buckets
Nmod <- lm(growthrate ~ littersize*density_cat, dat[treatment_ind == "No Bucket"])
Neffs <- as.data.table(ggpredict(Nmod, terms = c("littersize", "density_cat")))
Neffs[, facet := "No Bucket"]

#make model for just no buckets
Rmod <- lm(growthrate ~ littersize*density_cat, dat[treatment_ind == "Rattle"])
Reffs <- as.data.table(ggpredict(Rmod, terms = c("littersize", "density_cat")))
Reffs[, facet := "Rattle"]

#bind rattle and food add together
effs <- rbind(Ceffs, Beffs, Neffs, Reffs)
effs[, facet := factor(facet, levels = c("Control", "Bucket", "No Bucket", "Rattle"))]

#plot regressions 
(treatment_fig <- ggplot(effs)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
    scale_fill_discrete(guide = NULL)+
    scale_color_discrete(guide = NULL)+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Individual treatment", color = "Density", fill = "Density")+
    facet_wrap(~facet, ncol = 1, scales = "free")+
    theme1
  )



# Cache data set -----------------------------------------------------------

#create model that interacts litter size, density and cache size
cmod <- lm(growthrate ~ littersize*density_cat*cache_cat, cache)

#get effects table
cache_effs <- as.data.table(ggpredict(cmod, terms = c("littersize", "density_cat", "cache_cat")))

#plot regressions 
(cache_fig <- ggplot(cache_effs)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = 0.2)+
    geom_line(aes(y = predicted, x = x, color = group, group = group), linewidth = 1)+
    labs(x = "Litter size", y = "", subtitle = "Cache size", color = "Density", fill = "Density")+
    facet_wrap(~facet, ncol = 1, scales = "free")+
    theme1+
    theme(legend.position = "bottom")
  )



fullfig <- ggarrange(treatment_fig, cache_fig, ncol = 2, nrow = 1, align = "hv")
fullfig



# save figures --------------------------------------------------

ggsave("Output/Figures/lm_full.jpeg", fullfig, width = 6.5, height = 10, unit = "in")
