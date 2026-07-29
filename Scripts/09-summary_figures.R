
#Script to make plots that summarize data

# load library and read in data -------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")
cache <- readRDS("Output/Data/cache_cleaned_litters.rds")



# figures for density --------------------------------------------------------------------

#get mean density by year for controls only
means <- controls[, .(density = mean(density)), year]
setorder(means, year)

#plot mean density by year
(density_yr <- 
    ggplot(means)+
    geom_path(aes(x = year, y = density, group = 1))+
    labs(y = "Mean density (squirrels/ha)", x = "Year")+
    theme_bw())

#plot density by treatment
(density_treatment <-
    ggplot(dat)+
    geom_boxplot(aes(x = treatment, y = density))+
    labs(y = "Local density (squrirels/ha)", x = "Treatment")+
    theme_bw())



# figures for growth rate ----------------------------------------------------

#growth rate in response to litter size
(ls <- ggplot(controls)+
   geom_point(aes(x = littersize, y = growthrate))+
   geom_smooth(aes(x = littersize, y = growthrate), method = "lm")+
   labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls")+
   theme_bw())

#growth rate in response to birthdate
(birthmonth <- ggplot(controls)+
    geom_point(aes(x = bjulian, y = growthrate))+
    geom_smooth(aes(x = bjulian, y = growthrate), method = "lm")+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)", subtitle = "Controls")+
    theme_bw())

#growth rate in response to treatments
(treatment <- ggplot(dat)+
   geom_boxplot(aes(x = treatment, y = growthrate, fill = bucket), alpha = 0.6)+
   labs(x = "Grid Treatment", y = "Growth rate (g/day)")+
   theme_bw())

#average growth rates by year
annual <- dat[, .(growthrate = mean(growthrate, na.rm = TRUE)), by = .(year, treatment)]
setorder(annual, year)

#plot avg annual growth rate over time and by food add
(annualplot <- ggplot(annual)+
    geom_line(aes(x = year, y = growthrate, color = treatment), linewidth = 1)+
    labs(x = "Year", y = "Growth rate (g/day)")+
    theme_bw())


# save --------------------------------------------------------------------

#save figs
ggsave("Output/Figures/local_densities_total_year.jpeg", density_yr, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/local_densities_total_treatment.jpeg", density_treatment, width = 6, height = 4, unit = "in")

ggsave("Output/Figures/GR_birthmonth.jpeg", birthmonth, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")

ggsave("Output/Figures/GR_treatment.jpeg", treatment, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_annual.jpeg", annualplot, width = 6, height = 4, unit = "in")

