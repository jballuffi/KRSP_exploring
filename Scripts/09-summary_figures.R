
#Script to make plots that summarize data

# load library and read in data -------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in finalized data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
food <- readRDS("Output/Data/food_cleaned_litters.rds")



# get annual trends -------------------------------------------------------

#first create year sequence, repeated three times
years <- data.table(year = rep(seq(min(dat$year), max(dat$year)), 3))

#add in all three treatments to year seq
years$treatment <- rep(unique(dat$treatment), each = years[, .N]/3)

#get mean densities, growth rates, and litter size by year and treatment
annualgrowth <- dat[, .(growthrate = mean(growthrate, na.rm = TRUE),
                        density = mean(density, na.rm = TRUE),
                        littersize = mean(littersize, na.rm = TRUE)), by = .(year, treatment)]

#merge with years
annualtrends <- merge(years, annualgrowth, by = c("year", "treatment"), all.x = TRUE)

#get mean density by year for controls only
annualdensity <- controls[, .(density = mean(density)), year]
setorder(annualdensity, year)



# figures for multi panel --------------------------------------------------------------------

#plot mean density on controls by year
(density_year <- 
    ggplot(annualdensity)+
    geom_line(aes(x = year, y = density, group = 1), linewidth = 1, color = "grey30")+
    labs(y = "Mean density (squirrels/ha)", x = "Year")+
    theme_bw())

#plot density by treatment
(density_treatment <-
    ggplot(dat)+
    geom_boxplot(aes(x = treatment, y = density))+
    labs(y = "Local density (squrirels/ha)", x = "Treatment")+
    theme_bw())

#plot avg annual growth rate over time and by food add
(density_year_treatment <- ggplot(annualtrends)+
    geom_line(aes(x = year, y = density, color = treatment), linewidth = 1)+
    labs(x = "Year", y = "Growth rate (g/day)", color = "Treatment")+
    theme_bw()+
    theme(legend.position = "inside",
    legend.position.inside = c(0.87, 0.75))
  )

densityplot <- ggarrange(density_year, density_treatment, density_year_treatment, ncol = 1, nrow = 3, align = "h")



#growth rate in response to treatments
(gr_treatment <- ggplot(dat)+
   geom_boxplot(aes(x = treatment, y = growthrate, fill = bucket), alpha = 0.6)+
   labs(x = "Grid Treatment", y = "Growth rate (g/day)", fill = "Bucket")+
   theme_bw())

#litter size in response to treatments
(ls_treatment <- ggplot(dat)+
    geom_violin(aes(x = treatment, y = littersize, fill = bucket), alpha = 0.6)+
    labs(x = "Grid Treatment", y = "Growth rate (g/day)", fill = "Bucket")+
    theme_bw())

responseplot <- ggarrange(gr_treatment, ls_treatment, ncol = 1, nrow = 2)



# figures for stand alone -------------------------------------------------

#get the relationship between litter size and growth rate from control data set
gr_ls_mod <- lm(growthrate ~ littersize, controls)
int <- coef(gr_ls_mod)[1]
slope <- coef(gr_ls_mod)[2]

#growth rate in response to litter size
(ls <- ggplot(controls)+
   geom_point(aes(x = littersize, y = growthrate), alpha = 0.5)+
   geom_abline(aes(intercept = int, slope = slope), linewidth = 1)+
   labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls")+
   theme_bw())

#growth rate in response to birth date
(birthmonth <- ggplot(controls)+
    geom_point(aes(x = bjulian, y = growthrate))+
    geom_smooth(aes(x = bjulian, y = growthrate), method = "lm")+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)", subtitle = "Controls")+
    theme_bw())



# save --------------------------------------------------------------------

#save figs
ggsave("Output/Figures/density_summary.jpeg", densityplot, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/response_summary.jpeg", responseplot, width = 5, height = 8, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")

