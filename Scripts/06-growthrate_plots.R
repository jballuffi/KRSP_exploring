

# load library and read in prepped data -----------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in growth rate data
growth <- readRDS("Output/Data/final_data.rds") 

#make birth month col as a factor
growth[, bmonth := as.factor(month(bdate))]



# figures for all data ----------------------------------------------------

#growth rate in response to litter size
(ls <- ggplot(growth)+
   geom_point(aes(x = littersize, y = growthrate))+
   geom_smooth(aes(x = littersize, y = growthrate), method = "lm")+
   labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "All treatments")+
   theme_bw())

#look at growth rate in response to birth month and food add
(birthmonth <- ggplot(growth)+
    geom_point(aes(x = bjulian, y = growthrate))+
    geom_smooth(aes(x = bjulian, y = growthrate), method = "lm")+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)", subtitle = "All treatments")+
    theme_bw())



# figures for treatments of interest --------------------------------------

#plot growth rate in response to these treatments
(treatment <- ggplot(growth)+
    geom_boxplot(aes(x = treatment, y = growthrate, fill = bucket), alpha = 0.6)+
    labs(x = "Grid Treatment", y = "Growth rate (g/day)", subtitle = "Treatments 0, 1, 18 and 19")+
    theme_bw())


#get average growth rates by year
annual <- growth[, .(growthrate = mean(growthrate, na.rm = TRUE)), by = .(year, treatment)]
setorder(annual, year)

#plot avg annual growth rate over time and by food add
(annualplot <- ggplot(annual)+
    geom_line(aes(x = year, y = growthrate, color = treatment), linewidth = 1)+
    labs(x = "Year", y = "Growth rate (g/day)", subtitle = "Treatments 0, 1, 18 and 19")+
    theme_bw())



# save --------------------------------------------------------------------

ggsave("Output/Figures/GR_birthmonth.jpeg", birthmonth, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")

ggsave("Output/Figures/GR_treatment.jpeg", treatment, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_annual.jpeg", annualplot, width = 6, height = 4, unit = "in")
