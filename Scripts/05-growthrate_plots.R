

# load library and read in prepped data -----------------------------------

library(ggplot2)
library(data.table)

#read in growth rate data
growth <- readRDS("Output/Data/growth_rates.rds") 

#make birth month col as a factor
growth[, bmonth := as.factor(month(bdate))]



# figures for all data ----------------------------------------------------

#look at growth rate in response to birth month and food add
(birthmonth <- ggplot(growth)+
   geom_boxplot(aes(x = bmonth, y = growthrate))+
   labs(x = "Birth month", y = "Growth rate (g/day)", subtitle = "All treatments")+
   theme_bw())

#growth rate in response to litter size
(ls <- ggplot(growth)+
    geom_boxplot(aes(x = litterf, y = growthrate))+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "All treatments")+
    theme_minimal())



# figures for treatments of interest --------------------------------------

#take only food and control and playbacks only
basic <- growth[food == 1 | food == 0 | food == 18 | food == 19]

#plot growth rate in response to these treatments
(treatment <- ggplot(basic)+
    geom_boxplot(aes(x = treatment, y = growthrate))+
    labs(x = "Grid Treatment", y = "Growth rate (g/day)", subtitle = "Treatments 0, 1, 18 and 19")+
    theme_bw())

#get average growth rates by year
annual <- basic[, .(growthrate = mean(growthrate, na.rm = TRUE)), by = .(year, treatment)]
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
