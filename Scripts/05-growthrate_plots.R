

# load library and read in prepped data -----------------------------------

library(ggplot2)
library(data.table)

#read in growth rate data
growth <- readRDS("Output/Data/growth_rates.rds") 



# figures -----------------------------------------------------------------

basic <- growth[food == 1 | food == 0]

#look at a t-test of food add and growth rate
(foodadd <- ggplot(basic)+
    geom_boxplot(aes(x = food, y = growthrate))+
    labs(x = "Food add", y = "Growth rate (g/day)")+
    theme_bw())

#get birth month based on bdate
basic[, bmonth := as.factor(month(bdate))]

#if bmonth is NA, use month of date1
basic[is.na(bmonth), bmonth := as.factor(month(date1))]

#look at growth rate in response to birth month and food add
(birthmonth <- ggplot(basic)+
    geom_boxplot(aes(x = bmonth, y = growthrate, color = food))+
    labs(x = "Birth month", y = "Growth rate (g/day)")+
    theme_bw())

#get average growth rates by year
annual <- basic[, .(growthrate = mean(growthrate, na.rm = TRUE)), by = .(year, food)]
setorder(annual, year)

#plot avg annual growth rate over time and by food add
(annualplot <- ggplot(annual)+
    geom_line(aes(x = year, y = growthrate, color = food), linewidth = 1)+
    labs(x = "Year", y = "Growth rate (g/day)")+
    theme_bw())

#growth rate in response to litter size
(ls <- ggplot(basic)+
    geom_boxplot(aes(x = litterf, y = growthrate))+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls only")+
    theme_minimal())



# save --------------------------------------------------------------------

ggsave("Output/Figures/GR_foodadd.jpeg", foodadd, width = 3, height = 4, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", birthmonth, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_annual.jpeg", annualplot, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")
