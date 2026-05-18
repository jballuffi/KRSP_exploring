

library(ggplot2)
library(data.table)
library(lubridate)



# read in prepped data ----------------------------------------------------

dens <- readRDS("Output/Data/grid_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")


#merge data by year and grid
dat <- merge(dens, growth, by = c("yr", "grid"), all.y = TRUE)

