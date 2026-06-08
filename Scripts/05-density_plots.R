

# load library and read in data -------------------------------------------

library(data.table)
library(ggplot2)

#read in data prepped from previous script
densities <- readRDS("Output/Data/final_data.rds")



# plot --------------------------------------------------------------------

#get mean density by year
means <- densities[, .(density = mean(density)), year]
setorder(means, year)

#plot mean density by year
(totaldensity_yr <- 
    ggplot(means)+
    geom_path(aes(x = year, y = density, group = 1))+
    labs(y = "Mean local density (squirrels/ha)", x = "Year")+
    theme_bw())

#plot density by grid
(totaldensity_gr <- 
    ggplot(densities)+
    geom_boxplot(aes(x = grid, y = density))+
    labs(y = "Local density (squrirels/ha)", x = "Grid")+
    theme_bw())


# save --------------------------------------------------------------------

#save figs
ggsave("Output/Figures/local_densities_total_year.jpeg", totaldensity_yr, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/local_densities_total_grid.jpeg", totaldensity_gr, width = 6, height = 4, unit = "in")

