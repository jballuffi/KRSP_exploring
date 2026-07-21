

# load library and read in data -------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data prepped from previous script
densities <- readRDS("Output/Data/final_data.rds")



# plot --------------------------------------------------------------------

#plot density by treatment
(density_treatment <-
    ggplot(densities)+
    geom_boxplot(aes(x = treatment, y = density))+
    labs(y = "Local density (squrirels/ha)", x = "Treatment")+
    theme_bw())

#get mean density by year for controls only
means <- densities[food == 0, .(density = mean(density)), year]
setorder(means, year)

#plot mean density by year
(density_yr <- 
    ggplot(means)+
    geom_path(aes(x = year, y = density, group = 1))+
    labs(y = "Mean density (squirrels/ha)", x = "Year")+
    theme_bw())



# save --------------------------------------------------------------------

#save figs
ggsave("Output/Figures/local_densities_total_year.jpeg", density_yr, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/local_densities_total_grid.jpeg", density_treatment, width = 6, height = 4, unit = "in")

