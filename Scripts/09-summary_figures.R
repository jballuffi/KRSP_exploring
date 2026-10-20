
#Script to make plots that summarize data

# load library and read in data -------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in finalized litter data
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")

#read in annual cone production and raw local density data
cones_yr <- readRDS("Output/Data/tree_cone_counts_annual.rds")
dens <- readRDS("Output/Data/local_densities.rds")



# Summary of sample sizes in tables -----------------------------------------------------------

#cut down data to food add grids, rattle grids, chickadee grids, and KL/SU
dat2 <- dat[(food == 1 | food == 18 | food == 19) | (grid == "SU" | grid == "KL")]

#sample sizes for each treatment excluding 
table_treatgrids <- dat2[, .(N_juveniles = sum(littersize),
                             N_litters = length(unique(litter_id)),
                             N_females = length(unique(mother_id)),
                             Years = paste0(min(year), "-", max(year))), by = .(Grid = grid, Grid_treatment = treatment_gr)]

#sample sizes but by year for 2010 - 2022
table_year <- dat2[year <= 2022 & year >= 2010, .(N_juveniles = sum(littersize),
                                                  N_litters = length(unique(litter_id)),
                                                  N_females = length(unique(mother_id))), 
                   by = .(Year = year, Grid = grid, Grid_treatment = treatment_gr)]

setorder(table_year, Year)



# get annual means -------------------------------------------------------

#first, create year sequence, repeated three times
years <- data.table(year = rep(seq(min(dat$year), max(dat$year)), 3))

#next, add in all three treatments to year seq
years$treatment_gr <- rep(unique(dat$treatment_gr), each = years[, .N]/3)

#get the treatment of every grid every year
treatments <- dat[, .(treatment_gr = getmode(treatment_gr)), by = .(year, grid)]

#merge density data with treatments and get mean and SD density by treatment and year
dens <- merge(dens[, .(year, grid, density)], treatments, by = c("year", "grid"), all.y = TRUE)
dens <- dens[, .(density = mean(density, na.rm = TRUE), density_sd = sd(density, na.rm = TRUE)), by = .(year, treatment_gr)]

#merge density with year sequence
density_yr <- merge(years, dens, by = c("year", "treatment_gr"), all.x = TRUE)

#make a mast year col in cone data
names(cones_yr) <- c("year", "cones", "cones_sd")

#cut cone data to match year seq
cones_yr <- cones_yr[year >= min(years$year) & year <= max(years$year)]

#get conf lims
cones_yr[, cones_low := cones - cones_sd][, cones_max := cones + cones_sd]
cones_yr[cones_low < 0, cones_low := 0]

#add mast year col
cones_yr[year %in% mast_years, mast := "yes"]
cones_yr[is.na(mast), mast := "no"]



# Cone production and density over time ------------------------------------------------------

#plot density over time by treatment
(density_year <- ggplot(density_yr)+
    geom_vline(aes(xintercept = 2005), linewidth = 0.9, color = "firebrick1", alpha = .7, linetype = "dashed")+
    geom_vline(aes(xintercept = 2017), linewidth = 0.9, color = "firebrick1", alpha = .7, linetype = "dashed")+
    geom_vline(aes(xintercept = 2018), linewidth = 0.9, color = "orchid1", alpha = .7, linetype = "dashed")+
    geom_vline(aes(xintercept = 2022), linewidth = 0.9, color = "orchid1", alpha = .7, linetype = "dashed")+
    geom_ribbon(aes(x = year, ymin = density - density_sd, ymax = density + density_sd, fill = treatment_gr), alpha = .3)+
    geom_line(aes(x = year, y = density, color = treatment_gr), linewidth = 0.9)+
    labs(x = "Year", y = "Local density (squirrels/ha)", subtitle = "A)", fill = "Grid treatment", color = "Grid treatment")+
    scale_fill_manual(values = treatment_gr_cols)+
    scale_color_manual(values = treatment_gr_cols)+
    theme1+
    theme(legend.position = "inside",
         legend.position.inside = c(0.1, 0.78))
)

#plot density over time by treatment
(cones_year <- ggplot(cones_yr)+
    geom_ribbon(aes(x = year, ymin = cones_low, ymax = cones_max), fill = "grey40", alpha = .3)+
    geom_line(aes(x = year, y = cones), color = "grey40", linewidth = 0.9)+
    geom_point(aes(x = year, y = cones, size = mast), shape = 18, color = "grey40")+
    scale_size_manual(values = mast_sizes, guide = NULL)+
    labs(x = "Year", y = "Log cone production (cones/tree)", subtitle = "B)")+
    theme1
)

annualplot <- ggarrange(density_year, cones_year, ncol = 1, nrow = 2, align = "h")



# Density, growth rate, and litter size in response to treatments  --------------------------------------------------------------------

#density by grid treatment
(density_treatment <-
    ggplot(dat)+
    geom_jitter(aes(x = treatment_gr, y = density), shape = 1, color = "grey40", width = .25, alpha = .15)+
    geom_boxplot(aes(x = treatment_gr, y = density), fill = "grey50", alpha = 0.4, outliers = FALSE)+
    labs(y = "Local density (squirrels/ha)", x = "Grid Treatment", subtitle = "A)")+
    theme1)

#growth rate by individual treatments
(gr_treatment <- ggplot(dat)+
    geom_jitter(aes(x = treatment_ind, y = growthrate), shape = 1, color = "grey40", width = .25, alpha = .15)+
    geom_boxplot(aes(x = treatment_ind, y = growthrate), fill = "grey50", alpha = 0.4, outliers = FALSE)+
    labs(x = "Mother Treatment", y = "Litter growth rate (g/day)", subtitle = "B)")+
    theme1)

#litter size by treatments
(ls_treatment <- ggplot(dat)+
    geom_jitter(aes(x = treatment_ind, y = littersize), shape = 1, color = "grey40", width = .25, alpha = .15)+
    geom_violin(aes(x = treatment_ind, y = littersize), fill = "grey50", alpha = 0.4)+
    labs(x = "Mother Treatment", y = "Litter size", subtitle = "C)")+
    theme1)

treatmentplot <- ggarrange(density_treatment, gr_treatment, ls_treatment, nrow = 3, ncol = 1, align = "h")



# Growth rate in response to litter size -------------------------------------------------

#linear model for GR ~ LS from control data set
gr_ls_mod <- lm(growthrate ~ littersize, controls)
gr_ls_slope <- round(gr_ls_mod$coefficients[2], 2) #this is for the results markdown

#get predictions from the linear model
gr_ls_pred <- as.data.table(ggpredict(gr_ls_mod, terms = c("littersize")))

#plot control data and linear model
(ls <- ggplot()+
    geom_point(aes(x = littersize, y = growthrate), shape = 1, color = "grey40", alpha = 0.3, data = controls)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = gr_ls_pred)+
    geom_line(aes(x = x, y = predicted), linewidth = .9, data = gr_ls_pred)+
    labs(x = "Litter size", y = "Growth rate (g/day)")+
    theme1)



# Growth rate in response to birth date -----------------------------------

#linear model for GR ~ Birthdate from control data set
gr_bd_mod <- lm(growthrate ~ bjulian, controls)

#get predictions from the linear model
gr_bd_pred <- as.data.table(ggpredict(gr_bd_mod, terms = c("bjulian")))

#plot control data and linear model
(bd <- ggplot()+
    geom_point(aes(x = bjulian, y = growthrate), shape = 1, color = "grey40", alpha = 0.3,  data = controls)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = gr_bd_pred)+
    geom_line(aes(x = x, y = predicted), linewidth = .9, data = gr_bd_pred)+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)")+
    theme1)



# save --------------------------------------------------------------------

#save tables
write.csv(table_treatgrids, "Output/Tables/Summary of grid-treatments.csv")
write.csv(table_year, "Output/Tables/Summary of experimental years.csv")

#save figs
ggsave("Output/Figures/annual_summary.jpeg", annualplot, width = 11, height = 8, unit = "in")
ggsave("Output/Figures/treatment_summary.jpeg", treatmentplot, width = 4, height = 10)
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 5, height = 5, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", bd, width = 6, height = 4, unit = "in")

