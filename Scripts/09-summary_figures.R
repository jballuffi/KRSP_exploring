
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
years$treatment_gr <- rep(unique(dat$treatment_gr), each = years[, .N]/3)

#get mean + SD for densities, cones, growth rates, and litter size by year and treatment
annualgrowth <- dat[, .(growthrate = mean(growthrate, na.rm = TRUE),
                        growthrate_sd = sd(growthrate, na.rm = TRUE),
                        density = mean(density, na.rm = TRUE),
                        density_sd = sd(density, na.rm = TRUE),
                        littersize = mean(littersize, na.rm = TRUE),
                        littersize_sd = sd(littersize, na.rm = TRUE),
                        cones = mean(cones_prev, na.rm = TRUE),
                        cones_sd = mean(cones_prev, na.rm = TRUE)), by = .(year, treatment_gr)]

#merge with years
annualtrends <- merge(years, annualgrowth, by = c("year", "treatment_gr"), all.x = TRUE)



# Cone production and density over time ------------------------------------------------------

#plot density over time by treatment
(density_year <- ggplot(annualtrends)+
   geom_ribbon(aes(x = year, ymin = density - density_sd, ymax = density + density_sd, fill = treatment_gr), alpha = .3)+
   geom_line(aes(x = year, y = density, color = treatment_gr), alpha = 0.9, linewidth = .9)+
   labs(x = "Year", y = "Local density (squirrels/ha)", subtitle = "A)", fill = "Grid treatment", color = "Grid treatment")+
   scale_fill_manual(values = treatment_gr_cols)+
   scale_color_manual(values = treatment_gr_cols)+
   theme1+
   theme(legend.position = "inside",
         legend.position.inside = c(0.9, 0.78))
)

#plot density over time by treatment
(cones_year <- ggplot(annualtrends)+
   geom_ribbon(aes(x = year+1, ymin = cones - cones_sd, ymax = cones + cones_sd, fill = treatment_gr), alpha = .3)+
   geom_line(aes(x = year+1, y = cones, color = treatment_gr), alpha = 0.9, linewidth = .9)+
   labs(x = "Year", y = "Log cone production (cones/tree)", subtitle = "B)", fill = "Grid treatment", color = "Grid treatment")+
   scale_fill_manual(values = treatment_gr_cols, guide = NULL)+
   scale_color_manual(values = treatment_gr_cols, guide = NULL)+
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
    #scale_fill_manual(values = treatment_ind_cols, guide = NULL)+
    #scale_color_manual(values = treatment_ind_cols, guide = NULL)+
    labs(x = "Mother Treatment", y = "Litter growth rate (g/day)", subtitle = "B)")+
    theme1)

#litter size by treatments
(ls_treatment <- ggplot(dat)+
    geom_jitter(aes(x = treatment_ind, y = littersize), shape = 1, color = "grey40", width = .25, alpha = .15)+
    geom_violin(aes(x = treatment_ind, y = littersize), fill = "grey50", alpha = 0.4)+
    # scale_fill_manual(values = treatment_ind_cols, guide = NULL)+
    # scale_color_manual(values = treatment_ind_cols, guide = NULL)+
    labs(x = "Mother Treatment", y = "Litter size", subtitle = "C)")+
    theme1)

treatmentplot <- ggarrange(density_treatment, gr_treatment, ls_treatment, nrow = 3, ncol = 1, align = "hv")



# Growth rate in response to litter size -------------------------------------------------

#linear model for GR ~ LS from control data set
gr_ls_mod <- lm(growthrate ~ littersize, controls)

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


#save figs
ggsave("Output/Figures/annual_summary.jpeg", annualplot, width = 11, height = 8, unit = "in")
ggsave("Output/Figures/treatment_summary.jpeg", treatmentplot, width = 4, height = 10)
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 5, height = 5, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", bd, width = 6, height = 4, unit = "in")

