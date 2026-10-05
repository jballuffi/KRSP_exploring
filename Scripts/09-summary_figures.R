
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

#get mean densities, growth rates, and litter size by year and treatment
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



# Response over time ------------------------------------------------------

#plot density over time by treatment
(cones_year <- ggplot(annualtrends)+
   geom_ribbon(aes(x = year+1, ymin = cones - cones_sd, ymax = cones + cones_sd, fill = treatment_gr), alpha = .3)+
   geom_line(aes(x = year+1, y = cones, color = treatment_gr), alpha = 0.9, linewidth = 1)+
   labs(x = "Year", y = "Log cone production (cones/tree)", subtitle = "B)", fill = "Grid treatment", color = "Grid treatment")+
   scale_fill_manual(values = grid_cols)+
   scale_color_manual(values = grid_cols)+
   theme_bw()+
   theme(legend.position = "inside",
         legend.key.size = unit(.3, "cm"),
         legend.position.inside = c(0.82, 0.88))
)

#plot density over time by treatment
(density_year <- ggplot(annualtrends)+
   geom_ribbon(aes(x = year, ymin = density - density_sd, ymax = density + density_sd, fill = treatment_gr), alpha = .3)+
   geom_line(aes(x = year, y = density, color = treatment_gr), alpha = 0.9, linewidth = 1)+
   labs(x = "Year", y = "Local density (squirrels/ha)", subtitle = "B)", fill = "Grid treatment", color = "Grid treatment")+
   scale_fill_manual(values = grid_cols)+
   scale_color_manual(values = grid_cols)+
   theme_bw()+
   theme(legend.position = "inside",
         legend.key.size = unit(.3, "cm"),
         legend.position.inside = c(0.82, 0.88))
)

#annualplot <- ggarrange(density_treatment, density_year_treatment, ncol = 1, nrow = 2, align = "h")





# response to treatments  --------------------------------------------------------------------

#density by grid treatment
(density_treatment <-
    ggplot(dat)+
    geom_jitter(aes(x = treatment_gr, y = density), width = .25, alpha= .2)+
    geom_boxplot(aes(x = treatment_gr, y = density), fill = NA, outliers = FALSE)+
    labs(y = "Local density (squirrels/ha)", x = "Grid Treatment", subtitle = "A)")+
    theme_bw())

#growth rate by individual treatments
(gr_treatment <- ggplot(dat)+
    geom_boxplot(aes(x = treatment_ind, y = growthrate, color = treatment_ind), fill = NA, alpha = 0.6, outliers = FALSE)+
    geom_jitter(aes(x = treatment_ind, y = growthrate, color = treatment_ind), width = .25, alpha = .2)+
    scale_color_manual(values = treatment_cols, guide = NULL)+
    labs(x = "Mother Treatment", y = "Litter growth rate (g/day)", subtitle = "C)")+
    theme_bw())

#litter size by treatments
(ls_treatment <- ggplot(dat)+
    geom_violin(aes(x = treatment_ind, y = littersize, fill = treatment_ind), alpha = 0.4)+
    geom_jitter(aes(x = treatment_ind, y = littersize, color = treatment_ind), width = .25, alpha = .2)+
    scale_fill_manual(values = treatment_cols, guide = NULL)+
    scale_color_manual(values = treatment_cols, guide = NULL)+
    labs(x = "Mother Treatment", y = "Litter size", subtitle = "D)")+
    theme_bw())

#responseplot <- ggarrange(gr_treatment, ls_treatment, ncol = 1, nrow = 2)
full_sum <- ggarrange(density_treatment, density_year_treatment, gr_treatment, ls_treatment, nrow = 2, ncol = 2, align = "hv")



# Growth rate in response to litter size -------------------------------------------------

#get the relationship between litter size and growth rate from control data set
gr_ls_mod <- lm(growthrate ~ littersize, controls)

gr_ls_slope <- round(coef(gr_ls_mod)[2], 1)

#get effects table
gr_ls_pred <- as.data.table(ggpredict(gr_ls_mod, terms = c("littersize")))

(ls <- ggplot()+
    geom_point(aes(x = littersize, y = growthrate), alpha = 0.5, data = controls)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = gr_ls_pred)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = gr_ls_pred)+
    labs(x = "Litter size", y = "Growth rate (g/day)")+
    theme_bw())



# Growth rate in response to birth date -----------------------------------

#get the relationship between litter size and growth rate from control data set
gr_bd_mod <- lm(growthrate ~ bjulian, controls)

#get effects table
gr_bd_pred <- as.data.table(ggpredict(gr_bd_mod, terms = c("bjulian")))


#growth rate in response to birth date
(bd <- ggplot()+
    geom_point(aes(x = bjulian, y = growthrate), data = controls)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = gr_bd_pred)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = gr_bd_pred)+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)")+
    theme_bw())



# save --------------------------------------------------------------------


#save figs
#ggsave("Output/Figures/density_summary.jpeg", densityplot, width = 5, height = 7, unit = "in")
#ggsave("Output/Figures/response_summary.jpeg", responseplot, width = 5, height = 8, unit = "in")


ggsave("Output/Figures/full_summary.jpeg", full_sum, width = 11, height = 9, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 5, height = 5, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", bd, width = 6, height = 4, unit = "in")

