
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
                        density = mean(density, na.rm = TRUE),
                        littersize = mean(littersize, na.rm = TRUE)), by = .(year, treatment_gr)]

#merge with years
annualtrends <- merge(years, annualgrowth, by = c("year", "treatment_gr"), all.x = TRUE)



# Local Density in response to treatments  --------------------------------------------------------------------

#plot density by treatment
(density_treatment <-
    ggplot(dat)+
    geom_boxplot(aes(x = treatment_gr, y = density))+
    labs(y = "Local density (squirrels/ha)", x = "Grid Treatment", subtitle = "A)")+
    theme_bw())

#plot avg annual growth rate over time and by food add
(density_year_treatment <- ggplot(annualtrends)+
    geom_line(aes(x = year, y = density, linetype = treatment_gr), alpha = 0.9, linewidth = 1)+
    labs(x = "Year", y = "Local density (squirrels/ha)", linetype = "Grid Treatment", subtitle = "B)")+
    scale_linetype_manual(values = grid_lines)+
    theme_bw()+
    theme(legend.position = "inside",
    legend.position.inside = c(0.87, 0.75))
  )

densityplot <- ggarrange(density_treatment, density_year_treatment, ncol = 1, nrow = 2, align = "h")



# Life History in response to treatments ----------------------------------

#growth rate in response to treatments
(gr_treatment <- ggplot(dat)+
   geom_boxplot(aes(x = treatment_gr, y = growthrate, fill = bucket), alpha = 0.6)+
   labs(x = "Grid Treatment", y = "Growth rate (g/day)", fill = "Bucket", subtitle = "A)")+
   theme_bw())

#litter size in response to treatments
(ls_treatment <- ggplot(dat)+
    geom_violin(aes(x = treatment_gr, y = littersize, fill = bucket), alpha = 0.6)+
    labs(x = "Grid Treatment", y = "Litter size", fill = "Bucket", subtitle = "B)")+
    theme_bw())

responseplot <- ggarrange(gr_treatment, ls_treatment, ncol = 1, nrow = 2)



# Growth rate in response to litter size -------------------------------------------------

#get the relationship between litter size and growth rate from control data set
gr_ls_mod <- lm(growthrate ~ littersize, controls)

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
(birthmonth <- ggplot()+
    geom_point(aes(x = bjulian, y = growthrate), data = controls)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = gr_bd_pred)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = gr_bd_pred)+
    labs(x = "Birthday (Julian day)", y = "Growth rate (g/day)")+
    theme_bw())



# save --------------------------------------------------------------------

#save figs
ggsave("Output/Figures/density_summary.jpeg", densityplot, width = 5, height = 7, unit = "in")
ggsave("Output/Figures/response_summary.jpeg", responseplot, width = 5, height = 8, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")

