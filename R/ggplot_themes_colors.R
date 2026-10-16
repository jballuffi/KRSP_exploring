
#ggplot colors
library(ggplot2)

#colors for grid level treatment
treatment_gr_cols <- c("Control" = "grey40", "Food add" = "firebrick1", "Rattle" = "orchid1")

#colors for individual level treatment
treatment_ind_cols <- c("Control" = "grey40", "Bucket" = "firebrick1", "Rattle" = "orchid1", "Chickadee" = "navyblue", "No Bucket" = "dodgerblue2") 

#colors for buckets
bucket_cols <- c("1" = "firebrick1", "0" = "grey60")

#colors for cache levels
cache_cols <- c("small" = "lightgoldenrod1", "medium" = "olivedrab4", "large" = "darkgreen")

#density colors for ellipse
density_cols <- c("low" = "lightskyblue", "medium" = "royalblue3", "high" = "navyblue")

#point size for mast years
mast_sizes <- c("yes" = 4, "no" = .1)


#theme option 1
theme1 <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size = 13, family = "serif"),
                     axis.text = element_text(size = 11, family = "serif"),
                     legend.key = element_blank(),
                     legend.text = element_text(size = 11),
                     legend.title = element_text(size = 12),
                     strip.background = element_blank(),
                     panel.background = element_blank(),
                     panel.border = element_rect(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))
