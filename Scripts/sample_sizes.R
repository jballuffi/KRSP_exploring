library(data.table)

#read in full little data from script 09 final merge
dat <- readRDS("Output/Data/full_cleaned_litters.rds")


# Summary Tables -----------------------------------------------------------

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
                                                  N_females = length(unique(mother_id)),
                                                  Years = paste0(min(year), "-", max(year))), 
                   by = .(Year = year, Grid = grid, Grid_treatment = treatment_gr)]

setorder(table_year, Year)



# save tables -------------------------------------------------------------

write.csv(table_treatgrids, "Output/Tables/Summary of grid-treatments.csv")
write.csv(table_year, "Output/Tables/Summary of experimental years.csv")
