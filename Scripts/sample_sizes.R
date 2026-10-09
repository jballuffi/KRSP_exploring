library(data.table)

#read in full little data from script 09 final merge
dat <- readRDS("Output/Data/full_cleaned_litters.rds")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")
foods <- readRDS("Output/Data/food_cleaned_litters.rds")

#make tables for sample size
table_treatment <- dat[, .(N = .N, 
                           Avg_density = round(mean(density), 2), 
                           Avg_growthrate = round(mean(growthrate), 2),
                           Avg_littersize = round(mean(littersize), 2)), by = .(treatment_ind)]

table_year <- dat[, .(N = .N, 
                           Avg_density = round(mean(density), 2), 
                           Avg_growthrate = round(mean(growthrate), 2),
                           Avg_littersize = round(mean(littersize), 2)), by = .(treatment_ind, year)]

# Summary Table -----------------------------------------------------------

# I think you should have a table in the supplementary methods showing sample sizes 
# (of litters, # of females) per grid and then indicate when food supplementation 
# occurred on those grids (year start/end) with # of females with and without buckets? 
# You could have that main table and then a longer table on a per year basis

cgrids <- controls[, .(N_litters = length(unique(litter_id)), 
                        N_mothers = length(unique(mother_id)),
                        Years = paste0(min(year), "-", max(year))), by = .(Grid = grid, Treatment = treatment_gr)]

fgrids <- foods[treatment_gr == "Food add", .(N_litters = length(unique(litter_id)), 
                                              N_mothers = length(unique(mother_id)),
                                              Years = paste0(min(year), "-", max(year))), 
                by = .(Grid = grid, Treatment = treatment_gr, Food_status = treatment_ind)]

rgrids <- dat[treatment_gr == "Rattle", .(N_litters = length(unique(litter_id)), 
                                  N_mothers = length(unique(mother_id)),
                                  Years = paste0(min(year), "-", max(year))), 
              by = .(Grid = grid, Treatment = treatment_gr)]


setorder(fgrids, Grid)
setorder(rgrids, Years)

grid_info <- rbind(cgrids, fgrids, rgrids, fill = TRUE)

grid_info <- grid_info[, .(Grid, Treatment, Food_status, N_mothers, N_litters, Years)]




setorder(table_year, treatment_ind, year)

write.csv(grid_info, "Output/Tables/Summary of grids and treatments.csv")
write.csv(table_treatment, "Output/Tables/N treatment post new census.csv")
write.csv(table_year, "Output/Tables/N year post new census.csv")
