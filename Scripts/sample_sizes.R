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

setorder(table_year, treatment_ind, year)



# Summary Table -----------------------------------------------------------

#Ben wants a table showing #litters and #females per grid and what treatment they had and when

# I think you should have a table in the supplementary methods showing sample sizes 
# (of litters, # of females) per grid and then indicate when food supplementation 
# occurred on those grids (year start/end) with # of females with and without buckets? 
# You could have that main table and then a longer table on a per year basis

setorder(dat, grid, year)

treatgrids <- dat[food == 1 | food == 18 | food == 19, 
             .(N_juveniles = sum(littersize),
               N_litters = length(unique(litter_id)),
               N_females = length(unique(mother_id)),
               Years = paste0(min(year), "-", max(year))), by = .(Grid = grid, Treatment = treatment_gr)]

#first take sample size and years on control grids (SU and KL)
cgrids <- controls[, .(N_juveniles = sum(littersize),
                       N_litters = length(unique(litter_id)), 
                       N_females = length(unique(mother_id)),
                       Years = paste0(min(year), "-", max(year))), by = .(Grid = grid, Treatment = treatment_gr)]


grids <- rbind(cgrids, treatgrids)
setorder(grids, Grid, Years)


# #next take sample size and years on food add grids by grid and bucket
# fgrids <- foods[treatment_gr == "Food add", .(N_litters = length(unique(litter_id)), 
#                                               N_females = length(unique(mother_id)),
#                                               Years = paste0(min(year), "-", max(year))), 
#                 by = .(Grid = grid, Treatment = treatment_gr, Food_status = treatment_ind)]
# 
# #take sample size and years on rattle grids
# rgrids <- dat[treatment_gr == "Rattle", .(N_litters = length(unique(litter_id)), 
#                                   N_females = length(unique(mother_id)),
#                                   Years = paste0(min(year), "-", max(year))), 
#               by = .(Grid = grid, Treatment = treatment_gr)]
# 
# #change order of tables 
# setorder(fgrids, Grid)
# setorder(rgrids, Years)
# 
# #bind all three tables together, fill = true because of bucket col that's only in food add
# grid_info <- rbind(cgrids, fgrids, rgrids, fill = TRUE)
# 
# #reorder cols
# grid_info <- grid_info[, .(Grid, Treatment, Food_status, N_females, N_litters, Years)]





# save tables -------------------------------------------------------------

write.csv(grids, "Output/Tables/Summary of grids and treatments.csv")
write.csv(table_treatment, "Output/Tables/N treatment post new census.csv")
write.csv(table_year, "Output/Tables/N year post new census.csv")
