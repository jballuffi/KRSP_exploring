library(data.table)

#read in full little data from script 09 final merge
dat <- readRDS("Output/Data/full_cleaned_litters.rds")

#make treatment food option for rattle
dat[food == 19, treatment_food := "Rattle"]
setnames(dat, "treatment_food", "treatment_ind")

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

write.csv(table_treatment, "Output/Tables/N treatment post new census.csv")
write.csv(table_year, "Output/Tables/N year post new census.csv")
