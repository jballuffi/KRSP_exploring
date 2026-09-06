library(data.table)


dat <- readRDS("Output/Data/full_cleaned_litters.rds")


dat[food == 19, treatment_food := "Rattle"]

setnames(dat, "treatment_food", "treatment_ind")

table_treatment <- dat[, .N, by = .(treatment_ind)]

table_treatment_year <- dat[, .N, by = .(treatment_ind, year)]
setorder(table_treatment_year, treatment_ind, year)

write.csv(table_treatment, "Output/Tables/N treatment post new census.csv")
write.csv(table_treatment_year, "Output/Tables/N year post new census.csv")
